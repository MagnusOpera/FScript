namespace FScript.Runtime

open System
open System.IO
open System.Collections.Generic
open System.Security.Cryptography
open System.Text
open System.Runtime.InteropServices
open System.Threading
open NuGet.Commands
open NuGet.Common
open NuGet.Configuration
open NuGet.Frameworks
open NuGet.LibraryModel
open NuGet.ProjectModel
open NuGet.Protocol
open NuGet.Protocol.Core.Types
open NuGet.RuntimeModel
open NuGet.Versioning

/// The concrete packages selected for one host process.
type ResolvedExtensionPackage = { Name: string; Version: string; Assemblies: string list }
type internal NuGetEnvironment = { Packages: ResolvedExtensionPackage list; AssemblyPaths: string list }

/// In-process NuGet restore: no dotnet executable, MSBuild evaluation, or package build targets.
module internal NuGetEnvironment =
    let private hash (value: string) =
        use sha = SHA256.Create()
        sha.ComputeHash(Encoding.UTF8.GetBytes value) |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""

    let private runtimeIdentifier () =
        let os = if RuntimeInformation.IsOSPlatform OSPlatform.Windows then "win" elif RuntimeInformation.IsOSPlatform OSPlatform.OSX then "osx" else "linux"
        let architecture = RuntimeInformation.ProcessArchitecture.ToString().ToLowerInvariant()
        // RuntimeIdentifier is absent from netstandard's reference surface.
        let property = typeof<RuntimeInformation>.GetProperty("RuntimeIdentifier")
        if isNull property then $"{os}-{architecture}" else
            match (nonNull property).GetValue(null) with
            | :? string as value -> value
            | _ -> $"{os}-{architecture}"

    let private acquireLock path =
        let deadline = DateTime.UtcNow.AddMinutes 2.
        let rec attempt () =
            try new FileStream(path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)
            with :? IOException when DateTime.UtcNow < deadline -> Thread.Sleep 100; attempt ()
        attempt ()

    let restore (settings: EnvironmentSettings) =
        if settings.Packages.IsEmpty then { Packages = []; AssemblyPaths = [] } else
        let directory = settings.ConfigurationPath |> Option.map Path.GetDirectoryName |> Option.defaultValue settings.BaseDirectory
        let nugetSettings = Settings.LoadDefaultSettings directory
        let sources = PackageSourceProvider(nugetSettings).LoadPackageSources() |> Seq.filter (fun source -> source.IsEnabled) |> Seq.toList
        if sources.IsEmpty then invalidOp "No enabled NuGet package sources. Configure a source in NuGet.Config."
        let framework = NuGetFramework.ParseFolder($"net{Environment.Version.Major}.0")
        let rid = runtimeIdentifier ()
        let inputs = settings.Packages |> List.map (fun p -> p.Name.ToLowerInvariant() + "/" + Option.defaultValue "*" p.Version) |> String.concat "\n"
        let key = hash ($"{directory}\n{framework}\n{rid}\n{inputs}")
        let cacheRoot = Path.Combine(directory, ".fscript")
        let work = Path.Combine(cacheRoot, "restore", key)
        Directory.CreateDirectory work |> ignore
        use gate = acquireLock (Path.Combine(work, "restore.lock"))
        let project = Path.Combine(work, "environment.csproj")
        let packagesPath = Path.Combine(cacheRoot, "packages")
        let metadata = ProjectRestoreMetadata(ProjectStyle = ProjectStyle.PackageReference, ProjectPath = project,
                            ProjectUniqueName = project, ProjectName = "FScript.Environment", OutputPath = work, PackagesPath = packagesPath)
        metadata.ProjectWideWarningProperties <- WarningProperties(HashSet([NuGetLogCode.NU1605; NuGetLogCode.NU1608]), HashSet(), false, HashSet())
        metadata.Sources <- ResizeArray(sources)
        metadata.OriginalTargetFrameworks.Add(framework.GetShortFolderName())
        metadata.TargetFrameworks.Add(ProjectRestoreMetadataFrameworkInfo(framework, TargetAlias = framework.GetShortFolderName()))
        let target = TargetFrameworkInformation(FrameworkName = framework, TargetAlias = framework.GetShortFolderName())
        let spec = PackageSpec(ResizeArray [target], Name = "FScript.Environment", FilePath = project, RestoreMetadata = metadata)
        use graphStream = typeof<EnvironmentSettings>.Assembly.GetManifestResourceStream("FScript.Runtime.PortableRuntimeIdentifierGraph.json") |> nonNull
        use reader = new StreamReader(graphStream)
        spec.RuntimeGraph <- JsonRuntimeFormat.ReadRuntimeGraph reader
        for package in settings.Packages do
            let range = package.Version |> Option.map (fun v -> VersionRange.Parse($"[{v}]")) |> Option.defaultWith (fun () -> VersionRange.Parse "*")
            spec.Dependencies.Add(LibraryDependency(LibraryRange = LibraryRange(package.Name, range, LibraryDependencyTarget.Package)))
        let graph = DependencyGraphSpec()
        graph.AddProject spec
        graph.AddRestore project
        use httpCache = new SourceCacheContext()
        if settings.Packages |> List.exists (fun p -> p.Version.IsNone) then httpCache.MaxAge <- Nullable DateTimeOffset.UtcNow
        let args = RestoreArgs(CacheContext = httpCache, Log = NullLogger.Instance, AllowNoOp = false,
                               RestoreForceEvaluate = true, GlobalPackagesFolder = packagesPath)
        args.Runtimes.Add rid |> ignore
        args.CachingSourceProvider <- CachingSourceProvider(PackageSourceProvider(nugetSettings))
        args.PreLoadedRequestProviders.Add(DependencyGraphSpecRequestProvider(RestoreCommandProvidersCache(), graph, nugetSettings))
        let summaries = RestoreRunner.RunAsync(args, CancellationToken.None).GetAwaiter().GetResult()
        let failures = summaries |> Seq.filter (fun summary -> not summary.Success) |> Seq.toList
        if not failures.IsEmpty then
            let errors = failures |> List.collect (fun summary -> summary.Errors |> Seq.map (fun error -> error.Message) |> Seq.toList) |> String.concat "\n"
            invalidOp $"NuGet package restore failed: {errors}"
        let lockFile = LockFileFormat().Read(Path.Combine(work, "project.assets.json"))
        let selected = lockFile.Targets |> Seq.find (fun target -> target.RuntimeIdentifier = rid && target.TargetFramework.Equals framework)
        // Stage the selected runtime assets together so Assembly.LoadFrom can resolve dependencies.
        // A new graph gets a new directory, never overwriting a loaded environment.
        let graphKey = selected.Libraries |> Seq.map (fun lib -> $"{lib.Name}/{lib.Version}") |> String.concat "\n" |> fun graph -> hash ($"{key}\n{graph}")
        let stage = Path.Combine(cacheRoot, "environments", graphKey)
        Directory.CreateDirectory stage |> ignore
        let staged = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let assemblyPaths = ResizeArray<string>()
        let roots = Dictionary<string, ResizeArray<string>>(StringComparer.OrdinalIgnoreCase)
        for package in settings.Packages do roots.Add(package.Name, ResizeArray())
        let copy (source: string) (destination: string) =
            Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
            if not (File.Exists destination) then
                let temporary = destination + "." + Guid.NewGuid().ToString("N") + ".tmp"
                try
                    File.Copy(source, temporary)
                    File.Move(temporary, destination)
                finally
                    if File.Exists temporary then File.Delete temporary
        for library in selected.Libraries do
            let installed = lockFile.Libraries |> Seq.find (fun lib -> lib.Name.Equals(library.Name, StringComparison.OrdinalIgnoreCase) && lib.Version.Equals library.Version)
            let packageDirectory = lockFile.PackageFolders |> Seq.map (fun folder -> Path.Combine(folder.Path, installed.Path)) |> Seq.find Directory.Exists
            for asset in Seq.concat [library.RuntimeAssemblies; library.NativeLibraries; library.ResourceAssemblies] do
                if not (asset.Path.EndsWith("/_._", StringComparison.Ordinal)) then
                    let source = Path.Combine(packageDirectory, asset.Path)
                    let relative =
                        if library.ResourceAssemblies.Contains asset then Path.Combine(Path.GetFileName(Path.GetDirectoryName asset.Path), Path.GetFileName asset.Path)
                        else Path.GetFileName asset.Path
                    match staged.TryGetValue relative with
                    | true, other when File.ReadAllBytes(other) <> File.ReadAllBytes(source) -> invalidOp $"NuGet runtime asset collision: {relative}."
                    | _ -> staged.[relative] <- source
                    let destination = Path.Combine(stage, relative)
                    copy source destination
                    // Also retain RID-native paths for providers that locate assets explicitly.
                    if library.NativeLibraries.Contains asset then copy source (Path.Combine(stage, asset.Path))
                    if library.RuntimeAssemblies.Contains asset then
                        assemblyPaths.Add destination
                        match roots.TryGetValue library.Name with true, paths -> paths.Add destination | _ -> ()
        let resolved =
            settings.Packages |> List.map (fun (package: ExtensionPackage) ->
                let library = selected.Libraries |> Seq.find (fun lib -> String.Equals(lib.Name, package.Name, StringComparison.OrdinalIgnoreCase))
                { Name = nonNull library.Name; Version = (nonNull library.Version).ToNormalizedString(); Assemblies = roots.[package.Name] |> Seq.toList })
        { Packages = resolved; AssemblyPaths = assemblyPaths |> Seq.distinct |> Seq.toList }
