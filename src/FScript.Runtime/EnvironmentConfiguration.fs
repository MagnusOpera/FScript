namespace FScript.Runtime

open System
open System.IO
open Tomlyn
open Tomlyn.Model

/// A NuGet extension package; omitted versions resolve to the latest stable release.
type ExtensionPackage = { Name: string; Version: string option }

/// Compile-only defaults; parsing these never runs a compiler or loads providers.
type CompileConfiguration =
    { Backend: string option
      AheadOfTime: bool option
      MinimizeSize: bool option
      SingleFile: bool option
      RuntimeIdentifier: string option
      OutputDirectory: string option }

module CompileConfiguration =
    let empty =
        { Backend=None; AheadOfTime=None; MinimizeSize=None; SingleFile=None
          RuntimeIdentifier=None; OutputDirectory=None }

/// Parsed configuration; omitted properties remain distinct from defaults.
type EnvironmentConfiguration =
    { Path: string
      Root: string option
      DefaultExterns: bool option
      ExternAssemblies: string list option
      Packages: ExtensionPackage list option
      Compilation: CompileConfiguration }

/// Effective environment, containing absolute paths and its configuration provenance.
type EnvironmentSettings =
    { ConfigurationPath: string option
      BaseDirectory: string
      RootDirectory: string
      DefaultExterns: bool
      ExternAssemblies: string list
      Packages: ExtensionPackage list
      Compilation: CompileConfiguration }

module EnvironmentConfiguration =
    let normalizePath (baseDirectory: string) (path: string) =
        Path.GetFullPath(if Path.IsPathRooted path then path else Path.Combine(baseDirectory, path))

    let parse (path: string) (text: string) : Result<EnvironmentConfiguration, string> =
        try
            let metadata = Tomlyn.Serialization.TomlMetadataStore()
            let options = TomlSerializerOptions(SourceName = path, MetadataStore = metadata)
            let document = TomlSerializer.Deserialize<TomlTable>(text, options) |> nonNull
            let invalid (table: TomlTable) key property message =
                let location =
                    match metadata.TryGetProperties(table) with
                    | true, properties when not (isNull properties) ->
                        match (nonNull properties).TryGetProperty(key) with
                        | true, entry when not (isNull entry) -> (nonNull entry).Span.ToString()
                        | _ -> path
                    | _ -> path
                invalidArg property $"{location}: {property}: {message}"
            for key in document.Keys do
                if key <> "runtime" && key <> "compile" then invalid document key key "Unknown configuration section or property."
            let runtime =
                match document.TryGetValue "runtime" with
                | false, _ -> TomlTable()
                | true, (:? TomlTable as table) -> table
                | _ -> invalid document "runtime" "runtime" "Expected a table."
            for key in runtime.Keys do
                if not (["root"; "default-externs"; "extern-assemblies"; "packages"] |> List.contains key) then
                    invalid runtime key $"runtime.{key}" "Unknown configuration property."
            let root =
                match runtime.TryGetValue "root" with
                | false, _ -> None
                | true, (:? string as value) -> Some value
                | _ -> invalid runtime "root" "runtime.root" "Expected a string."
            let defaults =
                match runtime.TryGetValue "default-externs" with
                | false, _ -> None
                | true, (:? bool as value) -> Some value
                | _ -> invalid runtime "default-externs" "runtime.default-externs" "Expected a boolean."
            let assemblies =
                match runtime.TryGetValue "extern-assemblies" with
                | false, _ -> None
                | true, (:? TomlArray as values) ->
                    values |> Seq.map (function
                        | :? string as value -> value
                        | _ -> invalid runtime "extern-assemblies" "runtime.extern-assemblies" "Expected an array of strings.") |> Seq.toList |> Some
                | _ -> invalid runtime "extern-assemblies" "runtime.extern-assemblies" "Expected an array of strings."
            let packages =
                match runtime.TryGetValue "packages" with
                | false, _ -> None
                | true, (:? TomlTableArray as entries) ->
                    let seen = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
                    entries |> Seq.map (fun entry ->
                        for key in entry.Keys do
                            if key <> "name" && key <> "version" then invalid entry key $"runtime.packages.{key}" "Unknown package property."
                        let name =
                            match entry.TryGetValue "name" with
                            | true, (:? string as value) when NuGet.Packaging.PackageIdValidator.IsValidPackageId value -> value
                            | _ -> invalid entry "name" "runtime.packages.name" "A valid NuGet package name is required."
                        if not (seen.Add name) then invalid entry "name" "runtime.packages.name" $"Duplicate package '{name}'."
                        let version =
                            match entry.TryGetValue "version" with
                            | false, _ -> None
                            | true, (:? string as value) ->
                                let mutable parsedVersion: NuGet.Versioning.NuGetVersion | null = null
                                if NuGet.Versioning.NuGetVersion.TryParse(value, &parsedVersion) && value.Trim() = value then Some value
                                else invalid entry "version" "runtime.packages.version" "Expected an exact NuGet version; omit it for latest stable."
                            | _ -> invalid entry "version" "runtime.packages.version" "Expected a version string."
                        { Name = name; Version = version }) |> Seq.toList |> Some
                | _ -> invalid runtime "packages" "runtime.packages" "Expected an array of package tables ([[runtime.packages]])."
            let compile =
                match document.TryGetValue "compile" with
                | false, _ -> TomlTable()
                | true, (:? TomlTable as table) -> table
                | _ -> invalid document "compile" "compile" "Expected a table."
            for key in compile.Keys do
                if not (List.contains key ["backend"; "aot"; "size-optimized"; "single-file"; "runtime"; "output"]) then
                    invalid compile key $"compile.{key}" "Unknown configuration property."
            let textOption key =
                match compile.TryGetValue key with
                | false, _ -> None
                | true, (:? string as value) when not (String.IsNullOrWhiteSpace value) -> Some value
                | _ -> invalid compile key $"compile.{key}" "Expected a nonempty string."
            let boolOption key =
                match compile.TryGetValue key with
                | false, _ -> None
                | true, (:? bool as value) -> Some value
                | _ -> invalid compile key $"compile.{key}" "Expected a boolean."
            let backend = textOption "backend" |> Option.map (fun value -> value.ToLowerInvariant())
            match backend with
            | Some value when not (List.contains value ["ir"; "csharp"; "dotnet"]) ->
                invalid compile "backend" "compile.backend" "Expected ir, csharp, or dotnet."
            | _ -> ()
            let fullPath = Path.GetFullPath path
            let directory = Path.GetDirectoryName fullPath
            Ok { Path = fullPath; Root = root |> Option.map (normalizePath directory)
                 DefaultExterns = defaults; Packages = packages
                 Compilation =
                    { Backend=backend; AheadOfTime=boolOption "aot"; MinimizeSize=boolOption "size-optimized"
                      SingleFile=boolOption "single-file"; RuntimeIdentifier=textOption "runtime"
                      OutputDirectory=textOption "output" |> Option.map (normalizePath directory) }
                 ExternAssemblies = assemblies |> Option.map (List.map (normalizePath directory)) }
        with ex -> Error $"Unable to read environment configuration '{path}': {ex.Message}"

    let read path =
        try parse path (File.ReadAllText path)
        with ex -> Error $"Unable to read environment configuration '{path}': {ex.Message}"

    let merge baseDirectory defaultRoot (configuration: EnvironmentConfiguration option) rootOverride disableDefaults assemblyArguments =
        let seen = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
        { ConfigurationPath = configuration |> Option.map (fun c -> c.Path)
          BaseDirectory = Path.GetFullPath baseDirectory
          RootDirectory =
            rootOverride |> Option.map (normalizePath baseDirectory)
            |> Option.orElse (configuration |> Option.bind (fun c -> c.Root))
            |> Option.defaultValue defaultRoot |> Path.GetFullPath
          DefaultExterns = not disableDefaults && (configuration |> Option.bind (fun c -> c.DefaultExterns) |> Option.defaultValue true)
          Packages = configuration |> Option.bind (fun c -> c.Packages) |> Option.defaultValue []
          Compilation = configuration |> Option.map (fun c -> c.Compilation) |> Option.defaultValue CompileConfiguration.empty
          ExternAssemblies =
            (configuration |> Option.bind (fun c -> c.ExternAssemblies) |> Option.defaultValue [])
            @ (assemblyArguments |> List.map (normalizePath baseDirectory))
            |> List.filter seen.Add }

    let resolve configurationPath baseDirectory defaultRoot rootOverride disableDefaults assemblyArguments =
        try
            let parsed =
                match configurationPath with
                | None -> Ok None
                | Some path -> read (normalizePath baseDirectory path) |> Result.map Some
            parsed |> Result.map (fun c -> merge baseDirectory defaultRoot c rootOverride disableDefaults assemblyArguments)
        with ex -> Error $"Invalid environment settings: {ex.Message}"

    /// CLI-only discovery: file mode supplies the script directory; stdin/REPL supply cwd.
    /// Explicit resolver APIs for embedders and editor environments remain discovery-free.
    let resolveForCli configurationPath noConfig baseDirectory defaultRoot rootOverride disableDefaults assemblyArguments =
        try
            if noConfig && Option.isSome configurationPath then
                Error "--config and --no-config cannot be used together."
            else
                let selected =
                    configurationPath |> Option.orElseWith (fun () ->
                        let candidate = Path.Combine(defaultRoot, "fscript.toml")
                        if not noConfig && File.Exists candidate then Some candidate else None)
                resolve selected baseDirectory defaultRoot rootOverride disableDefaults assemblyArguments
        with ex -> Error $"Invalid environment settings: {ex.Message}"
