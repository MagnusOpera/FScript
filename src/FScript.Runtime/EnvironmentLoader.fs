namespace FScript.Runtime

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open FScript.Language

/// The registration and concrete assets used by a successfully initialized environment.
type LoadedEnvironment =
    { Registration: ExtensionRegistration
      AssemblyPaths: string list
      Packages: ResolvedExtensionPackage list }

module EnvironmentLoader =
    let private loadCore
        (settings: EnvironmentSettings)
        : Result<LoadedEnvironment, string> =
        let context : HostContext = { RootDirectory = settings.RootDirectory; DeniedPathGlobs = [] }
        let defaultSources =
            if not settings.DefaultExterns then
                []
            else
                [ "runtime default externs", HostInterop.ofExterns (Registry.all context) ]

        let packageEnvironment = NuGetEnvironment.restore settings
        let assemblyInputs = settings.ExternAssemblies
        let hasInjectedAssemblies = not assemblyInputs.IsEmpty || not settings.Packages.IsEmpty

        if hasInjectedAssemblies && not RuntimeFeature.IsDynamicCodeSupported then
            Error "External assembly injection is not supported by this runtime (likely AOT/native)."
        else
            let normalizeAssemblyPath (path: string) =
                if Path.IsPathRooted(path) then
                    Path.GetFullPath(path)
                else
                    Path.GetFullPath(Path.Combine(settings.BaseDirectory, path))

            let uniqueAssemblyPaths =
                let seen = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
                assemblyInputs
                |> List.map normalizeAssemblyPath
                |> List.filter seen.Add

            let externListType = typeof<FScript.Extensibility.ExternalFunction list>
            let providerContext = FScript.Extensibility.HostContext(context.RootDirectory, context.DeniedPathGlobs, ExtensionBridge.toExtensionFunctions (Registry.all context))

            let tryGetTypes (assembly: Assembly) : Result<System.Type list, string> =
                try
                    Ok (assembly.GetTypes() |> Array.toList)
                with
                | :? ReflectionTypeLoadException as ex ->
                    ex.Types
                    |> Array.choose (fun t ->
                        match t with
                        | null -> None
                        | value -> Some value)
                    |> Array.toList
                    |> Ok
                | ex ->
                    Error ex.Message

            let packageContext = lazy (new ExtensionLoadContext(packageEnvironment.AssemblyPaths))

            let tryLoadProviderSources allowEmpty (assemblyPath: string) : Result<(string * ExtensionRegistration) list, string> =
                if not (File.Exists assemblyPath) then
                    Error $"Extern assembly not found: {assemblyPath}"
                else
                    try
                        let assembly = if allowEmpty then packageContext.Value.LoadFromAssemblyPath(assemblyPath) else Assembly.LoadFrom(assemblyPath)
                        match tryGetTypes assembly with
                        | Error message ->
                            Error $"Failed to inspect assembly '{assemblyPath}': {message}"
                        | Ok types ->
                            let providerMethods : (System.Type * MethodInfo) list =
                                types
                                |> List.collect (fun t ->
                                    t.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                                    |> Array.toList
                                    |> List.filter (fun m -> m.GetCustomAttributes(typeof<FScript.Extensibility.FScriptExternProviderAttribute>, false).Length > 0)
                                    |> List.map (fun m -> t, m))

                            if providerMethods.IsEmpty && not allowEmpty then
                                Error $"No [<FScriptExternProvider>] methods found in assembly '{assemblyPath}'."
                            else
                                let rec collectProviders
                                    (remaining: (System.Type * MethodInfo) list)
                                    (acc: (string * ExtensionRegistration) list)
                                    : Result<(string * ExtensionRegistration) list, string> =
                                    match remaining with
                                    | [] -> Ok (List.rev acc)
                                    | (declaringType, providerMethod) :: tail ->
                                        if providerMethod.ReturnType <> externListType && providerMethod.ReturnType <> typeof<FScript.Extensibility.ExtensionRegistration> then
                                            Error $"Invalid extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}': return type must be ExternalFunction list or ExtensionRegistration."
                                        else
                                            let parameters = providerMethod.GetParameters()
                                            let invokeArgsResult : Result<objnull array, string> =
                                                if parameters.Length = 0 then
                                                    Ok [||]
                                                elif parameters.Length = 1 && parameters.[0].ParameterType = typeof<FScript.Extensibility.HostContext> then
                                                    Ok [| box providerContext |]
                                                else
                                                    Error $"Invalid extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}': expected signature unit -> ExternalFunction list or HostContext -> ExternalFunction list."

                                            match invokeArgsResult with
                                            | Error message -> Error message
                                            | Ok invokeArgs ->
                                                try
                                                    let rawProvided = providerMethod.Invoke(null, invokeArgs)
                                                    if isNull rawProvided then
                                                        Error $"Extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}' returned null."
                                                    else
                                                        match rawProvided with
                                                        | :? (FScript.Extensibility.ExternalFunction list) as provided ->
                                                            let sourceName = $"{Path.GetFileName(assemblyPath)}:{declaringType.FullName}.{providerMethod.Name}"
                                                            collectProviders tail ((sourceName, FScript.Extensibility.HostInterop.ofExterns provided |> ExtensionBridge.toLanguageRegistration) :: acc)
                                                        | :? FScript.Extensibility.ExtensionRegistration as provided ->
                                                            let sourceName = $"{Path.GetFileName(assemblyPath)}:{declaringType.FullName}.{providerMethod.Name}"
                                                            collectProviders tail ((sourceName, ExtensionBridge.toLanguageRegistration provided) :: acc)
                                                        | _ ->
                                                            Error $"Extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}' returned an invalid value."
                                                with
                                                | ex ->
                                                    Error $"Extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}' failed: {ex.GetBaseException().Message}"

                                collectProviders providerMethods []
                    with
                    | ex ->
                        Error $"Failed to load extern assembly '{assemblyPath}': {ex.GetBaseException().Message}"

            let loadedUserSources : Result<(string * ExtensionRegistration) list, string> =
                uniqueAssemblyPaths
                |> List.fold (fun state assemblyPath ->
                    match state with
                    | Error _ as err -> err
                    | Ok acc ->
                        match tryLoadProviderSources false assemblyPath with
                        | Ok providers -> Ok (acc @ providers)
                        | Error _ as err -> err) (Ok ([] : (string * ExtensionRegistration) list))

            // Discover each marked package assembly once, including marked dependencies.
            // A plotting provider can therefore reuse the numerical provider's type registration.
            let packageProviders = Collections.Generic.Dictionary<string, (string * ExtensionRegistration) list>(StringComparer.OrdinalIgnoreCase)
            let scanPackageAssembly path =
                match packageProviders.TryGetValue path with
                | true, providers -> Ok (providers, false)
                | _ ->
                    tryLoadProviderSources true path |> Result.map (fun providers ->
                        packageProviders.[path] <- providers
                        providers, true)
            let loadedRootSources =
                packageEnvironment.Packages |> List.fold (fun state package ->
                    state |> Result.bind (fun accumulated ->
                        let providers = package.Assemblies |> List.fold (fun state path ->
                            state |> Result.bind (fun (found, fresh) ->
                                scanPackageAssembly path |> Result.map (fun (sources, isFresh) ->
                                    found @ sources, fresh @ (if isFresh then sources else [])))) (Ok ([], []))
                        providers |> Result.bind (fun (found, fresh) ->
                            if found.IsEmpty then Error $"NuGet package '{package.Name}' {package.Version} contains no [<FScriptExternProvider>] methods in compatible runtime assemblies."
                            else Ok (accumulated @ fresh)))) loadedUserSources
            let loadedPackageSources =
                packageEnvironment.AssemblyPaths |> List.fold (fun state path ->
                    state |> Result.bind (fun accumulated ->
                        let name = Path.GetFileNameWithoutExtension path
                        if name = "FScript.Extensibility" || name = "FSharp.Core" then Ok accumulated
                        else scanPackageAssembly path |> Result.map (fun (found, fresh) -> accumulated @ (if fresh then found else [])))) loadedRootSources

            loadedPackageSources
            |> Result.bind (fun userSources ->
                let allSources = defaultSources @ userSources
                let conflicts =
                    allSources
                    |> List.collect (fun (sourceName, externs) ->
                        externs.Functions |> List.map (fun ext -> ext.Name, sourceName))
                    |> List.groupBy fst
                    |> List.choose (fun (name, entries) ->
                        if entries.Length > 1 then
                            let details =
                                entries
                                |> List.map snd
                                |> List.distinct
                                |> String.concat ", "
                            Some $"{name} ({details})"
                        else
                            None)

                if conflicts.IsEmpty then
                    let registrations = allSources |> List.map snd
                    let registration =
                        { Types = registrations |> List.collect (fun r -> r.Types)
                          Unions = registrations |> List.collect (fun r -> r.Unions)
                          Functions = registrations |> List.collect (fun r -> r.Functions) }
                    try
                        HostInterop.typeDefinitions registration [] |> ignore
                        Ok { Registration = registration
                             AssemblyPaths = (uniqueAssemblyPaths @ packageEnvironment.AssemblyPaths) |> List.distinct
                             Packages = packageEnvironment.Packages }
                    with EvalException error -> Error error.Message
                else
                    let conflictSummary = String.concat "; " conflicts
                    Error $"External function name conflicts detected: {conflictSummary}")


    /// Initialize providers atomically; callers never receive a partial registration.
    let loadWithInfo (settings: EnvironmentSettings) =
        try
            if not settings.Packages.IsEmpty && not RuntimeFeature.IsDynamicCodeSupported then
                Error "NuGet extensions are not supported by this runtime (likely AOT/native)."
            else loadCore settings
        with ex -> Error $"Failed to initialize external-function environment: {ex.GetBaseException().Message}"

    /// Load a registration from local DLLs and configured NuGet packages.
    let load settings = loadWithInfo settings |> Result.map (fun loaded -> loaded.Registration)
