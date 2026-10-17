namespace FScript.Runtime

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open FScript.Language

module EnvironmentLoader =
    let private loadCore
        (settings: EnvironmentSettings)
        : Result<ExtensionRegistration, string> =
        let context : HostContext = { RootDirectory = settings.RootDirectory; DeniedPathGlobs = [] }
        let defaultSources =
            if not settings.DefaultExterns then
                []
            else
                [ "runtime default externs", HostInterop.ofExterns (Registry.all context) ]

        let assemblyInputs = settings.ExternAssemblies

        if not assemblyInputs.IsEmpty && not RuntimeFeature.IsDynamicCodeSupported then
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

            let externListType = typeof<ExternalFunction list>

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

            let tryLoadProviderSources (assemblyPath: string) : Result<(string * ExtensionRegistration) list, string> =
                if not (File.Exists assemblyPath) then
                    Error $"Extern assembly not found: {assemblyPath}"
                else
                    try
                        let assembly = Assembly.LoadFrom(assemblyPath)
                        match tryGetTypes assembly with
                        | Error message ->
                            Error $"Failed to inspect assembly '{assemblyPath}': {message}"
                        | Ok types ->
                            let providerMethods : (System.Type * MethodInfo) list =
                                types
                                |> List.collect (fun t ->
                                    t.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                                    |> Array.toList
                                    |> List.filter (fun m -> m.GetCustomAttributes(typeof<FScriptExternProviderAttribute>, false).Length > 0)
                                    |> List.map (fun m -> t, m))

                            if providerMethods.IsEmpty then
                                Error $"No [<FScriptExternProvider>] methods found in assembly '{assemblyPath}'."
                            else
                                let rec collectProviders
                                    (remaining: (System.Type * MethodInfo) list)
                                    (acc: (string * ExtensionRegistration) list)
                                    : Result<(string * ExtensionRegistration) list, string> =
                                    match remaining with
                                    | [] -> Ok (List.rev acc)
                                    | (declaringType, providerMethod) :: tail ->
                                        if providerMethod.ReturnType <> externListType && providerMethod.ReturnType <> typeof<ExtensionRegistration> then
                                            Error $"Invalid extern provider '{declaringType.FullName}.{providerMethod.Name}' in '{assemblyPath}': return type must be ExternalFunction list or ExtensionRegistration."
                                        else
                                            let parameters = providerMethod.GetParameters()
                                            let invokeArgsResult : Result<objnull array, string> =
                                                if parameters.Length = 0 then
                                                    Ok [||]
                                                elif parameters.Length = 1 && parameters.[0].ParameterType = typeof<HostContext> then
                                                    Ok [| box context |]
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
                                                        | :? (ExternalFunction list) as provided ->
                                                            let sourceName = $"{Path.GetFileName(assemblyPath)}:{declaringType.FullName}.{providerMethod.Name}"
                                                            collectProviders tail ((sourceName, HostInterop.ofExterns provided) :: acc)
                                                        | :? ExtensionRegistration as provided ->
                                                            let sourceName = $"{Path.GetFileName(assemblyPath)}:{declaringType.FullName}.{providerMethod.Name}"
                                                            collectProviders tail ((sourceName, provided) :: acc)
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
                        match tryLoadProviderSources assemblyPath with
                        | Ok providers -> Ok (acc @ providers)
                        | Error _ as err -> err) (Ok ([] : (string * ExtensionRegistration) list))

            loadedUserSources
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
                          Functions = registrations |> List.collect (fun r -> r.Functions) }
                    try
                        HostInterop.typeDefinitions registration [] |> ignore
                        Ok registration
                    with EvalException error -> Error error.Message
                else
                    let conflictSummary = String.concat "; " conflicts
                    Error $"External function name conflicts detected: {conflictSummary}")


    /// Initialize providers atomically; callers never receive a partial registration.
    let load settings =
        try loadCore settings
        with ex -> Error $"Failed to initialize external-function environment: {ex.GetBaseException().Message}"
