namespace FScript.LanguageServer

open System
open System.IO
open System.Text.Json.Nodes
open FScript.Language
open FScript.Runtime
open FScript.CSharpInterop

/// One process owns one workspace environment. Source-only clients keep the historical defaults.
module LspRuntimeExterns =
    let mutable private registration: ExtensionRegistration option = None
    let mutable private configurationPath: string option = None
    let mutable private settings: EnvironmentSettings option = None
    let mutable private error: string option = None
    let mutable private loadedEnvironment: LoadedEnvironment option = None
    let mutable private blocked = false

    let initialize (options: JsonObject option) =
        registration <- None
        configurationPath <- None
        settings <- None
        error <- None
        blocked <- false
        loadedEnvironment <- None
        let stringValue (o: JsonObject) (key: string) =
            match o[key] with
            | :? JsonValue as v -> try Some(v.GetValue<string>()) with _ -> None
            | _ -> None
        match options with
        | Some o ->
            configurationPath <- stringValue o "configurationPath" |> Option.filter (String.IsNullOrWhiteSpace >> not)
            match configurationPath with
            | Some path ->
                let folder =
                    stringValue o "workspaceFolderUri"
                    |> Option.map (fun uri -> Uri(uri).LocalPath)
                    |> Option.defaultValue (Directory.GetCurrentDirectory())
                let trusted =
                    match o["workspaceTrusted"] with
                    | :? JsonValue as v -> try v.GetValue<bool>() with _ -> false
                    | _ -> false
                match EnvironmentConfiguration.resolve (Some path) folder folder None false [] with
                | Error message -> error <- Some message
                | Ok effective ->
                    settings <- Some effective
                    if not trusted then blocked <- true
                    else
                        let output = Console.Out
                        let loaded =
                            try
                                Console.SetOut(Console.Error)
                                EnvironmentLoader.loadWithInfo effective
                            finally Console.SetOut(output)
                        match loaded with
                        | Ok loaded ->
                            loadedEnvironment <- Some loaded
                            registration <- Some loaded.Registration
                        | Error message -> error <- Some $"{path}: {message}"
            | None -> ()
        | None -> ()

    let forSourcePath (sourcePath: string) : ExtensionRegistration =
        registration |> Option.defaultWith (fun () -> HostInterop.ofExterns (InteropServices.runtimeExternsForSourcePath sourcePath))

    let descriptors sourcePath = (forSourcePath sourcePath).Types

    let unions sourcePath = (forSourcePath sourcePath).Unions

    let info () =
        let result = JsonObject()
        configurationPath |> Option.iter (fun path -> result["configurationPath"] <- JsonValue.Create(path))
        result["blockedByTrust"] <- JsonValue.Create(blocked)
        result["failed"] <- JsonValue.Create(error.IsSome)
        error |> Option.iter (fun message -> result["error"] <- JsonValue.Create(message))
        let paths = JsonArray()
        settings |> Option.iter (fun s ->
            result["rootDirectory"] <- JsonValue.Create(s.RootDirectory)
            result["defaultExterns"] <- JsonValue.Create(s.DefaultExterns)
            let watchPaths = loadedEnvironment |> Option.map (fun loaded -> loaded.AssemblyPaths) |> Option.defaultValue s.ExternAssemblies
            watchPaths |> List.iter (fun path -> paths.Add(JsonValue.Create(path))))
        let packages = JsonArray()
        loadedEnvironment |> Option.iter (fun loaded ->
            for package in loaded.Packages do
                let item = JsonObject()
                item["name"] <- JsonValue.Create(package.Name)
                item["version"] <- JsonValue.Create(package.Version)
                packages.Add item)
        result["packages"] <- packages
        result["externAssemblies"] <- paths
        result

    let publishConfigurationDiagnostics () =
        match configurationPath with
        | Some path ->
            let diagnostics =
                error |> Option.map (fun message ->
                    LspModel.diagnostic 1 "environment" (Span.mk (Span.pos 1 1) (Span.pos 1 1)) message :> JsonNode)
                |> Option.toList
            LspModel.publishDiagnostics (Uri(Path.GetFullPath path).AbsoluteUri) diagnostics
        | None -> ()
