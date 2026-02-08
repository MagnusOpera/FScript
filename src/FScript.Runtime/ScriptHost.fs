namespace FScript.Runtime

open System.IO
open FScript.Language

module ScriptHost =
    type LoadedScript =
        { TypeDefs: Map<string, Type>
          Env: Env
          ExportedFunctionNames: string list
          LastValue: Value }

    let private isCallable value =
        match value with
        | VClosure _ -> true
        | VExternal _ -> true
        | VUnionCtor _ -> true
        | _ -> false

    let private declaredExportedNames (program: TypeInfer.TypedProgram) =
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, _, _, _, isExported, _) when isExported -> [ name ]
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])

    let loadSource (externs: ExternalFunction list) (source: string) : LoadedScript =
        let program = FScript.parse source
        let typed = FScript.inferWithExterns externs program
        let state = Eval.evalProgramWithExternsState externs typed
        let functionNames =
            declaredExportedNames typed
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> isCallable value
                | None -> false)
            |> List.distinct
            |> List.sort
        { TypeDefs = state.TypeDefs
          Env = state.Env
          ExportedFunctionNames = functionNames
          LastValue = state.LastValue }

    let loadFile (externs: ExternalFunction list) (path: string) : LoadedScript =
        File.ReadAllText(path) |> loadSource externs

    let listFunctions (loaded: LoadedScript) : string list =
        loaded.ExportedFunctionNames

    let invoke (loaded: LoadedScript) (functionName: string) (args: Value list) : Value =
        if not (loaded.ExportedFunctionNames |> List.contains functionName) then
            raise (HostCommon.evalError $"Unknown exported function '{functionName}'")
        else
            match loaded.Env.TryFind functionName with
            | Some fnValue when isCallable fnValue ->
                Eval.invokeValue loaded.TypeDefs fnValue args
            | Some _ ->
                raise (HostCommon.evalError $"'{functionName}' is not callable")
            | None ->
                raise (HostCommon.evalError $"Unknown exported function '{functionName}'")
