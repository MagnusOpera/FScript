namespace FScript.Runtime

open System.IO
open FScript.Language

module ScriptHost =
    type LoadedScript =
        { TypeDefs: Map<string, Type>
          Env: Env
          FunctionNames: string list
          LastValue: Value }

    let private isCallable value =
        match value with
        | VClosure _ -> true
        | VExternal _ -> true
        | VUnionCtor _ -> true
        | _ -> false

    let private declaredNames (program: TypeInfer.TypedProgram) =
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, _, _, _, _) -> [ name ]
            | TypeInfer.TSLetRecGroup(bindings, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])

    let loadSource (externs: ExternalFunction list) (source: string) : LoadedScript =
        let program = FScript.parse source
        let typed = FScript.inferWithExterns externs program
        let state = Eval.evalProgramWithExternsState externs typed
        let functionNames =
            declaredNames typed
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> isCallable value
                | None -> false)
            |> List.distinct
            |> List.sort
        { TypeDefs = state.TypeDefs
          Env = state.Env
          FunctionNames = functionNames
          LastValue = state.LastValue }

    let loadFile (externs: ExternalFunction list) (path: string) : LoadedScript =
        File.ReadAllText(path) |> loadSource externs

    let listFunctions (loaded: LoadedScript) : string list =
        loaded.FunctionNames

    let invoke (loaded: LoadedScript) (functionName: string) (args: Value list) : Value =
        match loaded.Env.TryFind functionName with
        | Some fnValue when isCallable fnValue ->
            Eval.invokeValue loaded.TypeDefs fnValue args
        | Some _ ->
            raise (HostCommon.evalError $"'{functionName}' is not callable")
        | None ->
            raise (HostCommon.evalError $"Unknown function '{functionName}'")
