namespace FScript.Runtime

open System.IO
open FScript.Language

module ScriptHost =
    type FunctionSignature =
        { Name: string
          ParameterNames: string list
          ParameterTypes: Type list
          ReturnType: Type }

    type LoadedScript =
        { TypeDefs: Map<string, Type>
          Env: Env
          ExportedFunctionNames: string list
          ExportedFunctionSignatures: Map<string, FunctionSignature>
          ExportedValueNames: string list
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

    let private flattenFunctionType (t: Type) : Type list * Type =
        let rec loop (acc: Type list) (current: Type) =
            match current with
            | TFun (arg, ret) -> loop (arg :: acc) ret
            | _ -> List.rev acc, current
        loop [] t

    let private flattenParameterNames (expr: Expr) : string list =
        let rec loop (acc: string list) (current: Expr) =
            match current with
            | ELambda (param, body, _) -> loop (param.Name :: acc) body
            | _ -> List.rev acc
        loop [] expr

    let private collectFunctionSignatures (program: TypeInfer.TypedProgram) : Map<string, FunctionSignature> =
        let fromLet name expr exprType =
            let paramNames = flattenParameterNames expr
            let parameterTypes, returnType = flattenFunctionType exprType
            if paramNames.IsEmpty || parameterTypes.IsEmpty then
                None
            elif paramNames.Length <> parameterTypes.Length then
                raise (HostCommon.evalError $"Signature mismatch for function '{name}'")
            else
                Some (name,
                      { Name = name
                        ParameterNames = paramNames
                        ParameterTypes = parameterTypes
                        ReturnType = returnType })

        program
        |> List.collect (function
            | TypeInfer.TSLet(name, expr, exprType, _, isExported, _) when isExported ->
                match fromLet name expr exprType with
                | Some signature -> [ signature ]
                | None -> []
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported ->
                bindings
                |> List.choose (fun (name, expr, exprType, _) -> fromLet name expr exprType)
            | _ -> [])
        |> Map.ofList

    let private loadProgram (externs: ExternalFunction list) (program: Program) : LoadedScript =
        let typed = FScript.inferWithExterns externs program
        let state = Eval.evalProgramWithExternsState externs typed
        let exportedNames =
            declaredExportedNames typed
            |> List.distinct
            |> List.sort

        let functionNames =
            exportedNames
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> isCallable value
                | None -> false)

        let functionSignatures =
            collectFunctionSignatures typed
            |> Map.filter (fun name _ -> functionNames |> List.contains name)

        let valueNames =
            exportedNames
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> not (isCallable value)
                | None -> false)

        { TypeDefs = state.TypeDefs
          Env = state.Env
          ExportedFunctionNames = functionNames
          ExportedFunctionSignatures = functionSignatures
          ExportedValueNames = valueNames
          LastValue = state.LastValue }

    let loadSource (externs: ExternalFunction list) (source: string) : LoadedScript =
        let program = FScript.parse source
        if program |> List.exists (function SImport _ -> true | _ -> false) then
            raise (HostCommon.evalError "'import' is only supported when loading scripts from files")
        loadProgram externs program

    let loadFile (externs: ExternalFunction list) (path: string) : LoadedScript =
        let fullPath = Path.GetFullPath(path)
        let rootDirectory =
            match Path.GetDirectoryName(fullPath) with
            | null
            | "" -> Directory.GetCurrentDirectory()
            | dir -> dir
        let program = FScript.parseFileWithIncludes rootDirectory fullPath
        loadProgram externs program

    let loadSourceWithIncludes
        (externs: ExternalFunction list)
        (rootDirectory: string)
        (entryFile: string)
        (entrySource: string)
        (resolveImportedSource: string -> string option)
        : LoadedScript =
        let program =
            FScript.parseSourceWithIncludesResolver
                rootDirectory
                entryFile
                entrySource
                resolveImportedSource
        loadProgram externs program

    let listFunctions (loaded: LoadedScript) : string list =
        loaded.ExportedFunctionNames

    let listValues (loaded: LoadedScript) : string list =
        loaded.ExportedValueNames

    let getValue (loaded: LoadedScript) (valueName: string) : Value =
        if not (loaded.ExportedValueNames |> List.contains valueName) then
            raise (HostCommon.evalError $"Unknown exported value '{valueName}'")
        else
            match loaded.Env.TryFind valueName with
            | Some value -> value
            | None -> raise (HostCommon.evalError $"Unknown exported value '{valueName}'")

    let invoke (loaded: LoadedScript) (functionName: string) (args: Value list) : Value =
        if not (loaded.ExportedFunctionNames |> List.contains functionName) then
            if loaded.ExportedValueNames |> List.contains functionName then
                raise (HostCommon.evalError $"'{functionName}' is a value and cannot be invoked")
            else
                raise (HostCommon.evalError $"Unknown exported function '{functionName}'")
        else
            match loaded.Env.TryFind functionName with
            | Some fnValue when isCallable fnValue ->
                Eval.invokeValue loaded.TypeDefs fnValue args
            | Some _ ->
                raise (HostCommon.evalError $"'{functionName}' is not callable")
            | None ->
                raise (HostCommon.evalError $"Unknown exported function '{functionName}'")
