namespace FScript.Runtime

open System.IO
open FScript.Language

module ScriptHost =
    type ExecutionMode =
        | Interpreted
        | Compiled

    type LoadOptions =
        { ExecutionMode: ExecutionMode }

    let defaultLoadOptions : LoadOptions =
        { ExecutionMode = Compiled }

    type FunctionSignature =
        { Name: string
          ParameterNames: string list
          ParameterTypes: Type list
          ReturnType: Type }

    type LoadedScript =
        { ExecutionMode: ExecutionMode
          Executable: Executable.ExecutableProgram option
          TypeDefs: Map<string, Type>
          Env: Env
          ExportedFunctionNames: string list
          ExportedFunctionSet: Set<string>
          ExportedFunctions: Map<string, Value>
          ExportedFunctionInvokers: Map<string, (Value list -> Value)>
          ExportedFunctionSignatures: Map<string, FunctionSignature>
          ExportedValueNames: string list
          ExportedValueSet: Set<string>
          ExportedValues: Map<string, Value>
          LastValue: Value }

    let private declaredExportedNames (program: TypeInfer.TypedProgram) =
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, _, _, _, isExported, _) when isExported -> [ name ]
            | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported -> bindings |> List.map (fun (name, _, _, _) -> name)
            | _ -> [])
        |> List.distinct
        |> List.sort

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

    let private loadProgram (options: LoadOptions) (externs: ExternalFunction list) (program: Program) : LoadedScript =
        let typed = FScript.inferWithExterns externs program
        let executionMode = options.ExecutionMode
        let executable, state, functionNames, functionSet, functions, functionInvokers, functionSignatures, valueNames, valueSet, values =
            match executionMode with
            | Compiled ->
                let executable = FScript.compileWithExterns externs typed
                let loadedExecutable = Executable.load executable
                let state = Executable.loadedState loadedExecutable
                let functionNames = Executable.loadedFunctionNames loadedExecutable
                let functionSet = Executable.loadedFunctionSet loadedExecutable
                let functions = Executable.loadedFunctions loadedExecutable
                let functionInvokers = Executable.loadedFunctionInvokers loadedExecutable
                let functionSignatures =
                    Executable.loadedFunctionSignatures loadedExecutable
                    |> Map.map (fun _ signature ->
                        { Name = signature.Name
                          ParameterNames = signature.ParameterNames
                          ParameterTypes = signature.ParameterTypes
                          ReturnType = signature.ReturnType })
                let valueNames = Executable.loadedValueNames loadedExecutable
                let valueSet = Executable.loadedValueSet loadedExecutable
                let values = Executable.loadedValues loadedExecutable
                Some executable, state, functionNames, functionSet, functions, functionInvokers, functionSignatures, valueNames, valueSet, values
            | Interpreted ->
                let state = Eval.evalProgramWithExternsState externs typed
                let exportedNames = declaredExportedNames typed
                let functionNames =
                    exportedNames
                    |> List.filter (fun name ->
                        match state.Env.TryFind name with
                        | Some value ->
                            match value with
                            | VClosure _
                            | VExternal _
                            | VUnionCtor _ -> true
                            | _ -> false
                        | None -> false)
                let functionSet = functionNames |> Set.ofList
                let functions =
                    functionNames
                    |> List.choose (fun name ->
                        match state.Env.TryFind name with
                        | Some value -> Some (name, value)
                        | None -> None)
                    |> Map.ofList
                let functionInvokers =
                    functions
                    |> Map.map (fun _ fnValue -> (fun args -> Eval.invokeValue state.TypeDefs fnValue args))
                let functionSignatures =
                    collectFunctionSignatures typed
                    |> Map.filter (fun name _ -> functionSet |> Set.contains name)
                let valueNames =
                    exportedNames
                    |> List.filter (fun name ->
                        match state.Env.TryFind name with
                        | Some value ->
                            match value with
                            | VClosure _
                            | VExternal _
                            | VUnionCtor _ -> false
                            | _ -> true
                        | None -> false)
                let valueSet = valueNames |> Set.ofList
                let values =
                    valueNames
                    |> List.choose (fun name ->
                        match state.Env.TryFind name with
                        | Some value -> Some (name, value)
                        | None -> None)
                    |> Map.ofList
                None, state, functionNames, functionSet, functions, functionInvokers, functionSignatures, valueNames, valueSet, values

        { ExecutionMode = executionMode
          Executable = executable
          TypeDefs = state.TypeDefs
          Env = state.Env
          ExportedFunctionNames = functionNames
          ExportedFunctionSet = functionSet
          ExportedFunctions = functions
          ExportedFunctionInvokers = functionInvokers
          ExportedFunctionSignatures = functionSignatures
          ExportedValueNames = valueNames
          ExportedValueSet = valueSet
          ExportedValues = values
          LastValue = state.LastValue }

    let loadSourceWithOptions (options: LoadOptions) (externs: ExternalFunction list) (source: string) : LoadedScript =
        let program = FScript.parse source
        if program |> List.exists (function SImport _ -> true | _ -> false) then
            raise (HostCommon.evalError "'import' is only supported when loading scripts from files")
        loadProgram options externs program

    let loadSource (externs: ExternalFunction list) (source: string) : LoadedScript =
        loadSourceWithOptions defaultLoadOptions externs source

    let loadFileWithOptions (options: LoadOptions) (externs: ExternalFunction list) (path: string) : LoadedScript =
        let fullPath = Path.GetFullPath(path)
        let rootDirectory =
            match Path.GetDirectoryName(fullPath) with
            | null
            | "" -> Directory.GetCurrentDirectory()
            | dir -> dir
        let program = FScript.parseFileWithIncludes rootDirectory fullPath
        loadProgram options externs program

    let loadFile (externs: ExternalFunction list) (path: string) : LoadedScript =
        loadFileWithOptions defaultLoadOptions externs path

    let loadSourceWithIncludesWithOptions
        (options: LoadOptions)
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
        loadProgram options externs program

    let loadSourceWithIncludes
        (externs: ExternalFunction list)
        (rootDirectory: string)
        (entryFile: string)
        (entrySource: string)
        (resolveImportedSource: string -> string option)
        : LoadedScript =
        loadSourceWithIncludesWithOptions
            defaultLoadOptions
            externs
            rootDirectory
            entryFile
            entrySource
            resolveImportedSource

    let listFunctions (loaded: LoadedScript) : string list =
        loaded.ExportedFunctionNames

    let listValues (loaded: LoadedScript) : string list =
        loaded.ExportedValueNames

    let getValue (loaded: LoadedScript) (valueName: string) : Value =
        if not (loaded.ExportedValueSet |> Set.contains valueName) then
            raise (HostCommon.evalError $"Unknown exported value '{valueName}'")
        else
            match loaded.ExportedValues |> Map.tryFind valueName with
            | Some value -> value
            | None -> raise (HostCommon.evalError $"Unknown exported value '{valueName}'")

    let invoke (loaded: LoadedScript) (functionName: string) (args: Value list) : Value =
        if not (loaded.ExportedFunctionSet |> Set.contains functionName) then
            if loaded.ExportedValueSet |> Set.contains functionName then
                raise (HostCommon.evalError $"'{functionName}' is a value and cannot be invoked")
            else
                raise (HostCommon.evalError $"Unknown exported function '{functionName}'")
        else
            match loaded.ExportedFunctionInvokers |> Map.tryFind functionName with
            | Some invoke -> invoke args
            | None ->
                raise (HostCommon.evalError $"Unknown exported function '{functionName}'")
