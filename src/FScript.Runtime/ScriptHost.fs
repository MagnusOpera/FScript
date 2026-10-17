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
          ExportedFunctionSet: Set<string>
          ExportedFunctions: Map<string, Value>
          ExportedFunctionSignatures: Map<string, FunctionSignature>
          ExportedValueNames: string list
          ExportedValueSet: Set<string>
          ExportedValues: Map<string, Value>
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

    let private collectFunctionSignatures (program: TypeInfer.TypedProgram) : Map<string, FunctionSignature> =
        let rec flatten t =
            match t with
            | TFun (argument, result) ->
                let rest, final = flatten result
                argument :: rest, final
            | other -> [], other
        let declared = ExportSignatures.fromTypedProgram program
        program
        |> List.collect (function
            | TypeInfer.TSLet(name, _, t, _, true, _) -> [name, t]
            | TypeInfer.TSLetRecGroup(bindings, true, _) -> bindings |> List.map (fun (name, _, t, _) -> name, t)
            | _ -> [])
        |> List.choose (fun (name, t) ->
            let parameters, result = flatten t
            if parameters.IsEmpty then None
            else
                let names =
                    match declared.TryFind name with
                    | Some signature -> signature.ParameterNames
                    | None -> parameters |> List.mapi (fun index _ -> $"arg{index + 1}")
                Some (name, { Name = name; ParameterNames = names; ParameterTypes = parameters; ReturnType = result }))
        |> Map.ofList

    let private loadProgram (registration: ExtensionRegistration) (program: Program) : LoadedScript =
        let typed = TypeInfer.inferProgramWithRegistration registration program
        let state = Eval.evalProgramWithRegistrationState registration typed
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
        let functionSet = functionNames |> Set.ofList
        let functions =
            functionNames
            |> List.choose (fun name ->
                match state.Env.TryFind name with
                | Some value -> Some (name, value)
                | None -> None)
            |> Map.ofList

        let functionSignatures =
            collectFunctionSignatures typed
            |> Map.filter (fun name _ -> functionSet |> Set.contains name)

        let valueNames =
            exportedNames
            |> List.filter (fun name ->
                match state.Env.TryFind name with
                | Some value -> not (isCallable value)
                | None -> false)
        let valueSet = valueNames |> Set.ofList
        let values =
            valueNames
            |> List.choose (fun name ->
                match state.Env.TryFind name with
                | Some value -> Some (name, value)
                | None -> None)
            |> Map.ofList

        { TypeDefs = state.TypeDefs
          Env = state.Env
          ExportedFunctionNames = functionNames
          ExportedFunctionSet = functionSet
          ExportedFunctions = functions
          ExportedFunctionSignatures = functionSignatures
          ExportedValueNames = valueNames
          ExportedValueSet = valueSet
          ExportedValues = values
          LastValue = state.LastValue }

    let loadSourceWithRegistration (registration: ExtensionRegistration) (source: string) : LoadedScript =
        let program = FScript.parse source
        if program |> List.exists (function SImport _ -> true | _ -> false) then
            raise (HostCommon.evalError "'import' is only supported when loading scripts from files")
        loadProgram registration program

    let loadFileWithRegistration (registration: ExtensionRegistration) (path: string) : LoadedScript =
        let fullPath = Path.GetFullPath(path)
        let rootDirectory =
            match Path.GetDirectoryName(fullPath) with
            | null
            | "" -> Directory.GetCurrentDirectory()
            | dir -> dir
        let program = FScript.parseFileWithIncludes rootDirectory fullPath
        loadProgram registration program

    let loadSourceWithIncludesAndRegistration
        (registration: ExtensionRegistration)
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
        loadProgram registration program

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
            match loaded.ExportedFunctions |> Map.tryFind functionName with
            | Some fnValue when isCallable fnValue ->
                match loaded.ExportedFunctionSignatures.TryFind functionName with
                | Some signature ->
                    let fullType = List.foldBack (fun argument result -> TFun(argument, result)) signature.ParameterTypes signature.ReturnType
                    HostInterop.validateCall loaded.TypeDefs fullType args (fun () -> Eval.invokeValue loaded.TypeDefs fnValue args)
                | None -> Eval.invokeValue loaded.TypeDefs fnValue args
            | Some _ ->
                raise (HostCommon.evalError $"'{functionName}' is not callable")
            | None ->
                raise (HostCommon.evalError $"Unknown exported function '{functionName}'")

    let loadSource externs source = loadSourceWithRegistration (HostInterop.ofExterns externs) source
    let loadFile externs path = loadFileWithRegistration (HostInterop.ofExterns externs) path
    let loadSourceWithIncludes externs root entry source resolver =
        loadSourceWithIncludesAndRegistration (HostInterop.ofExterns externs) root entry source resolver
