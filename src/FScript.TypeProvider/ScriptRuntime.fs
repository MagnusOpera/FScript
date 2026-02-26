namespace FScript.TypeProvider

#nowarn "3261"

open System
open System.Collections
open System.Collections.Concurrent
open System.Reflection
open Microsoft.FSharp.Reflection
open FScript.Language
open FScript.Runtime

module internal RuntimeTypeBridge =
    type DotNetType = System.Type

    let private fsharpCoreAssembly = typeof<list<int>>.Assembly
    let private listModuleType = fsharpCoreAssembly.GetType("Microsoft.FSharp.Collections.ListModule", true)
    let private mapModuleType = fsharpCoreAssembly.GetType("Microsoft.FSharp.Collections.MapModule", true)
    let private listOfSeqMethod = listModuleType.GetMethod("OfSeq", BindingFlags.Public ||| BindingFlags.Static)
    let private mapOfSeqMethod = mapModuleType.GetMethod("OfSeq", BindingFlags.Public ||| BindingFlags.Static)

    let private typeNameOrNull (value: obj) =
        if isNull value then "null" else value.GetType().FullName

    let rec toSystemType (t: SupportedType) : DotNetType =
        match t with
        | SupportedType.Unit -> typeof<unit>
        | SupportedType.Int64 -> typeof<int64>
        | SupportedType.Float -> typeof<float>
        | SupportedType.Bool -> typeof<bool>
        | SupportedType.String -> typeof<string>
        | SupportedType.List inner ->
            typedefof<list<_>>.MakeGenericType([| toSystemType inner |])
        | SupportedType.Option inner ->
            typedefof<option<_>>.MakeGenericType([| toSystemType inner |])
        | SupportedType.Tuple items ->
            items
            |> List.map toSystemType
            |> List.toArray
            |> FSharpType.MakeTupleType
        | SupportedType.StringMap inner ->
            typedefof<Map<_, _>>.MakeGenericType([| typeof<string>; toSystemType inner |])

    let private tryGetKeyValuePair (value: obj) : (obj * obj) option =
        if isNull value then
            None
        else
            let t = value.GetType()
            let keyProp = t.GetProperty("Key")
            let valueProp = t.GetProperty("Value")
            if isNull keyProp || isNull valueProp then
                None
            else
                Some (keyProp.GetValue(value), valueProp.GetValue(value))

    let rec toValue (expectedType: SupportedType) (input: obj) : Value =
        match expectedType with
        | SupportedType.Unit -> VUnit
        | SupportedType.Int64 ->
            match input with
            | :? int64 as value -> VInt value
            | _ -> invalidOp $"Expected int64 argument but got '{typeNameOrNull input}'."
        | SupportedType.Float ->
            match input with
            | :? float as value -> VFloat value
            | _ -> invalidOp $"Expected float argument but got '{typeNameOrNull input}'."
        | SupportedType.Bool ->
            match input with
            | :? bool as value -> VBool value
            | _ -> invalidOp $"Expected bool argument but got '{typeNameOrNull input}'."
        | SupportedType.String ->
            match input with
            | :? string as value -> VString value
            | _ -> invalidOp $"Expected string argument but got '{typeNameOrNull input}'."
        | SupportedType.List inner ->
            match input with
            | :? IEnumerable as sequence ->
                sequence
                |> Seq.cast<obj>
                |> Seq.map (toValue inner)
                |> Seq.toList
                |> VList
            | _ -> invalidOp $"Expected list argument but got '{typeNameOrNull input}'."
        | SupportedType.Option inner ->
            if isNull input then
                VOption None
            else
                let inputType = input.GetType()
                if not (FSharpType.IsUnion(inputType)) then
                    invalidOp $"Expected option argument but got '{inputType.FullName}'."
                else
                    let unionCase, fields = FSharpValue.GetUnionFields(input, inputType)
                    match unionCase.Name, fields with
                    | "None", _ -> VOption None
                    | "Some", [| value |] -> VOption (Some (toValue inner value))
                    | _ -> invalidOp $"Expected option argument but got '{inputType.FullName}'."
        | SupportedType.Tuple itemTypes ->
            if isNull input then
                invalidOp "Expected tuple argument but got null."
            elif not (FSharpType.IsTuple(input.GetType())) then
                invalidOp $"Expected tuple argument but got '{input.GetType().FullName}'."
            else
                let fields = FSharpValue.GetTupleFields(input)
                if fields.Length <> itemTypes.Length then
                    invalidOp $"Expected tuple arity {itemTypes.Length} but got {fields.Length}."
                else
                    (itemTypes, fields |> Array.toList)
                    ||> List.zip
                    |> List.map (fun (itemType, fieldValue) -> toValue itemType fieldValue)
                    |> VTuple
        | SupportedType.StringMap inner ->
            match input with
            | :? IEnumerable as sequence ->
                sequence
                |> Seq.cast<obj>
                |> Seq.map (fun item ->
                    match tryGetKeyValuePair item with
                    | Some (:? string as key, value) -> MapKey.MKString key, toValue inner value
                    | Some (key, _) ->
                        invalidOp $"Expected map key type string but got '{typeNameOrNull key}'."
                    | None ->
                        invalidOp "Expected map entries exposing Key/Value.")
                |> Seq.toList
                |> Map.ofList
                |> VMap
            | _ -> invalidOp $"Expected map argument but got '{typeNameOrNull input}'."

    let rec fromValue (expectedType: SupportedType) (value: Value) : obj =
        match expectedType, value with
        | SupportedType.Unit, VUnit -> box ()
        | SupportedType.Int64, VInt number -> box number
        | SupportedType.Float, VFloat number -> box number
        | SupportedType.Bool, VBool flag -> box flag
        | SupportedType.String, VString text -> box text
        | SupportedType.List inner, VList values ->
            let itemType = toSystemType inner
            let converted = values |> List.map (fromValue inner) |> List.toArray
            let typedArray = Array.CreateInstance(itemType, converted.Length)
            for i = 0 to converted.Length - 1 do
                typedArray.SetValue(converted[i], i)
            let ofSeq = listOfSeqMethod.MakeGenericMethod([| itemType |])
            ofSeq.Invoke(null, [| typedArray :> IEnumerable |])
        | SupportedType.Option inner, VOption maybeValue ->
            let optionType = toSystemType expectedType
            let unionCases = FSharpType.GetUnionCases(optionType)
            match maybeValue with
            | None ->
                let noneCase = unionCases |> Array.find (fun c -> c.Name = "None")
                FSharpValue.MakeUnion(noneCase, [||])
            | Some item ->
                let someCase = unionCases |> Array.find (fun c -> c.Name = "Some")
                let innerValue = fromValue inner item
                FSharpValue.MakeUnion(someCase, [| innerValue |])
        | SupportedType.Tuple itemTypes, VTuple values ->
            if itemTypes.Length <> values.Length then
                invalidOp $"Expected tuple return arity {itemTypes.Length} but got {values.Length}."
            else
                let tupleType = toSystemType expectedType
                let converted =
                    (itemTypes, values)
                    ||> List.zip
                    |> List.map (fun (itemType, itemValue) -> fromValue itemType itemValue)
                    |> List.toArray
                FSharpValue.MakeTuple(converted, tupleType)
        | SupportedType.StringMap inner, VMap entries ->
            let valueType = toSystemType inner
            let tupleType = FSharpType.MakeTupleType([| typeof<string>; valueType |])
            let tupleObjects =
                entries
                |> Map.toList
                |> List.map (fun (key, rawValue) ->
                    match key with
                    | MapKey.MKString text ->
                        let mappedValue = fromValue inner rawValue
                        FSharpValue.MakeTuple([| box text; mappedValue |], tupleType)
                    | _ ->
                        invalidOp "Expected string map keys in script result.")
                |> List.toArray
            let tupleArray = Array.CreateInstance(tupleType, tupleObjects.Length)
            for i = 0 to tupleObjects.Length - 1 do
                tupleArray.SetValue(tupleObjects[i], i)
            let ofSeq = mapOfSeqMethod.MakeGenericMethod([| typeof<string>; valueType |])
            ofSeq.Invoke(null, [| tupleArray :> IEnumerable |])
        | _ ->
            invalidOp $"Value mismatch. Expected '{Contract.canonicalType expectedType}' but got '{Pretty.valueToString value}'."

module ScriptRuntime =
    let private contractCache = ConcurrentDictionary<string, ScriptContract>()
    let private resolverCache = ConcurrentDictionary<string, RuntimeScriptResolver>()
    let private externListType = typeof<ExternalFunction list>

    let parseExternProviderNames (externProviders: string) : string list =
        externProviders.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun item -> item.Trim())
        |> Array.filter (fun item -> not (String.IsNullOrWhiteSpace(item)))
        |> Array.toList

    let private invokeExternProvider (ctx: HostContext) (providerType: System.Type) (providerMethod: MethodInfo) : ExternalFunction list =
        if providerMethod.ReturnType <> externListType then
            invalidOp $"Invalid extern provider '{providerType.FullName}.{providerMethod.Name}': return type must be ExternalFunction list."

        let parameters = providerMethod.GetParameters()
        let args =
            if parameters.Length = 0 then
                [||]
            elif parameters.Length = 1 && parameters.[0].ParameterType = typeof<HostContext> then
                [| box ctx |]
            else
                invalidOp
                    $"Invalid extern provider '{providerType.FullName}.{providerMethod.Name}': expected signature unit -> ExternalFunction list or HostContext -> ExternalFunction list."

        let provided = providerMethod.Invoke(null, args)
        if isNull provided then
            invalidOp $"Extern provider '{providerType.FullName}.{providerMethod.Name}' returned null."
        else
            match provided with
            | :? (ExternalFunction list) as externs -> externs
            | _ -> invalidOp $"Extern provider '{providerType.FullName}.{providerMethod.Name}' returned an invalid value."

    let resolveExterns (rootDirectory: string) (externProviderTypeNames: string list) : ExternalFunction list =
        let ctx = { RootDirectory = rootDirectory; DeniedPathGlobs = [] }
        let defaults = Registry.all ctx

        let userExterns =
            externProviderTypeNames
            |> List.collect (fun typeName ->
                let providerType = System.Type.GetType(typeName, throwOnError = true)
                providerType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                |> Array.toList
                |> List.filter (fun methodInfo ->
                    methodInfo.GetCustomAttributes(typeof<FScriptExternProviderAttribute>, false).Length > 0)
                |> List.collect (fun methodInfo -> invokeExternProvider ctx providerType methodInfo))

        let allExterns = defaults @ userExterns
        let conflicts =
            allExterns
            |> List.groupBy (fun ext -> ext.Name)
            |> List.filter (fun (_, values) -> values.Length > 1)
            |> List.map fst

        if conflicts.IsEmpty then
            allExterns
        else
            let conflictText = String.concat ", " conflicts
            invalidOp $"External function name conflicts detected: {conflictText}"

    let private getContract (contractJson: string) =
        let parsed = Contract.fromJson contractJson
        contractCache.GetOrAdd(parsed.ContractId, fun _ -> parsed)

    let setResolver (contractId: string) (resolver: RuntimeScriptResolver) : unit =
        resolverCache.[contractId] <- resolver

    let clearResolver (contractId: string) : unit =
        resolverCache.TryRemove(contractId) |> ignore

    let private getRuntimeOverride (contractId: string) =
        match resolverCache.TryGetValue(contractId) with
        | true, resolver -> resolver ()
        | _ -> None

    let private computeFingerprint (loaded: ScriptHost.LoadedScript) : string =
        let functionContracts =
            loaded.ExportedFunctionSignatures
            |> Map.toList
            |> List.map snd
            |> List.map (fun signature ->
                match Contract.fromExportedSignature
                        { Name = signature.Name
                          ParameterNames = signature.ParameterNames
                          ParameterTypes = signature.ParameterTypes
                          ReturnType = signature.ReturnType } with
                | Ok value -> value
                | Error err -> invalidOp err)
        Contract.fingerprint functionContracts

    let private loadScript (contract: ScriptContract) : ScriptHost.LoadedScript =
        let runtimeOverride = getRuntimeOverride contract.ContractId
        let rootDirectory =
            match runtimeOverride with
            | Some value -> value.RootDirectory
            | None -> contract.CompileRootDirectory
        let externs = resolveExterns rootDirectory contract.ExternProviderTypeNames
        match runtimeOverride with
        | Some value ->
            let resolver = defaultArg value.ResolveImportedSource (fun _ -> None)
            ScriptHost.loadSourceWithIncludes externs value.RootDirectory value.EntryFile value.EntrySource resolver
        | None ->
            ScriptHost.loadFile externs contract.CompileScriptPath

    let invoke (contractJson: string) (functionName: string) (args: obj array) : obj =
        let contract = getContract contractJson
        let loaded = loadScript contract
        let runtimeFingerprint = computeFingerprint loaded
        if not (String.Equals(contract.Fingerprint, runtimeFingerprint, StringComparison.Ordinal)) then
            invalidOp
                $"Runtime script signature mismatch for contract '{contract.ContractId}'. Expected fingerprint '{contract.Fingerprint}' but found '{runtimeFingerprint}'."

        let functionContract =
            contract.Functions
            |> List.tryFind (fun f -> f.Name = functionName)
            |> Option.defaultWith (fun () -> invalidOp $"Unknown exported function '{functionName}'.")

        if args.Length <> functionContract.ParameterTypes.Length then
            invalidOp $"Function '{functionName}' expects {functionContract.ParameterTypes.Length} argument(s), got {args.Length}."

        let valueArgs =
            (functionContract.ParameterTypes, args |> Array.toList)
            ||> List.zip
            |> List.map (fun (argType, argValue) -> RuntimeTypeBridge.toValue argType argValue)

        let result = ScriptHost.invoke loaded functionName valueArgs
        RuntimeTypeBridge.fromValue functionContract.ReturnType result

    let createContractJson
        (contractId: string)
        (compileScriptPath: string)
        (compileRootDirectory: string)
        (externProviderTypeNames: string list)
        (functions: FunctionContract list)
        : string =
        let contract =
            { ContractId = contractId
              CompileScriptPath = compileScriptPath
              CompileRootDirectory = compileRootDirectory
              ExternProviderTypeNames = externProviderTypeNames
              Fingerprint = Contract.fingerprint functions
              Functions = functions }
        Contract.toJson contract
