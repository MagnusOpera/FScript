module FScript.JavaScript

open System
open System.Globalization
open Fable.Core
open Fable.Core.JsInterop
open FScript.Language

type ParsedProgramHandle =
    { kind: string
      program: FScript.Language.Program
      registration: ExtensionRegistration }

type TypedProgramHandle =
    { kind: string
      typedProgram: TypeInfer.TypedProgram
      registration: ExtensionRegistration }

type LoadedScriptHandle =
    { kind: string
      typeDefs: Map<string, Type>
      env: Env
      exportedFunctionNames: string list
      exportedFunctionSet: Set<string>
      exportedFunctions: Map<string, Value>
      exportedFunctionTypes: Map<string, Type>
      exportedValueNames: string list
      exportedValueSet: Set<string>
      exportedValues: Map<string, Value>
      lastValue: Value
      registration: ExtensionRegistration }

type SessionHandle =
    { kind: string
      mutable interactiveSession: Interactive.Session
      options: obj
      registration: ExtensionRegistration }

[<Emit("typeof $0 === 'undefined' || $0 === null")>]
let private isNullOrUndefined (_value: obj) : bool = jsNative

[<Emit("typeof $0")>]
let private jsTypeOf (_value: obj) : string = jsNative

[<Emit("$0[$1]")>]
let private getProperty (_target: obj) (_name: string) : obj = jsNative

[<Emit("Array.isArray($0)")>]
let private isArray (_value: obj) : bool = jsNative

[<Emit("Number.isInteger($0)")>]
let private isInteger (_value: obj) : bool = jsNative

[<Emit("Number.isSafeInteger($0)")>]
let private isSafeInteger (_value: obj) : bool = jsNative

[<Emit("Object.keys($0)")>]
let private objectKeys (_value: obj) : string array = jsNative

[<Emit("Object.fromEntries($0)")>]
let private propertiesFromArray (_entries: (string * obj) array) : obj = jsNative

let private propertiesObject entries = entries |> List.toArray |> propertiesFromArray

[<Emit("$0($1)")>]
let private callOne (_fn: obj) (_arg: obj) : obj = jsNative

[<Emit("(() => { const e = new Error($0.message); Object.assign(e, $0); throw e; })()")>]
let private throwJs (_payload: obj) : 'T = jsNative

[<Emit("(() => { throw $0; })()")>]
let private rethrowJs (_error: obj) : 'T = jsNative

[<Emit("null")>]
let private nullObj : obj = jsNative

let private tryGetProperty name value =
    if isNullOrUndefined value then
        None
    else
        let property = getProperty value name
        if isNullOrUndefined property then None else Some property

let private getRequiredProperty name value =
    match tryGetProperty name value with
    | Some property -> property
    | None -> failwith $"Missing required property '{name}'"

let private isFScriptError value =
    match tryGetProperty "kind" value with
    | Some kind -> unbox<string> kind = "fscript-error"
    | None -> false

let private positionToObj (position: Position) =
    createObj
        [ "file" ==> (position.File |> Option.map box |> Option.defaultValue nullObj)
          "line" ==> position.Line
          "column" ==> position.Column ]

let private spanToObj (span: Span) =
    createObj
        [ "start" ==> positionToObj span.Start
          "end" ==> positionToObj span.End ]

let private throwLanguageError phase message span =
    throwJs
        (createObj
            [ "kind" ==> "fscript-error"
              "phase" ==> phase
              "message" ==> message
              "span" ==> spanToObj span ])

let private withStructuredErrors work =
    try
        work ()
    with
    | ParseException error -> throwLanguageError "parse" error.Message error.Span
    | TypeException error -> throwLanguageError "type" error.Message error.Span
    | EvalException error -> throwLanguageError "eval" error.Message error.Span
    | ex ->
        let error = box ex
        if isFScriptError error then
            rethrowJs error
        else
            throwJs
                (createObj
                    [ "kind" ==> "fscript-error"
                      "phase" ==> "host"
                      "message" ==> ex.Message ])

// Weak maps keep interpreter values and payloads off the public JavaScript objects.
[<Emit("new WeakMap()")>]
let private newWeakMap () : obj = jsNative

[<Emit("$0.get($1)")>]
let private weakGet (_map: obj) (_key: obj) : obj = jsNative

[<Emit("$0.set($1, $2)")>]
let private weakSet (_map: obj) (_key: obj) (_value: obj) : unit = jsNative

[<Emit("Object.freeze($0)")>]
let private freeze (_value: obj) : obj = jsNative

[<Emit("(() => { const visit = v => { if (v && typeof v === 'object' && !Object.isFrozen(v)) { for (const key of Object.keys(v)) visit(v[key]); Object.freeze(v); } }; visit($0); return $0; })()")>]
let private deepFreeze (_value: obj) : obj = jsNative

[<Emit("$0 != null && typeof $0.then === 'function'")>]
let private isThenable (_value: obj) : bool = jsNative

[<Emit("Object.is($0, $1)")>]
let private sameObject (_left: obj) (_right: obj) : bool = jsNative

[<Emit("$0($1, $2)")>]
let private callTwo (_fn: obj) (_first: obj) (_second: obj) : obj = jsNative

let private hostValues = newWeakMap ()
let private registrations = newWeakMap ()
let private boundaryHandles = newWeakMap ()
let private externDefinitions = newWeakMap ()

type private BoundaryHandle =
    { Value: Value
      Context: ExternalCallContext option
      Signature: Type option }

let private rememberHandle context value display =
    let handle = freeze display
    weakSet boundaryHandles handle (box { Value = value; Context = context; Signature = None })
    handle

let private int64FromObj value =
    match jsTypeOf value with
    | "bigint" -> unbox<int64> value
    | "number" ->
        if not (isSafeInteger value) then
            failwith "Expected a safe integer"
        int64 (unbox<float> value)
    | "string" ->
#if FABLE_COMPILER
        Int64.Parse(unbox<string> value)
#else
        Int64.Parse(unbox<string> value, CultureInfo.InvariantCulture)
#endif
    | _ -> failwith "Expected int value"

let private intFromObj value =
    match jsTypeOf value with
    | "number" -> int (unbox<float> value)
    | "bigint" -> int (unbox<int64> value)
    | "string" ->
#if FABLE_COMPILER
        Int32.Parse(unbox<string> value)
#else
        Int32.Parse(unbox<string> value, CultureInfo.InvariantCulture)
#endif
    | _ -> failwith "Expected numeric value"

let private mapKeyToTagged key =
    match key with
    | MKString value ->
        createObj [ "kind" ==> "string"; "value" ==> value ]
    | MKInt value ->
        createObj [ "kind" ==> "int"; "value" ==> value ]
    | MKBool value ->
        createObj [ "kind" ==> "bool"; "value" ==> value ]
    | MKUnionCase (typeName, caseName) ->
        createObj [ "kind" ==> "union"; "typeName" ==> typeName; "caseName" ==> caseName; "value" ==> nullObj ]

let rec private valueToTaggedWithContext context value : obj =
    let valueToTagged = valueToTaggedWithContext context
    match value with
    | VUnit -> createObj [ "kind" ==> "unit" ]
    | VInt value -> createObj [ "kind" ==> "int"; "value" ==> value ]
    | VFloat value -> createObj [ "kind" ==> "float"; "value" ==> value ]
    | VBool value -> createObj [ "kind" ==> "bool"; "value" ==> value ]
    | VString value -> createObj [ "kind" ==> "string"; "value" ==> value ]
    | VList values ->
        createObj
            [ "kind" ==> "list"
              "values" ==> (values |> List.map valueToTagged |> List.toArray) ]
    | VTuple values ->
        createObj
            [ "kind" ==> "tuple"
              "values" ==> (values |> List.map valueToTagged |> List.toArray) ]
    | VHostManaged host ->
        let fields = host.Fields |> Map.toList |> List.map (fun (name, v) -> name ==> valueToTagged v) |> propertiesObject |> deepFreeze
        let handle = createObj [ "kind" ==> "host"; "typeName" ==> host.Descriptor.Name; "fields" ==> fields ] |> freeze
        weakSet hostValues handle (box value)
        handle
    | VRecord fields ->
        let fieldObject =
            fields
            |> Map.toList
            |> List.map (fun (name, fieldValue) -> name ==> valueToTagged fieldValue)
            |> propertiesObject
        createObj [ "kind" ==> "record"; "fields" ==> fieldObject ]
    | VMap fields ->
        let entries =
            fields
            |> Map.toList
            |> List.map (fun (key, entryValue) ->
                createObj
                    [ "key" ==> mapKeyToTagged key
                      "value" ==> valueToTagged entryValue ])
            |> List.toArray
        createObj [ "kind" ==> "map"; "entries" ==> entries ]
    | VOption None ->
        createObj [ "kind" ==> "option"; "value" ==> nullObj ]
    | VOption (Some value) ->
        createObj [ "kind" ==> "option"; "value" ==> valueToTagged value ]
    | VUnionCase (typeName, caseName, payload) ->
        createObj
            [ "kind" ==> "union"
              "typeName" ==> typeName
              "caseName" ==> caseName
              "value" ==> (payload |> Option.map valueToTagged |> Option.defaultValue nullObj) ]
    | VUnionCtor (typeName, caseName) ->
        rememberHandle context value (createObj
            [ "kind" ==> "opaque"
              "valueType" ==> "union-constructor"
              "typeName" ==> typeName
              "caseName" ==> caseName ])
    | VTypeToken t ->
        rememberHandle context value (createObj [ "kind" ==> "type"; "name" ==> Types.typeToString t ])
    | VTask _ ->
        rememberHandle context value (createObj [ "kind" ==> "opaque"; "valueType" ==> "task" ])
    | VClosure _ ->
        rememberHandle context value (createObj [ "kind" ==> "opaque"; "valueType" ==> "function" ])
    | VExternal (externalFunction, existingArgs) ->
        rememberHandle context value (createObj
            [ "kind" ==> "opaque"
              "valueType" ==> "external"
              "name" ==> externalFunction.Name
              "applied" ==> existingArgs.Length
              "arity" ==> externalFunction.Arity ])

let private valueToTagged value = valueToTaggedWithContext None value

let rec private valueFromJs value : Value =
    if isNullOrUndefined value then
        VUnit
    elif isThenable value then
        failwith "JavaScript externs must return synchronously; Promises are not supported"
    elif isArray value then
        value
        |> unbox<obj array>
        |> Array.toList
        |> List.map valueFromJs
        |> VList
    else
        match jsTypeOf value with
        | "boolean" -> VBool (unbox<bool> value)
        | "bigint" -> VInt (unbox<int64> value)
        | "number" ->
            if isInteger value && isSafeInteger value then
                VInt (int64 (unbox<float> value))
            else
                VFloat (unbox<float> value)
        | "string" -> VString (unbox<string> value)
        | "object" ->
            match tryGetProperty "kind" value with
            | Some kindValue ->
                match unbox<string> kindValue with
                | "host" ->
                    let stored = weakGet hostValues value
                    if isNullOrUndefined stored then failwith "Invalid or forged host-value handle"
                    unbox<Value> stored
                | "opaque" | "type" ->
                    let stored = weakGet boundaryHandles value
                    if isNullOrUndefined stored then failwith "Invalid or forged opaque handle"
                    let handle = unbox<BoundaryHandle> stored
                    match handle.Context with
                    | Some context when not (context.IsActive ()) -> failwith "Expired invocation handle"
                    | None when (match handle.Value with VTypeToken _ -> false | _ -> true) ->
                        failwith "Callable handles can only be used during their originating invocation"
                    | _ -> handle.Value
                | "unit" -> VUnit
                | "int" -> getRequiredProperty "value" value |> int64FromObj |> VInt
                | "float" ->
                    let number = getRequiredProperty "value" value
                    if jsTypeOf number <> "number" then failwith "Expected a JavaScript number in a float tag"
                    VFloat (unbox<float> number)
                | "bool" ->
                    let flag = getRequiredProperty "value" value
                    if jsTypeOf flag <> "boolean" then failwith "Expected a JavaScript boolean in a bool tag"
                    VBool (unbox<bool> flag)
                | "string" ->
                    let text = getRequiredProperty "value" value
                    if jsTypeOf text <> "string" then failwith "Expected a JavaScript string in a string tag"
                    VString (unbox<string> text)
                | "list" ->
                    getRequiredProperty "values" value
                    |> unbox<obj array>
                    |> Array.toList
                    |> List.map valueFromJs
                    |> VList
                | "tuple" ->
                    getRequiredProperty "values" value
                    |> unbox<obj array>
                    |> Array.toList
                    |> List.map valueFromJs
                    |> VTuple
                | "record" ->
                    let fields = getRequiredProperty "fields" value
                    objectKeys fields
                    |> Array.toList
                    |> List.map (fun name -> name, getProperty fields name |> valueFromJs)
                    |> Map.ofList
                    |> VRecord
                | "map" ->
                    getRequiredProperty "entries" value
                    |> unbox<obj array>
                    |> Array.toList
                    |> List.map (fun entry ->
                        let keyValue = getRequiredProperty "key" entry |> valueFromJs
                        let key =
                            match keyValue with
                            | VString text -> MKString text
                            | VInt number -> MKInt number
                            | VBool flag -> MKBool flag
                            | VUnionCase (typeName, caseName, None) -> MKUnionCase(typeName, caseName)
                            | _ -> failwith "Map key must be string, int, bool, or a payload-free union case"
                        key, getRequiredProperty "value" entry |> valueFromJs)
                    |> Map.ofList
                    |> VMap
                | "option" ->
                    match tryGetProperty "value" value with
                    | None -> VOption None
                    | Some optionValue when isNullOrUndefined optionValue -> VOption None
                    | Some optionValue -> VOption (Some (valueFromJs optionValue))
                | "union" ->
                    let payload =
                        match tryGetProperty "value" value with
                        | None -> None
                        | Some payloadValue when isNullOrUndefined payloadValue -> None
                        | Some payloadValue -> Some (valueFromJs payloadValue)
                    VUnionCase(
                        getRequiredProperty "typeName" value |> unbox<string>,
                        getRequiredProperty "caseName" value |> unbox<string>,
                        payload)
                | other -> failwith $"Unsupported FScript tagged value kind '{other}'"
            | None ->
                objectKeys value
                |> Array.toList
                |> List.map (fun name -> name, getProperty value name |> valueFromJs)
                |> Map.ofList
                |> VRecord
        | actual -> failwith $"Unsupported JavaScript value type '{actual}'"

let private externsFromArray value =
    if isNullOrUndefined value then
        []
    else
        value |> unbox<ExternalFunction array> |> Array.toList

let rec private tagSignature definitions expected value tagged =
    let fields expectedFields actualFields taggedFields =
        expectedFields |> Map.iter (fun name t ->
            actualFields |> Map.tryFind name |> Option.iter (fun v -> tagSignature definitions t v (getProperty taggedFields name)))
    match expected, value with
    | TNamed name, _ -> definitions |> Map.tryFind name |> Option.iter (fun t -> tagSignature definitions t value tagged)
    | TFun _, _ ->
        let stored = weakGet boundaryHandles tagged
        if not (isNullOrUndefined stored) then
            let handle = unbox<BoundaryHandle> stored
            weakSet boundaryHandles tagged (box {handle with Signature = Some expected})
    | TList t, VList values ->
        let tags = getProperty tagged "values" |> unbox<obj array>
        values |> List.iteri (fun i v -> tagSignature definitions t v tags[i])
    | TTuple ts, VTuple values ->
        let tags = getProperty tagged "values" |> unbox<obj array>
        List.zip ts values |> List.iteri (fun i (t, v) -> tagSignature definitions t v tags[i])
    | TOption t, VOption (Some v) -> tagSignature definitions t v (getProperty tagged "value")
    | TRecord ts, VRecord vs -> fields ts vs (getProperty tagged "fields")
    | THostManaged (_, ts), VHostManaged host -> fields ts host.Fields (getProperty tagged "fields")
    | TUnion (_, cases), VUnionCase (_, caseName, Some v) ->
        cases |> Map.tryFind caseName |> Option.flatten |> Option.iter (fun t -> tagSignature definitions t v (getProperty tagged "value"))
    | TMap (_, t), VMap values ->
        let tags = getProperty tagged "entries" |> unbox<obj array>
        values |> Map.toList |> List.iteri (fun i (_, v) -> tagSignature definitions t v (getProperty tags[i] "value"))
    | _ -> ()

let private buildExternal (definitions: Map<string, Type>) (definition: obj) : ExternalFunction =
    withStructuredErrors (fun () ->
        let name = getRequiredProperty "name" definition |> unbox<string>
        let arity = getRequiredProperty "arity" definition |> intFromObj
        let scheme = getRequiredProperty "scheme" definition |> unbox<Scheme>
        let invoke = getRequiredProperty "invoke" definition
        if jsTypeOf invoke <> "function" then failwith "invoke must be a JavaScript function"
        { Name = name
          Arity = arity
          Scheme = scheme
          Impl =
            fun context args ->
                let check () =
                    if not (context.IsActive ()) then failwith "Expired invocation context"
                    context.CheckRuntime ()
                let rec checkHandle value =
                    if not (isNullOrUndefined value) && jsTypeOf value = "object" then
                        let stored = weakGet boundaryHandles value
                        if not (isNullOrUndefined stored) then
                            let handle = unbox<BoundaryHandle> stored
                            match handle.Context with
                            | Some origin when not (sameObject origin.Session context.Session) -> failwith "Foreign invocation handle"
                            | Some origin when not (origin.IsActive ()) -> failwith "Expired invocation handle"
                            | None when (match handle.Value with VTypeToken _ -> false | _ -> true) ->
                                failwith "Callable handles require their originating invocation context"
                            | _ -> ()
                        elif isNullOrUndefined (weakGet hostValues value) then
                            if isArray value then value |> unbox<obj array> |> Array.iter checkHandle
                            else objectKeys value |> Array.iter (fun key -> checkHandle (getProperty value key))
                let convert value =
                    if isThenable value then failwith "JavaScript externs must return synchronously; Promises are not supported"
                    checkHandle value
                    valueFromJs value
                let api = createObj [
                    "apply" ==> fun fn arguments ->
                        check ()
                        let handleObject = weakGet boundaryHandles fn
                        let fnValue = convert fn
                        let arguments = arguments |> unbox<obj array> |> Array.toList |> List.map convert
                        let signature = if isNullOrUndefined handleObject then None else (unbox<BoundaryHandle> handleObject).Signature
                        let invoke () = List.fold context.Apply fnValue arguments
                        let result =
                            match signature with
                            | Some signature when Types.ftvType signature |> Set.isEmpty ->
                                HostInterop.validateCall definitions signature arguments invoke
                            | Some _ -> failwith "Callback invocation requires a closed function type; use HostFunctions.create"
                            | None -> failwith "Callback has no declared function signature"
                        let tagged = valueToTaggedWithContext (Some context) result
                        let rec resultType count t =
                            match count, t with
                            | 0, _ -> Some t
                            | n, TFun (_, t) -> resultType (n - 1) t
                            | _ -> None
                        signature |> Option.bind (resultType arguments.Length) |> Option.iter (fun t -> tagSignature definitions t result tagged)
                        tagged
                    "spawnTask" ==> fun fn -> check (); context.SpawnTask (convert fn) |> valueToTaggedWithContext (Some context)
                    "awaitTask" ==> fun task -> check (); context.AwaitTask (convert task) |> valueToTaggedWithContext (Some context)
                    "checkRuntime" ==> fun () -> check () ] |> freeze
                let taggedArgs = args |> List.map (valueToTaggedWithContext (Some context))
                let (Forall (_, signature)) = scheme
                let rec annotate signature values tags =
                    match signature, values, tags with
                    | TFun (t, rest), v :: vs, tag :: ts -> tagSignature definitions t v tag; annotate rest vs ts
                    | _ -> ()
                annotate signature args taggedArgs
                let result = callTwo invoke (taggedArgs |> List.toArray |> box) api
                convert result })

let private validateRegistration (registration: ExtensionRegistration) =
    let definitions = HostInterop.typeDefinitions registration []
    let rec validateType t =
        match t with
        | TNamed name when not (definitions.ContainsKey name) -> failwith $"Unknown registered type '{name}'"
        | TList t | TOption t | TTask t -> validateType t
        | TFun (a, b) | TMap (a, b) -> validateType a; validateType b
        | TTuple ts -> List.iter validateType ts
        | TRecord fields | THostManaged (_, fields) | TRecordShape (_, fields, _) -> fields |> Map.iter (fun _ t -> validateType t)
        | TUnion (_, cases) -> cases |> Map.iter (fun _ t -> Option.iter validateType t)
        | _ -> ()
    definitions |> Map.iter (fun _ t ->
        validateType t
        if not (Types.ftvType t |> Set.isEmpty) then failwith "Host descriptors require closed field and payload types")
    for descriptor in registration.Types do
        descriptor.Fields |> Map.iter (fun _ t ->
            if not (Types.ftvType t |> Set.isEmpty) then failwith "Host descriptors require closed field types")
    for descriptor in registration.Unions do
        descriptor.Cases |> Map.iter (fun _ t ->
            t |> Option.iter (fun t ->
                if not (Types.ftvType t |> Set.isEmpty) then failwith "Host descriptors require closed union payload types"))
    let mutable names = Set.empty
    for fn in registration.Functions do
        if String.IsNullOrWhiteSpace fn.Name || names.Contains fn.Name then failwith $"Host function name conflict: '{fn.Name}'"
        if fn.Arity < 0 then failwith "Host function arity must not be negative"
        names <- names.Add fn.Name
        let (Forall (variables, t)) = fn.Scheme
        let rec accepts count t =
            if count = 0 then true
            else match t with TFun (_, rest) -> accepts (count - 1) rest | _ -> false
        if not (accepts fn.Arity t) then failwith $"Scheme does not accept {fn.Arity} arguments for '{fn.Name}'"
        if not (Set.isSubset (Types.ftvType t) (Set.ofList variables)) then failwith $"Unbound scheme variables for '{fn.Name}'"
        validateType t
    let functions = registration.Functions |> List.map (fun fn ->
        let definition = weakGet externDefinitions (box fn)
        if isNullOrUndefined definition then fn else
        let prepared = buildExternal definitions definition
        weakSet externDefinitions (box prepared) definition
        prepared)
    { registration with Functions = functions }

let private registrationFromHandle value =
    let stored = weakGet registrations value
    if isNullOrUndefined stored then failwith "Expected an extension registration handle"
    unbox<ExtensionRegistration> stored

let private registrationOverride fallback value =
    if isNullOrUndefined value then fallback
    elif isArray value then validateRegistration { fallback with Functions = externsFromArray value }
    else registrationFromHandle value

let private registrationFromOptions options =
    let registration =
        match tryGetProperty "registration" options with
        | Some value -> registrationFromHandle value
        | None -> HostInterop.empty
    let functions = tryGetProperty "externs" options |> Option.map externsFromArray |> Option.defaultValue []
    validateRegistration { registration with Functions = registration.Functions @ functions }

let private optionString name fallback options =
    match tryGetProperty name options with
    | Some value -> unbox<string> value
    | None -> fallback

let private resolverFromOptions options =
    let sourceMap = tryGetProperty "sources" options
    let callback = tryGetProperty "resolveImport" options
    fun path ->
        match callback with
        | Some resolve ->
            let value = callOne resolve (box path)
            if isNullOrUndefined value then
                None
            else
                Some (unbox<string> value)
        | None ->
            match sourceMap with
            | Some sources ->
                let value = getProperty sources path
                if isNullOrUndefined value then None else Some (unbox<string> value)
            | None -> None

let private parseSource source options registration : ParsedProgramHandle =
    let rootDirectory = optionString "rootDirectory" "/" options
    let entryFile = optionString "entryFile" "/main.fss" options
    let resolver = resolverFromOptions options
    let program =
        IncludeResolver.parseProgramFromSourceWithIncludesResolver
            rootDirectory
            entryFile
            source
            resolver
    { kind = "program"; program = program; registration = registration }

let private isCallable value =
    match value with
    | VClosure _
    | VExternal _
    | VUnionCtor _ -> true
    | _ -> false

let private declaredExportedNames (program: TypeInfer.TypedProgram) =
    program
    |> List.collect (function
        | TypeInfer.TSLet(name, _, _, _, isExported, _) when isExported -> [ name ]
        | TypeInfer.TSLetRecGroup(bindings, isExported, _) when isExported -> bindings |> List.map (fun (name, _, _, _) -> name)
        | _ -> [])

let private hasExpression statements =
    statements
    |> List.exists (function
        | SExpr _ -> true
        | _ -> false)

let private loadTyped typed registration =
    let state = Eval.evalProgramWithRegistrationState registration typed
    let exportedNames = declaredExportedNames typed |> List.distinct |> List.sort
    let functionNames =
        exportedNames
        |> List.filter (fun name ->
            match state.Env.TryFind name with
            | Some value -> isCallable value
            | None -> false)
    let functionSet = functionNames |> Set.ofList
    let functions =
        functionNames
        |> List.choose (fun name -> state.Env.TryFind name |> Option.map (fun value -> name, value))
        |> Map.ofList
    let valueNames =
        exportedNames
        |> List.filter (fun name ->
            match state.Env.TryFind name with
            | Some value -> not (isCallable value)
            | None -> false)
    let valueSet = valueNames |> Set.ofList
    let values =
        valueNames
        |> List.choose (fun name -> state.Env.TryFind name |> Option.map (fun value -> name, value))
        |> Map.ofList
    { kind = "loaded"
      typeDefs = state.TypeDefs
      env = state.Env
      exportedFunctionNames = functionNames
      exportedFunctionSet = functionSet
      exportedFunctions = functions
      exportedValueNames = valueNames
      exportedValueSet = valueSet
      exportedValues = values
      lastValue = state.LastValue
      registration = registration
      exportedFunctionTypes =
        typed |> List.collect (function
            | TypeInfer.TSLet (name, _, t, _, true, _) -> [name, t]
            | TypeInfer.TSLetRecGroup (bindings, true, _) -> bindings |> List.map (fun (name, _, t, _) -> name, t)
            | _ -> []) |> Map.ofList }

let parse (source: string) (options: obj) : obj =
    withStructuredErrors (fun () ->
        let registration = registrationFromOptions options
        parseSource source options registration |> box)

let infer (program: obj) (externs: obj) : obj =
    withStructuredErrors (fun () ->
        let parsed = unbox<ParsedProgramHandle> program
        let registration = registrationOverride parsed.registration externs
        ({ kind = "typedProgram"
           typedProgram = TypeInfer.inferProgramWithRegistration registration parsed.program
           registration = registration }: TypedProgramHandle)
        |> box)

let evaluate (typedProgram: obj) (externs: obj) : obj =
    withStructuredErrors (fun () ->
        let typed = unbox<TypedProgramHandle> typedProgram
        let registration = registrationOverride typed.registration externs
        Eval.evalProgramWithRegistration registration typed.typedProgram |> valueToTagged)

let load (source: string) (options: obj) : obj =
    withStructuredErrors (fun () ->
        let registration = registrationFromOptions options
        let parsed = parseSource source options registration
        let typed = TypeInfer.inferProgramWithRegistration registration parsed.program
        loadTyped typed registration |> box)

let run (source: string) (options: obj) : obj =
    withStructuredErrors (fun () ->
        let loaded = load source options |> unbox<LoadedScriptHandle>
        valueToTagged loaded.lastValue)

let createSession (options: obj) : obj =
    withStructuredErrors (fun () ->
        let registration = registrationFromOptions options
        ({ kind = "session"
           interactiveSession = Interactive.createWithRegistration registration
           options = options
           registration = registration }: SessionHandle)
        |> box)

let resetSession (session: obj) : obj =
    withStructuredErrors (fun () ->
        let session = unbox<SessionHandle> session
        session.interactiveSession <- Interactive.createWithRegistration session.registration
        box session)

let submit (session: obj) (source: string) : obj =
    withStructuredErrors (fun () ->
        let session = unbox<SessionHandle> session
        let parsed = parseSource source session.options session.registration
        let submission = Interactive.submit session.interactiveSession parsed.program
        session.interactiveSession <- submission.Session
        let state = submission.State
        let submittedHasExpression = hasExpression parsed.program
        createObj
            [ "kind" ==> "session-result"
              "hasValue" ==> submittedHasExpression
              "value" ==> (if submittedHasExpression then valueToTagged state.LastValue else nullObj)
              "text" ==> (if submittedHasExpression then Pretty.valueToString state.LastValue else "")
              "retainedCount" ==> submission.RetainedCount ])

let listFunctions (loaded: obj) : string array =
    withStructuredErrors (fun () ->
        let loaded = unbox<LoadedScriptHandle> loaded
        loaded.exportedFunctionNames |> List.toArray)

let listValues (loaded: obj) : string array =
    withStructuredErrors (fun () ->
        let loaded = unbox<LoadedScriptHandle> loaded
        loaded.exportedValueNames |> List.toArray)

let getValue (loaded: obj) (name: string) : obj =
    withStructuredErrors (fun () ->
        let loaded = unbox<LoadedScriptHandle> loaded
        if not (loaded.exportedValueSet.Contains name) then
            raise (EvalException { Message = $"Unknown exported value '{name}'"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) })
        loaded.exportedValues[name] |> valueToTagged)

let invoke (loaded: obj) (name: string) (args: obj) : obj =
    withStructuredErrors (fun () ->
        let loaded = unbox<LoadedScriptHandle> loaded
        if not (loaded.exportedFunctionSet.Contains name) then
            if loaded.exportedValueSet.Contains name then
                raise (EvalException { Message = $"'{name}' is a value and cannot be invoked"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) })
            else
                raise (EvalException { Message = $"Unknown exported function '{name}'"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) })
        let fnValue = loaded.exportedFunctions[name]
        let values =
            if isNullOrUndefined args then
                []
            else
                args |> unbox<obj array> |> Array.toList |> List.map valueFromJs
        HostInterop.validateCall loaded.typeDefs loaded.exportedFunctionTypes[name] values
            (fun () -> Eval.invokeValue loaded.typeDefs fnValue values) |> valueToTagged)

let formatValue (value: obj) : string =
    withStructuredErrors (fun () -> valueFromJs value |> Pretty.valueToString)

let toJs (value: obj) : obj =
    withStructuredErrors (fun () -> value |> unbox<Value> |> valueToTagged)

let fromJs (value: obj) (_expectedType: obj) : obj =
    withStructuredErrors (fun () -> valueFromJs value |> box)

let T =
    createObj
        [ "unit" ==> TUnit
          "int" ==> TInt
          "float" ==> TFloat
          "bool" ==> TBool
          "string" ==> TString
          "typeToken" ==> TTypeToken
          "named" ==> fun name -> TNamed name
          "var" ==> fun value -> TVar (intFromObj value)
          "list" ==> fun value -> TList (unbox<Type> value)
          "task" ==> fun value -> TTask (unbox<Type> value)
          "option" ==> fun value -> TOption (unbox<Type> value)
          "map" ==> fun value -> TMap(TString, unbox<Type> value)
          "mapOf" ==> fun key value -> TMap(unbox<Type> key, unbox<Type> value)
          "tuple" ==> fun values -> values |> unbox<obj array> |> Array.toList |> List.map unbox<Type> |> TTuple
          "record" ==> fun fields ->
              objectKeys fields
              |> Array.toList
              |> List.map (fun name -> name, getProperty fields name |> unbox<Type>)
              |> Map.ofList
              |> TRecord
          "func" ==> fun left right -> TFun(unbox<Type> left, unbox<Type> right)
          "scheme" ==> fun value -> Forall([], unbox<Type> value)
          "forall" ==> fun vars value ->
              let vars = vars |> unbox<obj array> |> Array.toList |> List.map intFromObj
              Forall(vars, unbox<Type> value) ]

let ``extern`` (definition: obj) : ExternalFunction =
    let fn = buildExternal Map.empty definition
    let copied = createObj [
        "name" ==> fn.Name
        "arity" ==> fn.Arity
        "scheme" ==> fn.Scheme
        "invoke" ==> getRequiredProperty "invoke" definition ] |> freeze
    weakSet externDefinitions (box fn) copied
    fn

let private fieldTypes fields =
    if isNullOrUndefined fields || jsTypeOf fields <> "object" || isArray fields then failwith "fields must be an object of type descriptors"
    objectKeys fields |> Array.toList
    |> List.map (fun name ->
        if String.IsNullOrWhiteSpace name then failwith "Visible field names must not be empty"
        name, getProperty fields name |> unbox<Type>) |> Map.ofList

let private describeType definition =
    let name = getRequiredProperty "name" definition |> unbox<string>
    let fields = getRequiredProperty "fields" definition |> fieldTypes
    let constructible =
        match tryGetProperty "scriptConstructible" definition with
        | None -> false
        | Some value when jsTypeOf value = "boolean" -> unbox<bool> value
        | _ -> failwith "scriptConstructible must be a boolean"
    let descriptor = { Name = name; Fields = fields; HostManaged = not constructible }
    HostInterop.typeDefinitions { HostInterop.empty with Types = [descriptor] } [] |> ignore
    createObj [ "name" ==> name
                "fields" ==> (fields |> Map.toList |> List.map (fun (k, v) -> k ==> v) |> propertiesObject |> deepFreeze)
                "scriptConstructible" ==> constructible ] |> freeze

let private describeUnion definition =
    let name = getRequiredProperty "name" definition |> unbox<string>
    let cases = getRequiredProperty "cases" definition
    let copied = objectKeys cases |> Array.map (fun k -> k ==> getProperty cases k) |> Array.toList |> propertiesObject |> freeze
    let descriptor : HostUnionDescriptor =
        { Name = name
          Cases = objectKeys copied |> Array.toList |> List.map (fun k ->
              let t = getProperty copied k
              k, if isNullOrUndefined t then None else Some (unbox<Type> t)) |> Map.ofList }
    HostInterop.typeDefinitions { HostInterop.empty with Unions = [descriptor] } [] |> ignore
    deepFreeze copied |> ignore
    createObj [ "name" ==> name; "cases" ==> copied ] |> freeze

/// Build a registration from explicit JavaScript descriptors and extern functions.
let registration (definition: obj) : obj =
    withStructuredErrors (fun () ->
        let entries name = tryGetProperty name definition |> Option.map unbox<obj array> |> Option.defaultValue [||] |> Array.toList
        let types : HostTypeDescriptor list = entries "types" |> List.map (fun d ->
            let d = describeType d
            { Name = getProperty d "name" |> unbox<string>
              Fields = getProperty d "fields" |> fieldTypes
              HostManaged = not (getProperty d "scriptConstructible" |> unbox<bool>) })
        let unions : HostUnionDescriptor list = entries "unions" |> List.map (fun d ->
            let name = getRequiredProperty "name" d |> unbox<string>
            let cases = getRequiredProperty "cases" d
            { Name = name
              Cases = objectKeys cases |> Array.toList |> List.map (fun k ->
                  let t = getProperty cases k
                  k, if isNullOrUndefined t then None else Some (unbox<Type> t)) |> Map.ofList })
        let functions = entries "functions" |> List.map unbox<ExternalFunction>
        let value = validateRegistration { Types = types; Unions = unions; Functions = functions }
        let handle = createObj [ "kind" ==> "registration" ] |> freeze
        weakSet registrations handle (box value)
        handle)

let private validateHostValue definitions expected value =
    HostInterop.validateCall definitions (TFun(expected, TUnit)) [value] (fun () -> VUnit) |> ignore

let private wrapHost registrationHandle typeName fields payload =
    let registration = registrationFromHandle registrationHandle
    let descriptor =
        registration.Types |> List.tryFind (fun d -> d.Name = typeName)
        |> Option.defaultWith (fun () -> failwith $"Unknown registered type '{typeName}'")
    let values = objectKeys fields |> Array.toList |> List.map (fun k -> k, getProperty fields k |> valueFromJs) |> Map.ofList
    if Set.ofSeq values.Keys <> Set.ofSeq descriptor.Fields.Keys then failwith $"Visible fields do not match '{typeName}'"
    let value =
        if descriptor.HostManaged then HostInterop.create registration typeName values payload
        else
            if not (isNullOrUndefined payload) then failwith "Script-constructible types cannot carry hidden payloads"
            let value = VRecord values
            validateHostValue (HostInterop.typeDefinitions registration []) (TNamed typeName) value
            value
    validateHostValue (HostInterop.typeDefinitions registration []) (TNamed typeName) value
    valueToTagged value

let private unwrapHost registrationHandle typeName value =
    let registration = registrationFromHandle registrationHandle
    let definitions = HostInterop.typeDefinitions registration []
    if not (registration.Types |> List.exists (fun d -> d.Name = typeName)) then failwith $"Unknown registered type '{typeName}'"
    let value = valueFromJs value
    validateHostValue definitions (TNamed typeName) value
    match value with
    | VHostManaged host -> host.Payload
    | VRecord fields ->
        let descriptor = registration.Types |> List.find (fun d -> d.Name = typeName)
        if fields.Count <> descriptor.Fields.Count then failwith $"Visible fields do not match '{typeName}'"
        fields |> Map.toList |> List.map (fun (k, v) -> k ==> valueToTagged v) |> propertiesObject
    | _ -> failwith $"Expected registered type '{typeName}'"

let private wrapUnion registrationHandle typeName caseName payload =
    let registration = registrationFromHandle registrationHandle
    let descriptor = registration.Unions |> List.tryFind (fun d -> d.Name = typeName)
    if descriptor.IsNone then failwith $"Unknown registered union '{typeName}'"
    let value = VUnionCase(typeName, caseName, if isNullOrUndefined payload then None else Some (valueFromJs payload))
    validateHostValue (HostInterop.typeDefinitions registration []) (TNamed typeName) value
    valueToTagged value

let private unwrapUnion registrationHandle typeName value =
    let registration = registrationFromHandle registrationHandle
    if not (registration.Unions |> List.exists (fun d -> d.Name = typeName)) then failwith $"Unknown registered union '{typeName}'"
    let value = valueFromJs value
    validateHostValue (HostInterop.typeDefinitions registration []) (TNamed typeName) value
    valueToTagged value

/// JavaScript descriptors, snapshot wrapping, and validated payload retrieval.
let HostTypes =
    createObj [
        "describe" ==> fun definition -> withStructuredErrors (fun () -> describeType definition)
        "describeUnion" ==> fun definition -> withStructuredErrors (fun () -> describeUnion definition)
        "wrap" ==> fun registration name fields payload -> withStructuredErrors (fun () -> wrapHost registration name fields payload)
        "unwrap" ==> fun registration name value -> withStructuredErrors (fun () -> unwrapHost registration name value)
        "wrapUnion" ==> fun registration name caseName payload -> withStructuredErrors (fun () -> wrapUnion registration name caseName payload)
        "unwrapUnion" ==> fun registration name value -> withStructuredErrors (fun () -> unwrapUnion registration name value)
        "fields" ==> fun value -> withStructuredErrors (fun () ->
            match valueFromJs value with
            | HostInterop.RecordFields fields -> fields |> Map.toList |> List.map (fun (k, v) -> k ==> valueToTagged v) |> propertiesObject
            | _ -> failwith "Expected a record or host value") ] |> freeze

/// Derive a closed curried scheme and arity from explicit JavaScript parameter/result types.
let HostFunctions =
    createObj [ "create" ==> fun definition -> withStructuredErrors (fun () ->
        let parameters = getRequiredProperty "parameters" definition |> unbox<Type array> |> Array.toList
        if parameters.IsEmpty then failwith "Use parameters: [T.unit] for a parameterless script function"
        let result = getRequiredProperty "result" definition |> unbox<Type>
        let signature = List.foldBack (fun argument result -> TFun(argument, result)) parameters result
        if not (Types.ftvType signature |> Set.isEmpty) then failwith "HostFunctions.create requires closed types; use extern for polymorphic schemes"
        ``extern`` (createObj [
            "name" ==> getRequiredProperty "name" definition
            "arity" ==> parameters.Length
            "scheme" ==> Forall([], signature)
            "invoke" ==> getRequiredProperty "invoke" definition ])) ] |> freeze
