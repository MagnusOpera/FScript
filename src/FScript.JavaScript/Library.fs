module FScript.JavaScript

open System
open System.Globalization
open Fable.Core
open Fable.Core.JsInterop
open FScript.Language

type ParsedProgramHandle =
    { kind: string
      program: FScript.Language.Program
      externs: ExternalFunction list }

type TypedProgramHandle =
    { kind: string
      typedProgram: TypeInfer.TypedProgram
      externs: ExternalFunction list }

type LoadedScriptHandle =
    { kind: string
      typeDefs: Map<string, Type>
      env: Env
      exportedFunctionNames: string list
      exportedFunctionSet: Set<string>
      exportedFunctions: Map<string, Value>
      exportedValueNames: string list
      exportedValueSet: Set<string>
      exportedValues: Map<string, Value>
      lastValue: Value
      externs: ExternalFunction list }

type SessionHandle =
    { kind: string
      mutable retainedProgram: FScript.Language.Program
      options: obj
      externs: ExternalFunction list }

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

let rec private valueToTagged value : obj =
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
    | VRecord fields ->
        let fieldObject =
            fields
            |> Map.toList
            |> List.map (fun (name, fieldValue) -> name ==> valueToTagged fieldValue)
            |> createObj
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
        createObj
            [ "kind" ==> "opaque"
              "valueType" ==> "union-constructor"
              "typeName" ==> typeName
              "caseName" ==> caseName ]
    | VTypeToken value ->
        createObj [ "kind" ==> "type"; "name" ==> Types.typeToString value ]
    | VTask _ ->
        createObj [ "kind" ==> "opaque"; "valueType" ==> "task" ]
    | VClosure _ ->
        createObj [ "kind" ==> "opaque"; "valueType" ==> "function" ]
    | VExternal (externalFunction, existingArgs) ->
        createObj
            [ "kind" ==> "opaque"
              "valueType" ==> "external"
              "name" ==> externalFunction.Name
              "applied" ==> existingArgs.Length
              "arity" ==> externalFunction.Arity ]

let rec private valueFromJs value : Value =
    if isNullOrUndefined value then
        VUnit
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
                | "unit" -> VUnit
                | "int" -> getRequiredProperty "value" value |> int64FromObj |> VInt
                | "float" -> getRequiredProperty "value" value |> unbox<float> |> VFloat
                | "bool" -> getRequiredProperty "value" value |> unbox<bool> |> VBool
                | "string" -> getRequiredProperty "value" value |> unbox<string> |> VString
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
                            | _ -> failwith "Map key must be string or int"
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

let private externsFromOptions options =
    match tryGetProperty "externs" options with
    | Some value -> externsFromArray value
    | None -> []

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

let private parseSource source options externs : ParsedProgramHandle =
    let rootDirectory = optionString "rootDirectory" "/" options
    let entryFile = optionString "entryFile" "/main.fss" options
    let resolver = resolverFromOptions options
    let program =
        IncludeResolver.parseProgramFromSourceWithIncludesResolver
            rootDirectory
            entryFile
            source
            resolver
    { kind = "program"; program = program; externs = externs }

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

let private retainedStatements statements =
    statements
    |> List.filter (function
        | SExpr _ -> false
        | _ -> true)

let private loadTyped typed externs =
    let state = Eval.evalProgramWithExternsState externs typed
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
      externs = externs }

let parse (source: string) (options: obj) : obj =
    withStructuredErrors (fun () ->
        let externs = externsFromOptions options
        parseSource source options externs |> box)

let infer (program: obj) (externs: obj) : obj =
    withStructuredErrors (fun () ->
        let parsed = unbox<ParsedProgramHandle> program
        let externs =
            if isNullOrUndefined externs then parsed.externs else externsFromArray externs
        ({ kind = "typedProgram"
           typedProgram = TypeInfer.inferProgramWithExterns externs parsed.program
           externs = externs }: TypedProgramHandle)
        |> box)

let evaluate (typedProgram: obj) (externs: obj) : obj =
    withStructuredErrors (fun () ->
        let typed = unbox<TypedProgramHandle> typedProgram
        let externs =
            if isNullOrUndefined externs then typed.externs else externsFromArray externs
        Eval.evalProgramWithExterns externs typed.typedProgram |> valueToTagged)

let load (source: string) (options: obj) : obj =
    withStructuredErrors (fun () ->
        let externs = externsFromOptions options
        let parsed = parseSource source options externs
        let typed = TypeInfer.inferProgramWithExterns externs parsed.program
        loadTyped typed externs |> box)

let run (source: string) (options: obj) : obj =
    withStructuredErrors (fun () ->
        let loaded = load source options |> unbox<LoadedScriptHandle>
        valueToTagged loaded.lastValue)

let createSession (options: obj) : obj =
    withStructuredErrors (fun () ->
        ({ kind = "session"
           retainedProgram = []
           options = options
           externs = externsFromOptions options }: SessionHandle)
        |> box)

let resetSession (session: obj) : obj =
    withStructuredErrors (fun () ->
        let session = unbox<SessionHandle> session
        session.retainedProgram <- []
        box session)

let submit (session: obj) (source: string) : obj =
    withStructuredErrors (fun () ->
        let session = unbox<SessionHandle> session
        let parsed = parseSource source session.options session.externs
        let candidate = session.retainedProgram @ parsed.program
        let typed = TypeInfer.inferProgramWithExterns session.externs candidate
        let state = Eval.evalProgramWithExternsState session.externs typed
        let submittedHasExpression = hasExpression parsed.program
        session.retainedProgram <- session.retainedProgram @ retainedStatements parsed.program
        createObj
            [ "kind" ==> "session-result"
              "hasValue" ==> submittedHasExpression
              "value" ==> (if submittedHasExpression then valueToTagged state.LastValue else nullObj)
              "text" ==> (if submittedHasExpression then Pretty.valueToString state.LastValue else "")
              "retainedCount" ==> session.retainedProgram.Length ])

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
        Eval.invokeValue loaded.typeDefs fnValue values |> valueToTagged)

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
          "var" ==> fun value -> TVar (intFromObj value)
          "list" ==> fun value -> TList (unbox<Type> value)
          "task" ==> fun value -> TTask (unbox<Type> value)
          "option" ==> fun value -> TOption (unbox<Type> value)
          "map" ==> fun value -> TMap(TString, unbox<Type> value)
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
    withStructuredErrors (fun () ->
        let name = getRequiredProperty "name" definition |> unbox<string>
        let arity = getRequiredProperty "arity" definition |> intFromObj
        let scheme = getRequiredProperty "scheme" definition |> unbox<Scheme>
        let invoke = getRequiredProperty "invoke" definition
        { Name = name
          Arity = arity
          Scheme = scheme
          Impl =
            fun _ args ->
                args
                |> List.map valueToTagged
                |> List.toArray
                |> box
                |> callOne invoke
                |> valueFromJs })
