namespace FScript.Compiler

open System.Threading
open FScript.Language

type InterpreterContext =
    { ScriptName: string option
      Arguments: string list }

type LirValue =
    | VUnit
    | VInt of int64
    | VFloat of float
    | VBool of bool
    | VString of string
    | VTuple of LirValue list
    | VList of LirValue list
    | VRecord of Map<string, LirValue>
    | VOption of LirValue option
    | VUnion of string * string * LirValue option
    | VMap of (LirValue * LirValue) list
    | VIntrinsic of string * LirValue list
    | VExternal of string * int * LirValue list
    | VTypeToken of Type
    | VRuntimeContext
    | VTargetImport of string * string * int * LirValue list
    | VClosure of Lir.SymbolId * string * Lir.Expression * Map<Lir.SymbolId, LirValue> ref

type ExecutionResult =
    { Value: LirValue
      Stdout: string list }

module LirInterpreter =
    let private currentOutput = AsyncLocal<ResizeArray<string>>()
    let private constantValue = function
        | Lir.Unit -> VUnit | Lir.Int value -> VInt value | Lir.Float value -> VFloat value
        | Lir.Bool value -> VBool value | Lir.String value -> VString value

    let rec private equal left right =
        match left, right with
        | VUnit, VUnit -> true
        | VInt a, VInt b -> a = b
        | VFloat a, VFloat b -> a = b
        | VBool a, VBool b -> a = b
        | VString a, VString b -> a = b
        | VTuple a, VTuple b | VList a, VList b -> a.Length = b.Length && List.forall2 equal a b
        | VRecord a, VRecord b -> a.Count = b.Count && (a |> Map.forall (fun key value -> b |> Map.tryFind key |> Option.exists (equal value)))
        | VOption None, VOption None -> true
        | VOption(Some a), VOption(Some b) -> equal a b
        | VUnion(aType, aCase, aValue), VUnion(bType, bCase, bValue) -> aType = bType && aCase = bCase && equal (VOption aValue) (VOption bValue)
        | VMap a, VMap b -> a.Length = b.Length && List.forall2 (fun (ak, av) (bk, bv) -> equal ak bk && equal av bv) a b
        | VTypeToken a, VTypeToken b -> a = b
        | _ -> false

    let rec private formatValue = function
        | VUnit -> "()" | VInt value -> string value | VFloat value -> value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
        | VBool value -> value.ToString().ToLowerInvariant() | VString value -> value
        | VTuple values -> "(" + (values |> List.map formatValue |> String.concat ", ") + ")"
        | VList values -> "[" + (values |> List.map formatValue |> String.concat ";") + "]"
        | VRecord fields ->
            let rendered = fields |> Map.toList |> List.map (fun (name, value) -> name + " = " + formatValue value) |> String.concat "; "
            "{ " + rendered + " }"
        | VOption None -> "None" | VOption(Some value) -> $"Some {formatValue value}"
        | VUnion(_, caseName, None) -> caseName | VUnion(_, caseName, Some value) -> $"{caseName} {formatValue value}"
        | VMap entries ->
            entries |> List.map (fun (key, value) -> $"[{formatValue key}] = {formatValue value}") |> String.concat "; " |> fun value -> "{ " + value + " }"
        | VClosure _ -> "<function>"
        | VIntrinsic _ -> "<intrinsic>"
        | VExternal _ -> "<external>"
        | VTypeToken value -> $"<type {Types.typeToString value}>"
        | VRuntimeContext -> "<runtime-context>"
        | VTargetImport _ -> "<target-import>"

    let private fail code span message =
        Error [ { Code = code; Stage = Interpreter; Message = message; Span = Some span } ]

    let private intrinsicArity name =
        let scheme =
            FScript.Language.BuiltinSignatures.builtinSchemes
            |> Map.tryFind name
            |> Option.orElseWith (fun () -> RuntimeIntrinsics.schemes |> Map.tryFind name)
            |> Option.defaultWith (fun () -> invalidArg "name" $"Unknown intrinsic '{name}'.")
        let (Forall(_, valueType)) = scheme
        let rec count = function TFun(_, result) -> 1 + count result | _ -> 0
        count valueType

    let rec private arityFromType = function TFun(_, result) -> 1 + arityFromType result | _ -> 0

    let private invokeIntrinsic apply span name arguments =
        let unsupported () = fail "FSI3020" span $"Intrinsic '{name}' is not yet executable in LIR."
        let expectBool = function Ok(VBool value) -> Ok value | Ok _ -> fail "FSI3022" span $"Intrinsic '{name}' callback must return bool." | Error errors -> Error errors
        let rec mapCallback callback acc = function
            | [] -> Ok(List.rev acc)
            | value :: rest -> apply callback value |> Result.bind (fun mapped -> mapCallback callback (mapped :: acc) rest)
        let rec filterCallback callback acc = function
            | [] -> Ok(List.rev acc)
            | value :: rest -> apply callback value |> expectBool |> Result.bind (fun keep -> filterCallback callback (if keep then value :: acc else acc) rest)
        let mapSet key value entries =
            let rec loop acc = function
                | [] -> List.rev ((key, value) :: acc)
                | (existing, _) :: rest when equal existing key -> List.rev acc @ ((key, value) :: rest)
                | entry :: rest -> loop (entry :: acc) rest
            loop [] entries
        match name, arguments with
        | "Console.writeLine", [ VString value ] -> currentOutput.Value.Add(value); Ok VUnit
        | "ignore", [ _ ] -> Ok VUnit
        | "Int.tryParse", [ VString value ] -> match System.Int64.TryParse value with true, parsed -> Ok(VOption(Some(VInt parsed))) | _ -> Ok(VOption None)
        | "Float.tryParse", [ VString value ] -> match System.Double.TryParse(value, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with true, parsed -> Ok(VOption(Some(VFloat parsed))) | _ -> Ok(VOption None)
        | "Bool.tryParse", [ VString value ] -> match System.Boolean.TryParse value with true, parsed -> Ok(VOption(Some(VBool parsed))) | _ -> Ok(VOption None)
        | "Int.toString", [ VInt value ] -> Ok(VString(string value))
        | "Float.toString", [ VFloat value ] -> Ok(VString(value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)))
        | "Bool.toString", [ VBool value ] -> Ok(VString(value.ToString().ToLowerInvariant()))
        | "String.replace", [ VString oldValue; VString newValue; VString value ] -> Ok(VString(value.Replace(oldValue, newValue)))
        | "String.indexOf", [ VString needle; VString value ] -> let index = value.IndexOf(needle, System.StringComparison.Ordinal) in Ok(VOption(if index < 0 then None else Some(VInt(int64 index))))
        | "String.toLower", [ VString value ] -> Ok(VString(value.ToLowerInvariant()))
        | "String.toUpper", [ VString value ] -> Ok(VString(value.ToUpperInvariant()))
        | "String.substring", [ VInt start; VInt length; VString value ] when start >= 0L && length >= 0L && start + length <= int64 value.Length -> Ok(VOption(Some(VString(value.Substring(int start, int length)))))
        | "String.substring", [ VInt _; VInt _; VString _ ] -> Ok(VOption None)
        | "String.concat", [ VString separator; VList values ] -> values |> List.choose (function VString value -> Some value | _ -> None) |> String.concat separator |> VString |> Ok
        | "String.split", [ VString separator; VString value ] -> value.Split([| separator |], System.StringSplitOptions.None) |> Array.toList |> List.map VString |> VList |> Ok
        | "String.endsWith", [ VString suffix; VString value ] -> Ok(VBool(value.EndsWith(suffix, System.StringComparison.Ordinal)))
        | "List.length", [ VList values ] -> Ok(VInt(int64 values.Length))
        | "List.empty", [] -> Ok(VList [])
        | "List.map", [ callback; VList values ] -> mapCallback callback [] values |> Result.map VList
        | "List.iter", [ callback; VList values ] -> mapCallback callback [] values |> Result.map (fun _ -> VUnit)
        | "List.choose", [ callback; VList values ] ->
            mapCallback callback [] values |> Result.bind (fun mapped ->
                mapped |> List.fold (fun state value ->
                    match state, value with
                    | Ok acc, VOption(Some item) -> Ok(item :: acc)
                    | Ok acc, VOption None -> Ok acc
                    | Ok _, _ -> fail "FSI3022" span "List.choose callback must return option."
                    | Error errors, _ -> Error errors) (Ok []) |> Result.map (List.rev >> VList))
        | "List.collect", [ callback; VList values ] ->
            mapCallback callback [] values |> Result.bind (fun mapped ->
                mapped |> List.fold (fun state value ->
                    match state, value with Ok acc, VList items -> Ok(acc @ items) | Ok _, _ -> fail "FSI3022" span "List.collect callback must return list." | Error errors, _ -> Error errors) (Ok []) |> Result.map VList)
        | "List.exists", [ callback; VList values ] ->
            let rec loop = function [] -> Ok(VBool false) | value :: rest -> apply callback value |> expectBool |> Result.bind (fun found -> if found then Ok(VBool true) else loop rest)
            loop values
        | "List.rev", [ VList values ] -> Ok(VList(List.rev values))
        | "List.distinct", [ VList values ] -> values |> List.fold (fun acc value -> if List.exists (equal value) acc then acc else acc @ [ value ]) [] |> VList |> Ok
        | "List.fold", [ callback; state; VList values ] ->
            values |> List.fold (fun result value -> result |> Result.bind (fun current -> apply callback current |> Result.bind (fun partial -> apply partial value))) (Ok state)
        | "List.filter", [ callback; VList values ] -> filterCallback callback [] values |> Result.map VList
        | "List.tryFind", [ callback; VList values ] ->
            let rec loop = function [] -> Ok(VOption None) | value :: rest -> apply callback value |> expectBool |> Result.bind (fun found -> if found then Ok(VOption(Some value)) else loop rest)
            loop values
        | "List.tryGet", [ callback; VList values ] ->
            let rec loop index = function [] -> Ok(VOption None) | value :: rest -> apply callback value |> expectBool |> Result.bind (fun found -> if found then Ok(VOption(Some(VInt index))) else loop (index + 1L) rest)
            loop 0L values
        | "List.tryItem", [ VInt index; VList values ] -> if index < 0L || index >= int64 values.Length then Ok(VOption None) else Ok(VOption(Some values.[int index]))
        | "List.tryHead", [ VList values ] -> Ok(VOption(List.tryHead values))
        | "List.tail", [ VList(_ :: tail) ] -> Ok(VList tail)
        | "List.append", [ VList left; VList right ] -> Ok(VList(left @ right))
        | "List.contains", [ value; VList values ] -> Ok(VBool(List.exists (equal value) values))
        | "Option.isNone", [ VOption value ] -> Ok(VBool value.IsNone)
        | "Option.isSome", [ VOption value ] -> Ok(VBool value.IsSome)
        | "Option.defaultValue", [ fallback; VOption value ] -> Ok(Option.defaultValue fallback value)
        | "Option.defaultWith", [ fallback; VOption(Some value) ] -> Ok value
        | "Option.defaultWith", [ fallback; VOption None ] -> apply fallback VUnit
        | "Option.map", [ callback; VOption(Some value) ] -> apply callback value |> Result.map (Some >> VOption)
        | "Option.map", [ _; VOption None ] -> Ok(VOption None)
        | "Map.empty", [] -> Ok(VMap [])
        | "Map.count", [ VMap entries ] -> Ok(VInt(int64 entries.Length))
        | "Map.containsKey", [ key; VMap entries ] -> Ok(VBool(entries |> List.exists (fst >> equal key)))
        | "Map.tryGet", [ key; VMap entries ] -> Ok(VOption(entries |> List.tryPick (fun (itemKey, value) -> if equal itemKey key then Some value else None)))
        | "Map.add", [ key; value; VMap entries ] -> Ok(VMap(mapSet key value entries))
        | "Map.ofList", [ VList pairs ] ->
            pairs |> List.fold (fun state pair -> match state, pair with Ok entries, VTuple [ key; value ] -> Ok(mapSet key value entries) | Ok _, _ -> fail "FSI3022" span "Map.ofList expects key/value tuples." | Error errors, _ -> Error errors) (Ok []) |> Result.map VMap
        | "Map.fold", [ callback; state; VMap entries ] ->
            entries |> List.fold (fun result (key, value) -> result |> Result.bind (fun current -> apply callback current |> Result.bind (fun withKey -> apply withKey key |> Result.bind (fun withValue -> apply withValue value)))) (Ok state)
        | "Map.filter", [ callback; VMap entries ] ->
            let rec loop acc = function [] -> Ok(VMap(List.rev acc)) | (key, value as entry) :: rest -> apply callback key |> Result.bind (fun partial -> apply partial value |> expectBool |> Result.bind (fun keep -> loop (if keep then entry :: acc else acc) rest))
            loop [] entries
        | "Map.choose", [ callback; VMap entries ] ->
            let rec loop acc = function
                | [] -> Ok(VMap(List.rev acc))
                | (key, value) :: rest -> apply callback key |> Result.bind (fun partial -> apply partial value |> Result.bind (function VOption(Some chosen) -> loop ((key, chosen) :: acc) rest | VOption None -> loop acc rest | _ -> fail "FSI3022" span "Map.choose callback must return option."))
            loop [] entries
        | "Map.map", [ callback; VMap entries ] ->
            let rec loop acc = function [] -> Ok(VMap(List.rev acc)) | (key, value) :: rest -> apply callback value |> Result.bind (fun mapped -> loop ((key, mapped) :: acc) rest)
            loop [] entries
        | "Map.iter", [ callback; VMap entries ] ->
            let rec loop = function [] -> Ok VUnit | (key, value) :: rest -> apply callback key |> Result.bind (fun partial -> apply partial value |> Result.bind (fun _ -> loop rest))
            loop entries
        | "Map.remove", [ key; VMap entries ] -> Ok(VMap(entries |> List.filter (fst >> equal key >> not)))
        | _ -> unsupported ()

    let rec private matchPattern (pattern: Lir.Pattern) value =
        let combine left right =
            match left, right with
            | Some a, Some b ->
                b |> Map.fold (fun state key item ->
                    state |> Option.bind (fun env ->
                        match env |> Map.tryFind key with
                        | Some existing when not (equal existing item) -> None
                        | _ -> Some(Map.add key item env))) (Some a)
            | _ -> None
        let rec matchesType expected item =
            match expected, item with
            | TUnit, VUnit | TInt, VInt _ | TFloat, VFloat _ | TBool, VBool _ | TString, VString _ | TTypeToken, VTypeToken _ -> true
            | TTuple types, VTuple values -> types.Length = values.Length && List.forall2 matchesType types values
            | TList itemType, VList values -> values |> List.forall (matchesType itemType)
            | TOption _, VOption None -> true
            | TOption itemType, VOption(Some value) -> matchesType itemType value
            | TRecord fields, VRecord values -> fields |> Map.forall (fun name fieldType -> values |> Map.tryFind name |> Option.exists (matchesType fieldType))
            | TMap(keyType, valueType), VMap entries -> entries |> List.forall (fun (key, value) -> matchesType keyType key && matchesType valueType value)
            | TUnion(typeName, _), VUnion(valueType, _, _) | TNamed typeName, VUnion(valueType, _, _) -> typeName = valueType
            | TNamed _, VRecord _ -> true
            | TFun _, (VClosure _ | VIntrinsic _ | VExternal _) -> true
            | TVar _, _ -> true
            | _ -> false
        match pattern.Node, value with
        | Lir.Wildcard, _ -> Some Map.empty
        | Lir.Binding(id, _), item -> Some(Map.ofList [ id, item ])
        | Lir.LiteralPattern literal, item when equal (constantValue literal) item -> Some Map.empty
        | Lir.EmptyList, VList [] -> Some Map.empty
        | Lir.ConsPattern(head, tail), VList(headValue :: tailValues) ->
            combine (matchPattern head headValue) (matchPattern tail (VList tailValues))
        | Lir.TuplePattern patterns, VTuple values when patterns.Length = values.Length ->
            List.zip patterns values |> List.fold (fun state (itemPattern, itemValue) -> combine state (matchPattern itemPattern itemValue)) (Some Map.empty)
        | Lir.RecordPattern patterns, VRecord values ->
            patterns |> List.fold (fun state (name, itemPattern) ->
                combine state (values |> Map.tryFind name |> Option.bind (matchPattern itemPattern))) (Some Map.empty)
        | Lir.MapPattern(clauses, tailPattern), VMap entries ->
            let rec matchClauses bindings remaining = function
                | [] ->
                    match tailPattern with
                    | Some tail -> combine (Some bindings) (matchPattern tail (VMap remaining))
                    | None when not clauses.IsEmpty || remaining.IsEmpty -> Some bindings
                    | None -> None
                | (keyPattern, valuePattern) :: rest ->
                    remaining |> List.indexed |> List.tryPick (fun (index, (key, item)) ->
                        combine (Some bindings) (matchPattern keyPattern key)
                        |> Option.bind (fun withKey -> combine (Some withKey) (matchPattern valuePattern item))
                        |> Option.bind (fun withValue ->
                            let next = remaining |> List.indexed |> List.choose (fun (position, entry) -> if position = index then None else Some entry)
                            matchClauses withValue next rest))
            matchClauses Map.empty entries clauses
        | Lir.SomePattern itemPattern, VOption(Some item) -> matchPattern itemPattern item
        | Lir.NonePattern, VOption None -> Some Map.empty
        | Lir.UnionPattern(typeName, caseName, payloadPattern), VUnion(valueType, valueCase, payload) when typeName = valueType && caseName = valueCase ->
            match payloadPattern, payload with None, None -> Some Map.empty | Some pattern, Some value -> matchPattern pattern value | _ -> None
        | Lir.TypePattern expected, item when matchesType expected item -> Some Map.empty
        | _ -> None

    let rec private evaluate environment (expression: Lir.Expression) =
        let binary work =
            match expression.Node with
            | Lir.Operation(_, [ left; right ]) ->
                match evaluate environment left, evaluate environment right with
                | Ok a, Ok b -> work a b
                | Error a, Error b -> Error(a @ b)
                | Error errors, _ | _, Error errors -> Error errors
            | _ -> fail "FSI3002" expression.Span "Invalid primitive arity."
        match expression.Node with
        | Lir.Constant value ->
            Ok(constantValue value)
        | Lir.Load id ->
            environment |> Map.tryFind id |> Option.map Ok
            |> Option.defaultWith (fun () -> fail "FSI3001" expression.Span (sprintf "Unknown LIR symbol %%%d." (Lir.symbolNumber id)))
        | Lir.MakeClosure(id, name, _, body) -> Ok(VClosure(id, name, body, ref environment))
        | Lir.CallClosure(fn, argument) ->
            match evaluate environment fn, evaluate environment argument with
            | Ok fnValue, Ok value -> applyValue expression.Span fnValue value
            | Error a, Error b -> Error(a @ b)
            | Error errors, _ | _, Error errors -> Error errors
        | Lir.Branch(condition, whenTrue, whenFalse) ->
            match evaluate environment condition with
            | Ok(VBool true) -> evaluate environment whenTrue
            | Ok(VBool false) -> evaluate environment whenFalse
            | Ok _ -> fail "FSI3004" expression.Span "Branch condition is not Boolean."
            | Error errors -> Error errors
        | Lir.Operation(op, _) ->
            binary (fun left right ->
                match op, left, right with
                | Lir.IntAdd, VInt a, VInt b -> Ok(VInt(a + b))
                | Lir.FloatAdd, VFloat a, VFloat b -> Ok(VFloat(a + b))
                | Lir.IntSubtract, VInt a, VInt b -> Ok(VInt(a - b))
                | Lir.FloatSubtract, VFloat a, VFloat b -> Ok(VFloat(a - b))
                | Lir.IntMultiply, VInt a, VInt b -> Ok(VInt(a * b))
                | Lir.FloatMultiply, VFloat a, VFloat b -> Ok(VFloat(a * b))
                | Lir.IntDivide, VInt _, VInt 0L -> fail "FSI3005" expression.Span "Division by zero."
                | Lir.IntDivide, VInt a, VInt b -> Ok(VInt(a / b))
                | Lir.FloatDivide, VFloat a, VFloat b -> Ok(VFloat(a / b))
                | Lir.IntRemainder, VInt _, VInt 0L -> fail "FSI3005" expression.Span "Division by zero."
                | Lir.IntRemainder, VInt a, VInt b -> Ok(VInt(a % b))
                | Lir.StructuralEqual, a, b -> Ok(VBool(equal a b))
                | Lir.NumericLessThan, VInt a, VInt b -> Ok(VBool(a < b))
                | Lir.NumericGreaterThan, VInt a, VInt b -> Ok(VBool(a > b))
                | Lir.NumericLessThanOrEqual, VInt a, VInt b -> Ok(VBool(a <= b))
                | Lir.NumericGreaterThanOrEqual, VInt a, VInt b -> Ok(VBool(a >= b))
                | Lir.BooleanAnd, VBool a, VBool b -> Ok(VBool(a && b))
                | Lir.BooleanOr, VBool a, VBool b -> Ok(VBool(a || b))
                | _ -> fail "FSI3006" expression.Span "Primitive operands do not match their LIR operation.")
        | Lir.Tuple items -> evaluateMany environment items |> Result.map VTuple
        | Lir.List items -> evaluateMany environment items |> Result.map VList
        | Lir.Record fields ->
            fields |> List.map (fun (name, item) -> evaluate environment item |> Result.map (fun value -> name, value))
            |> collect |> Result.map (Map.ofList >> VRecord)
        | Lir.FieldGet(target, name) ->
            match evaluate environment target with
            | Ok(VRecord fields) -> fields |> Map.tryFind name |> Option.map Ok |> Option.defaultWith (fun () -> fail "FSI3007" expression.Span $"Record field '{name}' was not found.")
            | Ok _ -> fail "FSI3007" expression.Span "Field access requires a record."
            | Error errors -> Error errors
        | Lir.SomeValue item -> evaluate environment item |> Result.map (Some >> VOption)
        | Lir.NoneValue -> Ok(VOption None)
        | Lir.Cons(head, tail) ->
            match evaluate environment head, evaluate environment tail with
            | Ok h, Ok(VList t) -> Ok(VList(h :: t)) | Ok _, Ok _ -> fail "FSI3008" expression.Span "Cons tail must be a list."
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | Lir.Append(left, right) ->
            match evaluate environment left, evaluate environment right with
            | Ok(VList l), Ok(VList r) -> Ok(VList(l @ r)) | Ok _, Ok _ -> fail "FSI3009" expression.Span "Append operands must be lists."
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | Lir.Let(id, _, recursive, binding, body) ->
            if recursive then
                match evaluate environment binding with
                | Ok(VClosure(_, _, _, captured) as closure) ->
                    let next = environment |> Map.add id closure
                    captured.Value <- next
                    evaluate next body
                | Ok _ -> fail "FSI3010" expression.Span "Recursive local bindings must be functions."
                | Error errors -> Error errors
            else
                evaluate environment binding |> Result.bind (fun value -> evaluate (environment |> Map.add id value) body)
        | Lir.LetRecursiveGroup(bindings, body) ->
            let evaluated = bindings |> List.map (fun (id, _, _, binding) -> id, evaluate environment binding)
            let errors = evaluated |> List.collect (fun (_, result) -> match result with Error values -> values | Ok _ -> [])
            if not errors.IsEmpty then Error errors
            elif evaluated |> List.exists (fun (_, result) -> match result with Ok(VClosure _) -> false | _ -> true) then
                fail "FSI3010" expression.Span "Recursive local groups must contain only functions."
            else
                let recursiveEnvironment = evaluated |> List.fold (fun env (id, result) -> match result with Ok value -> Map.add id value env | Error _ -> env) environment
                evaluated |> List.iter (fun (_, result) -> match result with Ok(VClosure(_, _, _, captured)) -> captured.Value <- recursiveEnvironment | _ -> ())
                evaluate recursiveEnvironment body
        | Lir.Fail value ->
            match evaluate environment value with
            | Ok(VString message) -> fail "FSI3011" expression.Span message
            | Ok _ -> fail "FSI3011" expression.Span "FScript program raised a non-string value."
            | Error errors -> Error errors
        | Lir.DecisionTree(target, cases) ->
            evaluate environment target |> Result.bind (fun targetValue ->
                let rec run (remaining: Lir.DecisionCase list) =
                    match remaining with
                    | [] -> fail "FSI3012" expression.Span "Match failure."
                    | case :: rest ->
                        match matchPattern case.Pattern targetValue with
                        | None -> run rest
                        | Some bindings ->
                            let caseEnvironment = Map.fold (fun env id value -> Map.add id value env) environment bindings
                            match case.Guard with
                            | None -> evaluate caseEnvironment case.Body
                            | Some guard ->
                                match evaluate caseEnvironment guard with
                                | Ok(VBool true) -> evaluate caseEnvironment case.Body
                                | Ok(VBool false) -> run rest
                                | Ok _ -> fail "FSI3013" guard.Span "Match guard must evaluate to Boolean."
                                | Error errors -> Error errors
                run cases)
        | Lir.Range(startValue, endValue) ->
            match evaluate environment startValue, evaluate environment endValue with
            | Ok(VInt first), Ok(VInt last) ->
                let step = if first <= last then 1L else -1L
                Ok(VList([ first .. step .. last ] |> List.map VInt))
            | Ok _, Ok _ -> fail "FSI3014" expression.Span "Range endpoints must be integers."
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | Lir.RecordUpdate(target, fields) ->
            match evaluate environment target with
            | Ok(VRecord existing) ->
                fields |> List.map (fun (name, item) -> evaluate environment item |> Result.map (fun value -> name, value))
                |> collect |> Result.map (fun updates -> VRecord(List.fold (fun state (name, value) -> Map.add name value state) existing updates))
            | Ok _ -> fail "FSI3015" expression.Span "Record update requires a record."
            | Error errors -> Error errors
        | Lir.IndexGet(target, index) ->
            match evaluate environment target, evaluate environment index with
            | Ok(VList values), Ok(VInt position) ->
                if position < 0L || position >= int64 values.Length then Ok(VOption None)
                else Ok(VOption(Some values.[int position]))
            | Ok(VMap entries), Ok key -> Ok(VOption(entries |> List.tryPick (fun (itemKey, value) -> if equal itemKey key then Some value else None)))
            | Ok _, Ok _ -> fail "FSI3016" expression.Span "Index access requires a list and integer index."
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | Lir.Interpolate parts ->
            parts |> List.map (function Lir.TextPart text -> Ok text | Lir.ValuePart item -> evaluate environment item |> Result.map formatValue)
            |> collect |> Result.map (String.concat "" >> VString)
        | Lir.UnionCase(typeName, caseName, payload) ->
            payload |> Option.map (fun value -> evaluate environment value |> Result.map Some) |> Option.defaultValue (Ok None)
            |> Result.map (fun value -> VUnion(typeName, caseName, value))
        | Lir.Map entries ->
            let add key value values =
                let without = values |> List.filter (fun (existing, _) -> not (equal existing key))
                (key, value) :: without |> List.sortBy (fst >> formatValue)
            let rec build values remaining =
                match remaining with
                | [] -> Ok(VMap values)
                | Lir.KeyValue(key, item) :: rest ->
                    match evaluate environment key, evaluate environment item with
                    | Ok keyValue, Ok itemValue -> build (add keyValue itemValue values) rest
                    | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
                | Lir.Spread item :: rest ->
                    match evaluate environment item with
                    | Ok(VMap spread) ->
                        let merged = spread |> List.fold (fun state (key, value) -> if state |> List.exists (fst >> equal key) then state else add key value state) values
                        build merged rest
                    | Ok _ -> fail "FSI3017" expression.Span "Map spread requires a map."
                    | Error errors -> Error errors
            build [] entries
        | Lir.Intrinsic name ->
            if intrinsicArity name = 0 then invokeIntrinsic (applyValue expression.Span) expression.Span name []
            else Ok(VIntrinsic(name, []))
        | Lir.HostExternal(name, arity) -> Ok(VExternal(name, arity, []))
        | Lir.RuntimeContext -> Ok VRuntimeContext
        | Lir.TargetImport(modulePath, memberName) -> Ok(VTargetImport(modulePath, memberName, arityFromType expression.Type, []))
        | Lir.ForEach(id, _, _, source, body) ->
            match evaluate environment source with
            | Ok(VList values) ->
                let rec loop remaining =
                    match remaining with
                    | [] -> Ok VUnit
                    | value :: tail -> evaluate (environment |> Map.add id value) body |> Result.bind (fun _ -> loop tail)
                loop values
            | Ok _ -> fail "FSI3018" expression.Span "For-loop source must be a list."
            | Error errors -> Error errors
        | Lir.TypeToken tokenType -> Ok(VTypeToken tokenType)

    and private applyValue span fn value =
        match fn with
        | VClosure(parameter, _, body, captured) -> evaluate (captured.Value |> Map.add parameter value) body
        | VIntrinsic(name, existing) ->
            let arguments = existing @ [ value ]
            if arguments.Length = intrinsicArity name then invokeIntrinsic (applyValue span) span name arguments
            else Ok(VIntrinsic(name, arguments))
        | VExternal(name, arity, existing) ->
            let arguments = existing @ [ value ]
            if arguments.Length = arity then fail "FSI3021" span $"Host external '{name}' is not executable from LIR."
            else Ok(VExternal(name, arity, arguments))
        | VTargetImport(modulePath, memberName, arity, existing) ->
            let arguments = existing @ [ value ]
            if arguments.Length = arity then fail "FSI3023" span $"Target import '{modulePath}.{memberName}' is not executable from LIR."
            else Ok(VTargetImport(modulePath, memberName, arity, arguments))
        | _ -> fail "FSI3003" span "Attempted to call a non-function value."

    and private evaluateMany environment items = items |> List.map (evaluate environment) |> collect

    and private collect<'a> (results: Result<'a, CompilerDiagnostic list> list) : Result<'a list, CompilerDiagnostic list> =
        results |> List.fold (fun state result ->
            match state, result with Ok values, Ok value -> Ok(values @ [ value ]) | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e) (Ok [])

    let execute (context: InterpreterContext) (program: Lir.Program) =
        let output = ResizeArray<string>()
        currentOutput.Value <- output
        let runtimeContext =
            VRecord(Map.ofList
                [ "ScriptName", VOption(Option.map VString context.ScriptName)
                  "Arguments", VList(List.map VString context.Arguments) ])
        let environment = ref Map.empty
        let mutable failure = None
        let rec initialize (declarations: Lir.Declaration list) =
            match declarations with
            | [] -> ()
            | declaration :: rest when not declaration.IsRecursive ->
                match evaluate environment.Value declaration.Value with
                | Error errors -> failure <- Some errors
                | Ok value ->
                    let initialized = match value with VRuntimeContext -> runtimeContext | _ -> value
                    environment.Value <- environment.Value |> Map.add declaration.Id initialized
                    initialize rest
            | _ ->
                let group = declarations |> List.takeWhile _.IsRecursive
                let remainder = declarations |> List.skip group.Length
                let closures = group |> List.map (fun declaration -> declaration, evaluate environment.Value declaration.Value)
                let errors = closures |> List.collect (fun (_, result) -> match result with Error values -> values | Ok _ -> [])
                if not errors.IsEmpty then failure <- Some errors
                else
                    let next = closures |> List.fold (fun env (declaration, result) -> match result with Ok closure -> Map.add declaration.Id closure env | Error _ -> env) environment.Value
                    for _, result in closures do
                        match result with Ok(VClosure(_, _, _, captured)) -> captured.Value <- next | _ -> ()
                    environment.Value <- next
                    initialize remainder
        initialize program.Declarations
        match failure, program.Entry with
        | Some errors, _ -> Error errors
        | None, Some entry -> evaluate environment.Value entry |> Result.map (fun value -> { Value = value; Stdout = List.ofSeq output })
        | None, None -> Ok { Value = VUnit; Stdout = List.ofSeq output }
