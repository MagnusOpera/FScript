namespace FScript.Compiler

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
    | VClosure of Lir.SymbolId * string * Lir.Expression * Map<Lir.SymbolId, LirValue> ref

type ExecutionResult = { Value: LirValue }

module LirInterpreter =
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
        | _ -> false

    let private fail code span message =
        Error [ { Code = code; Stage = Interpreter; Message = message; Span = Some span } ]

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
            Ok(match value with Lir.Unit -> VUnit | Lir.Int v -> VInt v | Lir.Float v -> VFloat v | Lir.Bool v -> VBool v | Lir.String v -> VString v)
        | Lir.Load id ->
            environment |> Map.tryFind id |> Option.map Ok
            |> Option.defaultWith (fun () -> fail "FSI3001" expression.Span (sprintf "Unknown LIR symbol %%%d." (Lir.symbolNumber id)))
        | Lir.MakeClosure(id, name, _, body) -> Ok(VClosure(id, name, body, ref environment))
        | Lir.CallClosure(fn, argument) ->
            match evaluate environment fn, evaluate environment argument with
            | Ok(VClosure(parameter, _, body, captured)), Ok value -> evaluate (captured.Value |> Map.add parameter value) body
            | Ok _, Ok _ -> fail "FSI3003" expression.Span "Attempted to call a non-function value."
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

    and private evaluateMany environment items = items |> List.map (evaluate environment) |> collect

    and private collect<'a> (results: Result<'a, CompilerDiagnostic list> list) : Result<'a list, CompilerDiagnostic list> =
        results |> List.fold (fun state result ->
            match state, result with Ok values, Ok value -> Ok(values @ [ value ]) | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e) (Ok [])

    let execute (_context: InterpreterContext) (program: Lir.Program) =
        let environment = ref Map.empty
        let mutable failure = None
        for declaration in program.Declarations do
            if failure.IsNone then
                match evaluate environment.Value declaration.Value with
                | Error errors -> failure <- Some errors
                | Ok(VClosure(id, name, body, captured) as closure) ->
                    let next = environment.Value |> Map.add declaration.Id closure
                    if declaration.IsRecursive then captured.Value <- next
                    environment.Value <- next
                | Ok value -> environment.Value <- environment.Value |> Map.add declaration.Id value
        match failure, program.Entry with
        | Some errors, _ -> Error errors
        | None, Some entry -> evaluate environment.Value entry |> Result.map (fun value -> { Value = value })
        | None, None -> Ok { Value = VUnit }
