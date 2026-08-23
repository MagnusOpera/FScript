namespace FScript.Compiler

open FScript.Language

module Hir =
    type SymbolId = SymbolId of int

    type LiteralValue =
        | Unit
        | Int of int64
        | Float of float
        | Bool of bool
        | String of string

    type PrimitiveOperation =
        | IntAdd
        | FloatAdd
        | IntSubtract
        | FloatSubtract
        | IntMultiply
        | FloatMultiply
        | IntDivide
        | FloatDivide
        | IntRemainder
        | StructuralEqual
        | NumericLessThan
        | NumericGreaterThan
        | NumericLessThanOrEqual
        | NumericGreaterThanOrEqual
        | BooleanAnd
        | BooleanOr

    type Expression =
        { Type: Type
          Span: Span
          Node: ExpressionNode }

    and ExpressionNode =
        | Constant of LiteralValue
        | Reference of SymbolId
        | Lambda of SymbolId * string * Type * Expression
        | Apply of Expression * Expression
        | If of Expression * Expression * Expression
        | Primitive of PrimitiveOperation * Expression list
        | Tuple of Expression list
        | List of Expression list
        | Record of (string * Expression) list
        | FieldGet of Expression * string
        | SomeValue of Expression
        | NoneValue
        | Cons of Expression * Expression
        | Append of Expression * Expression
        | Let of SymbolId * string * bool * Expression * Expression
        | Fail of Expression
        | Match of Expression * MatchCase list
        | Range of Expression * Expression
        | RecordUpdate of Expression * (string * Expression) list
        | IndexGet of Expression * Expression
        | Interpolate of InterpolationPart list
        | UnionCase of string * string * Expression option
        | Map of MapEntry list

    and MapEntry =
        | KeyValue of Expression * Expression
        | Spread of Expression

    and InterpolationPart =
        | TextPart of string
        | ValuePart of Expression

    and Pattern =
        { Type: Type
          Span: Span
          Node: PatternNode }

    and PatternNode =
        | Wildcard
        | Binding of SymbolId * string
        | LiteralPattern of LiteralValue
        | EmptyList
        | ConsPattern of Pattern * Pattern
        | TuplePattern of Pattern list
        | RecordPattern of (string * Pattern) list
        | SomePattern of Pattern
        | NonePattern
        | UnionPattern of string * string * Pattern option

    and MatchCase =
        { Pattern: Pattern
          Guard: Expression option
          Body: Expression
          Span: Span }

    type Declaration =
        { Id: SymbolId
          Name: string
          Scheme: Scheme
          IsRecursive: bool
          IsExported: bool
          Span: Span
          Value: Expression }

    type Program =
        { Declarations: Declaration list
          Entry: Expression option }

    let symbolNumber (SymbolId value) = value

    let rec formatType = Types.typeToString

    let private formatLiteral value =
        match value with
        | Unit -> "unit"
        | Int value -> $"int {value}"
        | Float value -> $"float {value:R}"
        | Bool value -> $"bool {value.ToString().ToLowerInvariant()}"
        | String value -> $"string {System.Text.Json.JsonSerializer.Serialize(value)}"

    let private formatPrimitive value = sprintf "%A" value

    let format (program: Program) =
        let lines = ResizeArray<string>()
        let rec expression indent (value: Expression) =
            let prefix = System.String(' ', indent)
            let typed text = $"{prefix}{text} : {formatType value.Type}"
            match value.Node with
            | Constant literal -> lines.Add(typed $"const {formatLiteral literal}")
            | Reference id -> lines.Add(typed (sprintf "ref %%%d" (symbolNumber id)))
            | Lambda(id, name, parameterType, body) ->
                lines.Add(typed (sprintf "lambda %%%d %s: %s" (symbolNumber id) name (formatType parameterType)))
                expression (indent + 2) body
            | Apply(fn, argument) ->
                lines.Add(typed "apply")
                expression (indent + 2) fn
                expression (indent + 2) argument
            | If(condition, whenTrue, whenFalse) ->
                lines.Add(typed "if")
                expression (indent + 2) condition
                expression (indent + 2) whenTrue
                expression (indent + 2) whenFalse
            | Primitive(operation, arguments) ->
                lines.Add(typed $"primitive {formatPrimitive operation}")
                arguments |> List.iter (expression (indent + 2))
            | Tuple items | List items ->
                lines.Add(typed (match value.Node with Tuple _ -> "tuple" | _ -> "list"))
                items |> List.iter (expression (indent + 2))
            | Record fields ->
                lines.Add(typed "record")
                fields |> List.iter (fun (name, item) -> lines.Add($"{prefix}  field {name}"); expression (indent + 4) item)
            | FieldGet(target, name) -> lines.Add(typed $"field-get {name}"); expression (indent + 2) target
            | SomeValue item -> lines.Add(typed "some"); expression (indent + 2) item
            | NoneValue -> lines.Add(typed "none")
            | Cons(head, tail) -> lines.Add(typed "cons"); expression (indent + 2) head; expression (indent + 2) tail
            | Append(left, right) -> lines.Add(typed "append"); expression (indent + 2) left; expression (indent + 2) right
            | Let(id, name, recursive, binding, body) ->
                lines.Add(typed (sprintf "let%s %%%d %s" (if recursive then " rec" else "") (symbolNumber id) name))
                expression (indent + 2) binding
                expression (indent + 2) body
            | Fail value -> lines.Add(typed "fail"); expression (indent + 2) value
            | Match(target, cases) ->
                lines.Add(typed $"match {cases.Length} cases")
                expression (indent + 2) target
                cases |> List.iter (fun case -> expression (indent + 2) case.Body)
            | Range(startValue, endValue) -> lines.Add(typed "range"); expression (indent + 2) startValue; expression (indent + 2) endValue
            | RecordUpdate(target, fields) ->
                lines.Add(typed "record-update"); expression (indent + 2) target
                fields |> List.iter (fun (_, item) -> expression (indent + 2) item)
            | IndexGet(target, index) -> lines.Add(typed "index-get"); expression (indent + 2) target; expression (indent + 2) index
            | Interpolate parts ->
                lines.Add(typed "interpolate")
                parts |> List.iter (function TextPart _ -> () | ValuePart item -> expression (indent + 2) item)
            | UnionCase(typeName, caseName, payload) ->
                lines.Add(typed $"union {typeName}.{caseName}")
                payload |> Option.iter (expression (indent + 2))
            | Map entries ->
                lines.Add(typed "map")
                entries |> List.iter (function KeyValue(key, value) -> expression (indent + 2) key; expression (indent + 2) value | Spread value -> expression (indent + 2) value)

        for declaration in program.Declarations do
            lines.Add(sprintf "decl %%%d %s: %s" (symbolNumber declaration.Id) declaration.Name (formatType (let (Forall(_, value)) = declaration.Scheme in value)))
            expression 2 declaration.Value
        match program.Entry with
        | Some entry ->
            lines.Add("entry")
            expression 2 entry
        | None -> ()
        System.String.Join("\n", lines) + "\n"
