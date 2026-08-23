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
        let rec expression indent value =
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

        for declaration in program.Declarations do
            lines.Add(sprintf "decl %%%d %s: %s" (symbolNumber declaration.Id) declaration.Name (formatType (let (Forall(_, value)) = declaration.Scheme in value)))
            expression 2 declaration.Value
        match program.Entry with
        | Some entry ->
            lines.Add("entry")
            expression 2 entry
        | None -> ()
        System.String.Join("\n", lines) + "\n"
