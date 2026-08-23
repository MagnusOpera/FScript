namespace FScript.Compiler

open FScript.Language

module Lir =
    type SymbolId = SymbolId of int

    type Constant =
        | Unit
        | Int of int64
        | Float of float
        | Bool of bool
        | String of string

    type Operation =
        | IntAdd | FloatAdd | IntSubtract | FloatSubtract
        | IntMultiply | FloatMultiply | IntDivide | FloatDivide | IntRemainder
        | StructuralEqual
        | NumericLessThan | NumericGreaterThan | NumericLessThanOrEqual | NumericGreaterThanOrEqual
        | BooleanAnd | BooleanOr

    type Expression =
        { Type: Type
          Span: Span
          Node: ExpressionNode }
    and ExpressionNode =
        | Constant of Constant
        | Load of SymbolId
        | MakeClosure of SymbolId * string * Type * Expression
        | CallClosure of Expression * Expression
        | Branch of Expression * Expression * Expression
        | Operation of Operation * Expression list
        | Tuple of Expression list
        | List of Expression list
        | Record of (string * Expression) list
        | FieldGet of Expression * string
        | SomeValue of Expression
        | NoneValue
        | Cons of Expression * Expression
        | Append of Expression * Expression
        | Let of SymbolId * string * bool * Expression * Expression
        | LetRecursiveGroup of (SymbolId * string * Type * Expression) list * Expression
        | Fail of Expression
        | DecisionTree of Expression * DecisionCase list
        | Range of Expression * Expression
        | RecordUpdate of Expression * (string * Expression) list
        | IndexGet of Expression * Expression
        | Interpolate of InterpolationPart list
        | UnionCase of string * string * Expression option
        | Map of MapEntry list
        | Intrinsic of string
        | HostExternal of string * int
        | ForEach of SymbolId * string * Type * Expression * Expression
        | TypeToken of Type

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
        | LiteralPattern of Constant
        | EmptyList
        | ConsPattern of Pattern * Pattern
        | TuplePattern of Pattern list
        | RecordPattern of (string * Pattern) list
        | SomePattern of Pattern
        | NonePattern
        | UnionPattern of string * string * Pattern option

    and DecisionCase =
        { Pattern: Pattern
          Guard: Expression option
          Body: Expression
          Span: Span }

    type Declaration =
        { Id: SymbolId
          Name: string
          Type: Type
          IsRecursive: bool
          IsExported: bool
          Span: Span
          Value: Expression }

    type Program =
        { Declarations: Declaration list
          Entry: Expression option }

    let symbolNumber (SymbolId value) = value

module LirLowering =
    let private symbol (Hir.SymbolId value) = Lir.SymbolId value

    let private operation = function
        | Hir.IntAdd -> Lir.IntAdd | Hir.FloatAdd -> Lir.FloatAdd
        | Hir.IntSubtract -> Lir.IntSubtract | Hir.FloatSubtract -> Lir.FloatSubtract
        | Hir.IntMultiply -> Lir.IntMultiply | Hir.FloatMultiply -> Lir.FloatMultiply
        | Hir.IntDivide -> Lir.IntDivide | Hir.FloatDivide -> Lir.FloatDivide
        | Hir.IntRemainder -> Lir.IntRemainder | Hir.StructuralEqual -> Lir.StructuralEqual
        | Hir.NumericLessThan -> Lir.NumericLessThan | Hir.NumericGreaterThan -> Lir.NumericGreaterThan
        | Hir.NumericLessThanOrEqual -> Lir.NumericLessThanOrEqual
        | Hir.NumericGreaterThanOrEqual -> Lir.NumericGreaterThanOrEqual
        | Hir.BooleanAnd -> Lir.BooleanAnd | Hir.BooleanOr -> Lir.BooleanOr

    let private constant = function
        | Hir.Unit -> Lir.Unit | Hir.Int value -> Lir.Int value | Hir.Float value -> Lir.Float value
        | Hir.Bool value -> Lir.Bool value | Hir.String value -> Lir.String value

    let rec private pattern (value: Hir.Pattern) : Lir.Pattern =
        let node =
            match value.Node with
            | Hir.Wildcard -> Lir.Wildcard
            | Hir.Binding(id, name) -> Lir.Binding(symbol id, name)
            | Hir.LiteralPattern value -> Lir.LiteralPattern(constant value)
            | Hir.EmptyList -> Lir.EmptyList
            | Hir.ConsPattern(head, tail) -> Lir.ConsPattern(pattern head, pattern tail)
            | Hir.TuplePattern items -> Lir.TuplePattern(List.map pattern items)
            | Hir.RecordPattern fields -> Lir.RecordPattern(fields |> List.map (fun (name, item) -> name, pattern item))
            | Hir.SomePattern item -> Lir.SomePattern(pattern item)
            | Hir.NonePattern -> Lir.NonePattern
            | Hir.UnionPattern(typeName, caseName, payload) -> Lir.UnionPattern(typeName, caseName, Option.map pattern payload)
        { Type = value.Type; Span = value.Span; Node = node }

    let rec private expression (value: Hir.Expression) : Lir.Expression =
        let node =
            match value.Node with
            | Hir.Constant value -> Lir.Constant(constant value)
            | Hir.Reference id -> Lir.Load(symbol id)
            | Hir.Lambda(id, name, parameterType, body) -> Lir.MakeClosure(symbol id, name, parameterType, expression body)
            | Hir.Apply(fn, argument) -> Lir.CallClosure(expression fn, expression argument)
            | Hir.If(condition, whenTrue, whenFalse) -> Lir.Branch(expression condition, expression whenTrue, expression whenFalse)
            | Hir.Primitive(op, arguments) -> Lir.Operation(operation op, arguments |> List.map expression)
            | Hir.Tuple items -> Lir.Tuple(items |> List.map expression)
            | Hir.List items -> Lir.List(items |> List.map expression)
            | Hir.Record fields -> Lir.Record(fields |> List.map (fun (name, item) -> name, expression item))
            | Hir.FieldGet(target, name) -> Lir.FieldGet(expression target, name)
            | Hir.SomeValue item -> Lir.SomeValue(expression item)
            | Hir.NoneValue -> Lir.NoneValue
            | Hir.Cons(head, tail) -> Lir.Cons(expression head, expression tail)
            | Hir.Append(left, right) -> Lir.Append(expression left, expression right)
            | Hir.Let(id, name, recursive, binding, body) -> Lir.Let(symbol id, name, recursive, expression binding, expression body)
            | Hir.LetRecursiveGroup(bindings, body) ->
                Lir.LetRecursiveGroup(bindings |> List.map (fun (id, name, bindingType, value) -> symbol id, name, bindingType, expression value), expression body)
            | Hir.Fail value -> Lir.Fail(expression value)
            | Hir.Match(target, cases) ->
                Lir.DecisionTree(expression target, cases |> List.map (fun case ->
                    ({ Pattern = pattern case.Pattern
                       Guard = Option.map expression case.Guard
                       Body = expression case.Body
                       Span = case.Span }: Lir.DecisionCase)))
            | Hir.Range(startValue, endValue) -> Lir.Range(expression startValue, expression endValue)
            | Hir.RecordUpdate(target, fields) -> Lir.RecordUpdate(expression target, fields |> List.map (fun (name, item) -> name, expression item))
            | Hir.IndexGet(target, index) -> Lir.IndexGet(expression target, expression index)
            | Hir.Interpolate parts -> Lir.Interpolate(parts |> List.map (function Hir.TextPart text -> Lir.TextPart text | Hir.ValuePart item -> Lir.ValuePart(expression item)))
            | Hir.UnionCase(typeName, caseName, payload) -> Lir.UnionCase(typeName, caseName, Option.map expression payload)
            | Hir.Map entries -> Lir.Map(entries |> List.map (function Hir.KeyValue(key, item) -> Lir.KeyValue(expression key, expression item) | Hir.Spread item -> Lir.Spread(expression item)))
            | Hir.Intrinsic name -> Lir.Intrinsic name
            | Hir.HostExternal(name, arity) -> Lir.HostExternal(name, arity)
            | Hir.ForEach(id, name, itemType, source, body) -> Lir.ForEach(symbol id, name, itemType, expression source, expression body)
            | Hir.TypeToken tokenType -> Lir.TypeToken tokenType
        { Type = value.Type; Span = value.Span; Node = node }

    let lower (program: Hir.Program) : Lir.Program =
        { Declarations =
            program.Declarations
            |> List.map (fun value ->
                let (Forall(_, declarationType)) = value.Scheme
                ({ Id = symbol value.Id; Name = value.Name; Type = declarationType
                   IsRecursive = value.IsRecursive; IsExported = value.IsExported
                   Span = value.Span; Value = expression value.Value }: Lir.Declaration))
          Entry = program.Entry |> Option.map expression }

module LirValidation =
    let validate (program: Lir.Program) =
        let ids = program.Declarations |> List.map _.Id
        if ids.Length <> (ids |> Set.ofList |> Set.count) then
            Error [ { Code = "FSL2001"; Stage = Lowering; Message = "LIR declaration IDs must be unique."; Span = None } ]
        else Ok program
