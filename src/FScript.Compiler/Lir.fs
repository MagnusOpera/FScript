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

    let rec private expression (value: Hir.Expression) : Lir.Expression =
        let node =
            match value.Node with
            | Hir.Constant value -> Lir.Constant(constant value)
            | Hir.Reference id -> Lir.Load(symbol id)
            | Hir.Lambda(id, name, parameterType, body) -> Lir.MakeClosure(symbol id, name, parameterType, expression body)
            | Hir.Apply(fn, argument) -> Lir.CallClosure(expression fn, expression argument)
            | Hir.If(condition, whenTrue, whenFalse) -> Lir.Branch(expression condition, expression whenTrue, expression whenFalse)
            | Hir.Primitive(op, arguments) -> Lir.Operation(operation op, arguments |> List.map expression)
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
