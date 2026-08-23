namespace FScript.Compiler

open FScript.Language

module HirLowering =
    type private State =
        { mutable NextSymbol: int
          ExpressionTypes: Map<Expr, Type>
          mutable Diagnostics: CompilerDiagnostic list }

    let private newSymbol state =
        let value = state.NextSymbol
        state.NextSymbol <- value + 1
        Hir.SymbolId value

    let private expressionType state expression fallback =
        state.ExpressionTypes |> Map.tryFind expression |> Option.defaultValue fallback

    let private literal = function
        | LInt value -> Hir.Int value, TInt
        | LFloat value -> Hir.Float value, TFloat
        | LBool value -> Hir.Bool value, TBool
        | LString value -> Hir.String value, TString

    let private operation op operandType =
        match op, operandType with
        | "+", TInt -> Some Hir.IntAdd
        | "+", TFloat -> Some Hir.FloatAdd
        | "-", TInt -> Some Hir.IntSubtract
        | "-", TFloat -> Some Hir.FloatSubtract
        | "*", TInt -> Some Hir.IntMultiply
        | "*", TFloat -> Some Hir.FloatMultiply
        | "/", TInt -> Some Hir.IntDivide
        | "/", TFloat -> Some Hir.FloatDivide
        | "%", TInt -> Some Hir.IntRemainder
        | "=", _ -> Some Hir.StructuralEqual
        | "<", _ -> Some Hir.NumericLessThan
        | ">", _ -> Some Hir.NumericGreaterThan
        | "<=", _ -> Some Hir.NumericLessThanOrEqual
        | ">=", _ -> Some Hir.NumericGreaterThanOrEqual
        | "&&", TBool -> Some Hir.BooleanAnd
        | "||", TBool -> Some Hir.BooleanOr
        | _ -> None

    let rec private lowerExpression state environment fallback expression =
        let span = Ast.spanOfExpr expression
        let inferred = expressionType state expression fallback
        let make node = ({ Type = inferred; Span = span; Node = node }: Hir.Expression)
        match expression with
        | EUnit _ -> Ok(make (Hir.Constant Hir.Unit))
        | ELiteral(value, _) ->
            let value, _ = literal value
            Ok(make (Hir.Constant value))
        | EParen(inner, _) -> lowerExpression state environment inferred inner
        | EVar(name, _) ->
            match environment |> Map.tryFind name with
            | Some id -> Ok(make (Hir.Reference id))
            | None -> Error [ Diagnostic.hir "FSH1001" $"Unresolved HIR reference '{name}'." (Some span) ]
        | ELambda(parameter, body, _) ->
            match inferred with
            | TFun(parameterType, resultType) ->
                let id = newSymbol state
                lowerExpression state (environment |> Map.add parameter.Name id) resultType body
                |> Result.map (fun lowered -> make (Hir.Lambda(id, parameter.Name, parameterType, lowered)))
            | _ -> Error [ Diagnostic.hir "FSH1002" "Lambda does not have a function type." (Some span) ]
        | EApply(fn, argument, _) ->
            let fnType = expressionType state fn (TFun(Types.freshVar(), inferred))
            let argumentType =
                match fnType with
                | TFun(value, _) -> value
                | _ -> Types.freshVar()
            match lowerExpression state environment fnType fn, lowerExpression state environment argumentType argument with
            | Ok loweredFn, Ok loweredArgument -> Ok(make (Hir.Apply(loweredFn, loweredArgument)))
            | Error left, Error right -> Error(left @ right)
            | Error errors, _
            | _, Error errors -> Error errors
        | EIf(condition, whenTrue, whenFalse, _) ->
            match lowerExpression state environment TBool condition,
                  lowerExpression state environment inferred whenTrue,
                  lowerExpression state environment inferred whenFalse with
            | Ok c, Ok t, Ok f -> Ok(make (Hir.If(c, t, f)))
            | results ->
                let errors =
                    [ match results with
                      | Error e, _, _ -> yield! e
                      | _ -> ()
                      match results with
                      | _, Error e, _ -> yield! e
                      | _ -> ()
                      match results with
                      | _, _, Error e -> yield! e
                      | _ -> () ]
                Error errors
        | EBinOp(op, left, right, _) ->
            let capturedOperandType = expressionType state left inferred
            let operandType =
                match capturedOperandType, inferred with
                | TVar _, (TInt | TFloat as concrete) -> concrete
                | value, _ -> value
            match operation op operandType with
            | None -> Error [ Diagnostic.hir "FSH1003" $"Unresolved primitive operation '{op}'." (Some span) ]
            | Some primitive ->
                match lowerExpression state environment operandType left, lowerExpression state environment operandType right with
                | Ok l, Ok r -> Ok(make (Hir.Primitive(primitive, [ l; r ])))
                | Error l, Error r -> Error(l @ r)
                | Error errors, _
                | _, Error errors -> Error errors
        | _ -> Error [ Diagnostic.hir "FSH1099" $"HIR lowering does not yet support {expression.GetType().Name}." (Some span) ]

    let lower (resolved: TypeInfer.ResolvedProgram) : Result<Hir.Program, CompilerDiagnostic list> =
        let expressionTypes =
            resolved.ExpressionTypes
            |> List.fold (fun state item -> state |> Map.add item.Expr item.Type) Map.empty
        let state = { NextSymbol = 0; ExpressionTypes = expressionTypes; Diagnostics = [] }
        let builtins =
            BuiltinSignatures.builtinSchemes
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (name, _) -> name, newSymbol state)
            |> Map.ofList
        let topLevel =
            resolved.TypedProgram
            |> List.collect (function
                | TypeInfer.TSLet(name, _, _, _, _, _) -> [ name ]
                | TypeInfer.TSLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
                | _ -> [])
            |> List.distinct
            |> List.fold (fun env name -> env |> Map.add name (newSymbol state)) builtins

        let mutable declarations = []
        let mutable entry = None
        let mutable errors = []
        for statement in resolved.TypedProgram do
            match statement with
            | TypeInfer.TSLet(name, expression, inferred, isRecursive, isExported, span) ->
                match lowerExpression state topLevel inferred expression with
                | Ok value ->
                    declarations <- declarations @
                        [ ({ Id = Map.find name topLevel;
                            Name = name;
                            Scheme = Types.generalize Map.empty inferred;
                            IsRecursive = isRecursive;
                            IsExported = isExported;
                            Span = span;
                            Value = value }: Hir.Declaration) ]
                | Error diagnostics -> errors <- errors @ diagnostics
            | TypeInfer.TSExpr expression ->
                match lowerExpression state topLevel expression.Type expression.Expr with
                | Ok value -> entry <- Some value
                | Error diagnostics -> errors <- errors @ diagnostics
            | TypeInfer.TSType _ -> ()
            | _ -> errors <- errors @ [ Diagnostic.hir "FSH1098" "Top-level declaration form is not yet supported by HIR lowering." None ]
        if errors.IsEmpty then Ok { Declarations = declarations; Entry = entry }
        else Error errors
