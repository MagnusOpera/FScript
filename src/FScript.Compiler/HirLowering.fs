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

    let private collectResults<'a> (results: Result<'a, CompilerDiagnostic list> list) : Result<'a list, CompilerDiagnostic list> =
        let folder state result =
            match state, result with
            | Ok values, Ok value -> Ok(values @ [ value ])
            | Error a, Error b -> Error(a @ b)
            | Error errors, _ | _, Error errors -> Error errors
        results |> List.fold folder (Ok [])

    let rec private lowerPattern state expected pattern =
        let span = Ast.spanOfPattern pattern
        let make node = ({ Type = expected; Span = span; Node = node }: Hir.Pattern)
        match pattern with
        | PWildcard _ -> Ok(make Hir.Wildcard, Map.empty)
        | PVar(name, _) ->
            let id = newSymbol state
            Ok(make (Hir.Binding(id, name)), Map.ofList [ name, id ])
        | PLiteral(value, _) -> let lowered, _ = literal value in Ok(make (Hir.LiteralPattern lowered), Map.empty)
        | PNil _ -> Ok(make Hir.EmptyList, Map.empty)
        | PNone _ -> Ok(make Hir.NonePattern, Map.empty)
        | PSome(inner, _) ->
            let itemType = match expected with TOption value -> value | _ -> Types.freshVar()
            lowerPattern state itemType inner |> Result.map (fun (value, bindings) -> make (Hir.SomePattern value), bindings)
        | PCons(head, tail, _) ->
            let itemType = match expected with TList value -> value | _ -> Types.freshVar()
            match lowerPattern state itemType head, lowerPattern state expected tail with
            | Ok(h, hb), Ok(t, tb) -> Ok(make (Hir.ConsPattern(h, t)), Map.fold (fun env key value -> Map.add key value env) hb tb)
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | PTuple(items, _) ->
            let types = match expected with TTuple values when values.Length = items.Length -> values | _ -> List.replicate items.Length (Types.freshVar())
            List.zip items types |> List.map (fun (item, itemType) -> lowerPattern state itemType item)
            |> collectResults |> Result.map (fun values ->
                let patterns, bindingMaps = List.unzip values
                let bindings = bindingMaps |> List.fold (Map.fold (fun env key value -> Map.add key value env)) Map.empty
                make (Hir.TuplePattern patterns), bindings)
        | PRecord(fields, _) ->
            let types = match expected with TRecord values -> values | _ -> Map.empty
            fields |> List.map (fun (name, item) -> lowerPattern state (types |> Map.tryFind name |> Option.defaultValue (Types.freshVar())) item |> Result.map (fun (value, bindings) -> (name, value), bindings))
            |> collectResults |> Result.map (fun values ->
                let fields, bindingMaps = List.unzip values
                let bindings = bindingMaps |> List.fold (Map.fold (fun env key value -> Map.add key value env)) Map.empty
                make (Hir.RecordPattern fields), bindings)
        | _ -> Error [ Diagnostic.hir "FSH1004" "Pattern form is not yet supported by HIR lowering." (Some span) ]

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
            if op = "@" then
                match lowerExpression state environment inferred left, lowerExpression state environment inferred right with
                | Ok l, Ok r -> Ok(make (Hir.Append(l, r)))
                | Error a, Error b -> Error(a @ b)
                | Error errors, _ | _, Error errors -> Error errors
            else
                match operation op operandType with
                | None -> Error [ Diagnostic.hir "FSH1003" $"Unresolved primitive operation '{op}'." (Some span) ]
                | Some primitive ->
                    match lowerExpression state environment operandType left, lowerExpression state environment operandType right with
                    | Ok l, Ok r -> Ok(make (Hir.Primitive(primitive, [ l; r ])))
                    | Error l, Error r -> Error(l @ r)
                    | Error errors, _
                    | _, Error errors -> Error errors
        | ETuple(items, _) ->
            let itemTypes = match inferred with TTuple values -> values | _ -> List.replicate items.Length (Types.freshVar())
            List.zip items itemTypes |> List.map (fun (item, itemType) -> lowerExpression state environment itemType item)
            |> collectResults |> Result.map (Hir.Tuple >> make)
        | EList(items, _) ->
            let itemType = match inferred with TList value -> value | _ -> Types.freshVar()
            items |> List.map (lowerExpression state environment itemType) |> collectResults |> Result.map (Hir.List >> make)
        | ERecord(fields, _) | EStructuralRecord(fields, _) ->
            let fieldTypes = match inferred with TRecord values -> values | _ -> Map.empty
            fields |> List.map (fun (name, item) -> lowerExpression state environment (fieldTypes |> Map.tryFind name |> Option.defaultValue (Types.freshVar())) item |> Result.map (fun value -> name, value))
            |> collectResults |> Result.map (Hir.Record >> make)
        | EFieldGet(target, name, _) ->
            lowerExpression state environment (expressionType state target (TRecord Map.empty)) target |> Result.map (fun value -> make (Hir.FieldGet(value, name)))
        | ESome(item, _) ->
            let itemType = match inferred with TOption value -> value | _ -> Types.freshVar()
            lowerExpression state environment itemType item |> Result.map (Hir.SomeValue >> make)
        | ENone _ -> Ok(make Hir.NoneValue)
        | ECons(head, tail, _) ->
            let itemType = match inferred with TList value -> value | _ -> Types.freshVar()
            match lowerExpression state environment itemType head, lowerExpression state environment inferred tail with
            | Ok h, Ok t -> Ok(make (Hir.Cons(h, t))) | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | EAppend(left, right, _) ->
            match lowerExpression state environment inferred left, lowerExpression state environment inferred right with
            | Ok l, Ok r -> Ok(make (Hir.Append(l, r))) | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | ELet(name, binding, body, isRecursive, _, _) ->
            let id = newSymbol state
            let bindingType = expressionType state binding (Types.freshVar())
            let bodyEnvironment = environment |> Map.add name id
            let bindingEnvironment = if isRecursive then bodyEnvironment else environment
            match lowerExpression state bindingEnvironment bindingType binding, lowerExpression state bodyEnvironment inferred body with
            | Ok value, Ok loweredBody -> Ok(make (Hir.Let(id, name, isRecursive, value, loweredBody)))
            | Error a, Error b -> Error(a @ b)
            | Error errors, _ | _, Error errors -> Error errors
        | ERaise(value, _) -> lowerExpression state environment TString value |> Result.map (Hir.Fail >> make)
        | EMatch(target, cases, _) ->
            let targetType = expressionType state target (Types.freshVar())
            match lowerExpression state environment targetType target with
            | Error errors -> Error errors
            | Ok loweredTarget ->
                cases |> List.map (fun (pattern, guard, body, caseSpan) ->
                    lowerPattern state targetType pattern |> Result.bind (fun (loweredPattern, bindings) ->
                        let caseEnvironment = Map.fold (fun env name id -> Map.add name id env) environment bindings
                        match guard |> Option.map (fun value -> lowerExpression state caseEnvironment TBool value |> Result.map Some) |> Option.defaultValue (Ok None), lowerExpression state caseEnvironment inferred body with
                        | Ok loweredGuard, Ok loweredBody -> Ok({ Pattern = loweredPattern; Guard = loweredGuard; Body = loweredBody; Span = caseSpan }: Hir.MatchCase)
                        | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e))
                |> collectResults |> Result.map (fun loweredCases -> make (Hir.Match(loweredTarget, loweredCases)))
        | ERange(startValue, endValue, _) ->
            match lowerExpression state environment TInt startValue, lowerExpression state environment TInt endValue with
            | Ok startItem, Ok endItem -> Ok(make (Hir.Range(startItem, endItem)))
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | ERecordUpdate(target, fields, _) | EStructuralRecordUpdate(target, fields, _) ->
            match lowerExpression state environment inferred target with
            | Error errors -> Error errors
            | Ok loweredTarget ->
                let fieldTypes = match inferred with TRecord values -> values | _ -> Map.empty
                fields |> List.map (fun (name, item) -> lowerExpression state environment (fieldTypes |> Map.tryFind name |> Option.defaultValue (Types.freshVar())) item |> Result.map (fun value -> name, value))
                |> collectResults |> Result.map (fun loweredFields -> make (Hir.RecordUpdate(loweredTarget, loweredFields)))
        | EIndexGet(target, index, _) ->
            let targetType = expressionType state target (Types.freshVar())
            let indexType = match targetType with TList _ -> TInt | TMap(key, _) -> key | _ -> Types.freshVar()
            match lowerExpression state environment targetType target, lowerExpression state environment indexType index with
            | Ok loweredTarget, Ok loweredIndex -> Ok(make (Hir.IndexGet(loweredTarget, loweredIndex)))
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | ENameOf(name, _) -> Ok(make (Hir.Constant(Hir.String name)))
        | EInterpolatedString(parts, _) ->
            parts |> List.map (function
                | IPText text -> Ok(Hir.TextPart text)
                | IPExpr item -> lowerExpression state environment (expressionType state item (Types.freshVar())) item |> Result.map Hir.ValuePart)
            |> collectResults |> Result.map (Hir.Interpolate >> make)
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
