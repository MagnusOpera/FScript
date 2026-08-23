namespace FScript.Compiler

open FScript.Language

module HirLowering =
    type private State =
        { mutable NextSymbol: int
          ExpressionTypes: Map<Expr, Type>
          mutable UnionCases: Map<string, string * Type option>
          mutable TypeDefinitions: Map<string, Type>
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
        | PMap(clauses, tail, _) ->
            let keyType, valueType = match expected with TMap(key, value) -> key, value | _ -> Types.freshVar(), Types.freshVar()
            let loweredClauses =
                clauses |> List.map (fun (keyPattern, valuePattern) ->
                    match lowerPattern state keyType keyPattern, lowerPattern state valueType valuePattern with
                    | Ok(key, keyBindings), Ok(value, valueBindings) ->
                        Ok((key, value), Map.fold (fun env name id -> Map.add name id env) keyBindings valueBindings)
                    | Error a, Error b -> Error(a @ b) | Error errors, _ | _, Error errors -> Error errors)
                |> collectResults
            let loweredTail =
                tail |> Option.map (lowerPattern state expected >> Result.map Some) |> Option.defaultValue (Ok None)
            match loweredClauses, loweredTail with
            | Ok clausesWithBindings, Ok tailValue ->
                let clauses, bindingMaps = List.unzip clausesWithBindings
                let tailPattern, tailBindings = tailValue |> Option.map (fun (pattern, bindings) -> Some pattern, bindings) |> Option.defaultValue (None, Map.empty)
                let bindings = tailBindings :: bindingMaps |> List.fold (Map.fold (fun env name id -> Map.add name id env)) Map.empty
                Ok(make (Hir.MapPattern(clauses, tailPattern)), bindings)
            | Error a, Error b -> Error(a @ b) | Error errors, _ | _, Error errors -> Error errors
        | PUnionCase(typeName, caseName, payload, _) ->
            let resolvedTypeName, payloadType =
                match expected with
                | TUnion(name, cases) -> name, cases |> Map.tryFind caseName |> Option.flatten
                | _ ->
                    state.UnionCases |> Map.tryFind caseName
                    |> Option.defaultValue (typeName |> Option.defaultValue "", None)
            match payload, payloadType with
            | None, _ -> Ok(make (Hir.UnionPattern(resolvedTypeName, caseName, None)), Map.empty)
            | Some item, Some itemType -> lowerPattern state itemType item |> Result.map (fun (value, bindings) -> make (Hir.UnionPattern(resolvedTypeName, caseName, Some value)), bindings)
            | Some item, None -> lowerPattern state (Types.freshVar()) item |> Result.map (fun (value, bindings) -> make (Hir.UnionPattern(resolvedTypeName, caseName, Some value)), bindings)
        | PTypeRef _ -> Ok(make (Hir.TypePattern expected), Map.empty)

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
            match target with
            | EVar(moduleName, _) ->
                match environment |> Map.tryFind $"{moduleName}.{name}" with
                | Some id -> Ok(make (Hir.Reference id))
                | None -> lowerExpression state environment (expressionType state target (TRecord Map.empty)) target |> Result.map (fun value -> make (Hir.FieldGet(value, name)))
            | _ -> lowerExpression state environment (expressionType state target (TRecord Map.empty)) target |> Result.map (fun value -> make (Hir.FieldGet(value, name)))
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
        | ELetRecGroup(bindings, body, _) ->
            let ids = bindings |> List.map (fun (name, _, _, _, _) -> name, newSymbol state) |> Map.ofList
            let recursiveEnvironment = Map.fold (fun env name id -> Map.add name id env) environment ids
            let loweredBindings =
                bindings
                |> List.map (fun (name, parameters, _, valueExpression, bindingSpan) ->
                    let folded = Seq.foldBack (fun parameter inner -> ELambda(parameter, inner, bindingSpan)) parameters valueExpression
                    let bindingType = expressionType state folded (Types.freshVar())
                    lowerExpression state recursiveEnvironment bindingType folded
                    |> Result.map (fun value -> ids.[name], name, bindingType, value))
                |> collectResults
            match loweredBindings, lowerExpression state recursiveEnvironment inferred body with
            | Ok values, Ok loweredBody -> Ok(make (Hir.LetRecursiveGroup(values, loweredBody)))
            | Error a, Error b -> Error(a @ b)
            | Error errors, _ | _, Error errors -> Error errors
        | ERaise(value, _) -> lowerExpression state environment TString value |> Result.map (Hir.Fail >> make)
        | ELetPattern(pattern, binding, body, _) ->
            let bindingType = expressionType state binding (Types.freshVar())
            match lowerExpression state environment bindingType binding, lowerPattern state bindingType pattern with
            | Ok loweredBinding, Ok(loweredPattern, bindings) ->
                let bodyEnvironment = Map.fold (fun env name id -> Map.add name id env) environment bindings
                lowerExpression state bodyEnvironment inferred body
                |> Result.map (fun loweredBody -> make (Hir.Match(loweredBinding, [ { Pattern = loweredPattern; Guard = None; Body = loweredBody; Span = Ast.spanOfPattern pattern } ])))
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
        | EFor(name, source, body, _) ->
            let sourceType = expressionType state source (TList(Types.freshVar()))
            let itemType = match sourceType with TList item -> item | _ -> Types.freshVar()
            let id = newSymbol state
            match lowerExpression state environment sourceType source, lowerExpression state (environment |> Map.add name id) TUnit body with
            | Ok loweredSource, Ok loweredBody -> Ok(make (Hir.ForEach(id, name, itemType, loweredSource, loweredBody)))
            | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
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
        | ETypeOf(name, _) ->
            match state.TypeDefinitions |> Map.tryFind name with
            | Some value -> Ok(make (Hir.TypeToken value))
            | None -> Error [ Diagnostic.hir "FSH1005" $"Unknown type '{name}'." (Some span) ]
        | EInterpolatedString(parts, _) ->
            parts |> List.map (function
                | IPText text -> Ok(Hir.TextPart text)
                | IPExpr item -> lowerExpression state environment (expressionType state item (Types.freshVar())) item |> Result.map Hir.ValuePart)
            |> collectResults |> Result.map (Hir.Interpolate >> make)
        | EMap(entries, _) ->
            let keyType, valueType = match inferred with TMap(key, value) -> key, value | _ -> Types.freshVar(), Types.freshVar()
            entries |> List.map (function
                | MEKeyValue(key, value) ->
                    match lowerExpression state environment keyType key, lowerExpression state environment valueType value with
                    | Ok loweredKey, Ok loweredValue -> Ok(Hir.KeyValue(loweredKey, loweredValue))
                    | Error a, Error b -> Error(a @ b) | Error e, _ | _, Error e -> Error e
                | MESpread value -> lowerExpression state environment inferred value |> Result.map Hir.Spread)
            |> collectResults |> Result.map (Hir.Map >> make)

    let lower (externs: ExternalFunction list) (resolved: TypeInfer.ResolvedProgram) : Result<Hir.Program, CompilerDiagnostic list> =
        let expressionTypes =
            resolved.ExpressionTypes
            |> List.fold (fun state item -> state |> Map.add item.Expr item.Type) Map.empty
        let state = { NextSymbol = 0; ExpressionTypes = expressionTypes; UnionCases = Map.empty; TypeDefinitions = Map.empty; Diagnostics = [] }
        let builtins =
            BuiltinSignatures.builtinSchemes
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (name, _) -> name, newSymbol state)
            |> Map.ofList
        let externSymbols = externs |> List.sortBy _.Name |> List.map (fun value -> value.Name, newSymbol state) |> Map.ofList
        let typeDefinitions =
            resolved.TypedProgram |> List.choose (function TypeInfer.TSType definition -> Some definition | _ -> None)
        let rec typeFromRef = function
            | TRName "unit" -> TUnit | TRName "int" -> TInt | TRName "float" -> TFloat | TRName "bool" -> TBool | TRName "string" -> TString
            | TRName name -> TNamed name
            | TRTuple items -> TTuple(List.map typeFromRef items)
            | TRFun(argument, result) -> TFun(typeFromRef argument, typeFromRef result)
            | TRPostfix(item, "list") -> TList(typeFromRef item)
            | TRPostfix(item, "option") -> TOption(typeFromRef item)
            | TRPostfix(item, "task") -> TTask(typeFromRef item)
            | TRRecord fields | TRStructuralRecord fields -> TRecord(fields |> List.map (fun (name, item) -> name, typeFromRef item) |> Map.ofList)
            | value -> TNamed(sprintf "%A" value)
        let unionDefinitions =
            typeDefinitions |> List.filter (fun definition -> not definition.Cases.IsEmpty) |> List.map (fun definition ->
                let cases = definition.Cases |> List.map (fun (name, payload) -> name, Option.map typeFromRef payload) |> Map.ofList
                definition, TUnion(definition.Name, cases))
        state.TypeDefinitions <-
            typeDefinitions |> List.map (fun definition ->
                let value =
                    if not definition.Cases.IsEmpty then
                        TUnion(definition.Name, definition.Cases |> List.map (fun (name, payload) -> name, Option.map typeFromRef payload) |> Map.ofList)
                    else TRecord(definition.Fields |> List.map (fun (name, fieldType) -> name, typeFromRef fieldType) |> Map.ofList)
                definition.Name, value) |> Map.ofList
        state.UnionCases <-
            unionDefinitions |> List.collect (fun (definition, _) ->
                let cases = definition.Cases |> List.map (fun (name, payload) -> name, Option.map typeFromRef payload) |> Map.ofList
                definition.Cases |> List.map (fun (caseName, _) -> caseName, (definition.Name, cases.[caseName]))) |> Map.ofList
        let constructorSymbols =
            unionDefinitions |> List.collect (fun (definition, _) -> definition.Cases |> List.map (fun (caseName, _) -> definition.Name, caseName, newSymbol state))
        let topLevel =
            resolved.TypedProgram
            |> List.collect (function
                | TypeInfer.TSLet(name, _, _, _, _, _) -> [ name ]
                | TypeInfer.TSLetPattern(_, _, bindings, _, _) -> bindings |> Map.keys |> Seq.toList
                | TypeInfer.TSLetRecGroup(bindings, _, _) -> bindings |> List.map (fun (name, _, _, _) -> name)
                | _ -> [])
            |> List.distinct
            |> List.fold (fun env name -> env |> Map.add name (newSymbol state)) (Map.fold (fun env name id -> Map.add name id env) builtins externSymbols)
        let topLevel =
            constructorSymbols |> List.fold (fun env (typeName, caseName, id) -> env |> Map.add caseName id |> Map.add $"{typeName}.{caseName}" id) topLevel

        let mutable declarations =
            let builtinDeclarations =
                BuiltinSignatures.builtinSchemes |> Map.toList |> List.sortBy fst |> List.map (fun (name, scheme) ->
                    let (Forall(_, valueType)) = scheme
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    ({ Id = builtins.[name]; Name = name; Scheme = scheme; IsRecursive = false; IsExported = false; Span = span; Value = { Type = valueType; Span = span; Node = Hir.Intrinsic name } }: Hir.Declaration))
            let externalDeclarations =
                externs |> List.sortBy _.Name |> List.map (fun external ->
                    let (Forall(_, valueType)) = external.Scheme
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    let node = if external.Name = "Env" then Hir.RuntimeContext else Hir.HostExternal(external.Name, external.Arity)
                    ({ Id = externSymbols.[external.Name]; Name = external.Name; Scheme = external.Scheme; IsRecursive = false; IsExported = false; Span = span; Value = { Type = valueType; Span = span; Node = node } }: Hir.Declaration))
            builtinDeclarations @ externalDeclarations @ (constructorSymbols |> List.map (fun (typeName, caseName, id) ->
                let definition, unionType = unionDefinitions |> List.find (fun (definition, _) -> definition.Name = typeName)
                let payloadType = definition.Cases |> List.find (fst >> (=) caseName) |> snd |> Option.map typeFromRef
                let value =
                    match payloadType with
                    | None -> ({ Type = unionType; Span = definition.Span; Node = Hir.UnionCase(typeName, caseName, None) }: Hir.Expression)
                    | Some itemType ->
                        let parameter = newSymbol state
                        let body = ({ Type = unionType; Span = definition.Span; Node = Hir.UnionCase(typeName, caseName, Some({ Type = itemType; Span = definition.Span; Node = Hir.Reference parameter })) }: Hir.Expression)
                        ({ Type = TFun(itemType, unionType); Span = definition.Span; Node = Hir.Lambda(parameter, "value", itemType, body) }: Hir.Expression)
                ({ Id = id; Name = $"{typeName}.{caseName}"; Scheme = Types.generalize Map.empty value.Type; IsRecursive = false; IsExported = false; Span = definition.Span; Value = value }: Hir.Declaration)))
        let mutable entry: Hir.Expression option = None
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
                | Ok value ->
                    let id = newSymbol state
                    let name = $"$expression{Hir.symbolNumber id}"
                    declarations <- declarations @
                        [ ({ Id = id
                             Name = name
                             Scheme = Types.generalize Map.empty expression.Type
                             IsRecursive = false
                             IsExported = false
                             Span = value.Span
                             Value = value }: Hir.Declaration) ]
                    entry <- Some { Type = expression.Type; Span = value.Span; Node = Hir.Reference id }
                | Error diagnostics -> errors <- errors @ diagnostics
            | TypeInfer.TSLetPattern(pattern, expression, bindings, isExported, span) ->
                let bindingType = expressionType state expression (Types.freshVar())
                match lowerExpression state topLevel bindingType expression, lowerPattern state bindingType pattern with
                | Ok value, Ok(loweredPattern, patternBindings) ->
                    let temporaryId = newSymbol state
                    declarations <- declarations @
                        [ ({ Id = temporaryId
                             Name = $"$pattern{Hir.symbolNumber temporaryId}"
                             Scheme = Types.generalize Map.empty bindingType
                             IsRecursive = false
                             IsExported = false
                             Span = span
                             Value = value }: Hir.Declaration) ]
                    let target = ({ Type = bindingType; Span = span; Node = Hir.Reference temporaryId }: Hir.Expression)
                    let unitBody = ({ Type = TUnit; Span = span; Node = Hir.Constant Hir.Unit }: Hir.Expression)
                    let check = ({ Type = TUnit; Span = span; Node = Hir.Match(target, [ { Pattern = loweredPattern; Guard = None; Body = unitBody; Span = span } ]) }: Hir.Expression)
                    let checkId = newSymbol state
                    declarations <- declarations @
                        [ ({ Id = checkId; Name = $"$patternCheck{Hir.symbolNumber checkId}"; Scheme = Forall([], TUnit); IsRecursive = false; IsExported = false; Span = span; Value = check }: Hir.Declaration) ]
                    for name, inferred in bindings |> Map.toList do
                        match patternBindings |> Map.tryFind name with
                        | None -> errors <- errors @ [ Diagnostic.hir "FSH1006" $"Pattern binding '{name}' was not resolved." (Some span) ]
                        | Some patternId ->
                            let body = ({ Type = inferred; Span = span; Node = Hir.Reference patternId }: Hir.Expression)
                            let projection = ({ Type = inferred; Span = span; Node = Hir.Match(target, [ { Pattern = loweredPattern; Guard = None; Body = body; Span = span } ]) }: Hir.Expression)
                            declarations <- declarations @
                                [ ({ Id = Map.find name topLevel; Name = name; Scheme = Types.generalize Map.empty inferred; IsRecursive = false; IsExported = isExported; Span = span; Value = projection }: Hir.Declaration) ]
                | Error a, Error b -> errors <- errors @ a @ b
                | Error diagnostics, _ | _, Error diagnostics -> errors <- errors @ diagnostics
            | TypeInfer.TSLetRecGroup(bindings, isExported, groupSpan) ->
                for name, expression, inferred, span in bindings do
                    match lowerExpression state topLevel inferred expression with
                    | Ok value ->
                        declarations <- declarations @
                            [ ({ Id = Map.find name topLevel
                                 Name = name
                                 Scheme = Types.generalize Map.empty inferred
                                 IsRecursive = true
                                 IsExported = isExported
                                 Span = span
                                 Value = value }: Hir.Declaration) ]
                    | Error diagnostics -> errors <- errors @ diagnostics
            | TypeInfer.TSType _ -> ()
        if errors.IsEmpty then Ok { Declarations = declarations; Entry = entry }
        else Error errors
