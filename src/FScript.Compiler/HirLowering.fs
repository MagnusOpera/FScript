namespace FScript.Compiler

open FScript.Language

module HirLowering =
    type private State =
        { mutable NextSymbol: int
          mutable AvailableSymbols: TypeInfer.ResolvedSymbol list
          mutable Diagnostics: CompilerDiagnostic list }

    let private newSymbol state =
        let value = state.NextSymbol
        state.NextSymbol <- value + 1
        Hir.SymbolId value

    let private claimSymbol state kind name span =
        match state.AvailableSymbols |> List.tryFind (fun symbol -> symbol.Kind = kind && symbol.Name = name && symbol.Span = span) with
        | Some symbol ->
            state.AvailableSymbols <- state.AvailableSymbols |> List.filter (fun candidate -> candidate.SymbolId <> symbol.SymbolId)
            Hir.SymbolId symbol.SymbolId
        | None ->
            state.Diagnostics <- state.Diagnostics @ [ Diagnostic.hir "FSH1007" $"Resolved symbol '{name}' was not available during HIR lowering." (Some span) ]
            newSymbol state

    let private findSymbol state kind name span =
        state.AvailableSymbols
        |> List.tryFind (fun symbol -> symbol.Kind = kind && symbol.Name = name && symbol.Span = span)
        |> Option.map (fun symbol -> Hir.SymbolId symbol.SymbolId)
        |> Option.defaultWith (fun () -> claimSymbol state kind name span)

    let rec private representationType state = function
        | TFun(argument, result) -> TFun(representationType state argument, representationType state result)
        | TTuple items -> TTuple(List.map (representationType state) items)
        | TList item -> TList(representationType state item)
        | TOption item -> TOption(representationType state item)
        | TMap(key, value) -> TMap(representationType state key, representationType state value)
        | TTask item -> TTask(representationType state item)
        | TRecord fields -> TRecord(fields |> Map.map (fun _ -> representationType state))
        | TUnion(name, cases) -> TUnion(name, cases |> Map.map (fun _ -> Option.map (representationType state)))
        | TNamed name -> TNamed name
        | value -> value

    let private resolvedOperation = function
        | TypeInfer.IntegerAddition -> Hir.IntAdd
        | TypeInfer.FloatingAddition -> Hir.FloatAdd
        | TypeInfer.IntegerSubtraction -> Hir.IntSubtract
        | TypeInfer.FloatingSubtraction -> Hir.FloatSubtract
        | TypeInfer.IntegerMultiplication -> Hir.IntMultiply
        | TypeInfer.FloatingMultiplication -> Hir.FloatMultiply
        | TypeInfer.IntegerDivision -> Hir.IntDivide
        | TypeInfer.FloatingDivision -> Hir.FloatDivide
        | TypeInfer.IntegerRemainder -> Hir.IntRemainder
        | TypeInfer.StructuralEquality -> Hir.StructuralEqual
        | TypeInfer.NumericComparison "<" -> Hir.NumericLessThan
        | TypeInfer.NumericComparison ">" -> Hir.NumericGreaterThan
        | TypeInfer.NumericComparison "<=" -> Hir.NumericLessThanOrEqual
        | TypeInfer.NumericComparison ">=" -> Hir.NumericGreaterThanOrEqual
        | TypeInfer.BooleanOperation "&&" -> Hir.BooleanAnd
        | TypeInfer.BooleanOperation "||" -> Hir.BooleanOr
        | value -> invalidOp $"Unsupported resolved primitive operation '{value}'."

    let private resolvedLiteral = function
        | TypeInfer.ResolvedUnit -> Hir.Unit
        | TypeInfer.ResolvedInt value -> Hir.Int value
        | TypeInfer.ResolvedFloat value -> Hir.Float value
        | TypeInfer.ResolvedBool value -> Hir.Bool value
        | TypeInfer.ResolvedString value -> Hir.String value

    let rec private lowerResolvedPattern state (pattern: TypeInfer.ResolvedPattern) : Hir.Pattern * Map<string, Hir.SymbolId> =
        let make node = ({ Type = representationType state pattern.Type; Span = pattern.Span; Node = node }: Hir.Pattern)
        match pattern.Node with
        | TypeInfer.ResolvedWildcard -> make Hir.Wildcard, Map.empty
        | TypeInfer.ResolvedPatternBinding binding ->
            let id = Hir.SymbolId binding.SymbolId
            make (Hir.Binding(id, binding.Name)), Map.ofList [ binding.Name, id ]
        | TypeInfer.ResolvedLiteralPattern value -> make (Hir.LiteralPattern(resolvedLiteral value)), Map.empty
        | TypeInfer.ResolvedEmptyList -> make Hir.EmptyList, Map.empty
        | TypeInfer.ResolvedNonePattern -> make Hir.NonePattern, Map.empty
        | TypeInfer.ResolvedSomePattern item ->
            let lowered, bindings = lowerResolvedPattern state item
            make (Hir.SomePattern lowered), bindings
        | TypeInfer.ResolvedConsPattern(head, tail) ->
            let loweredHead, headBindings = lowerResolvedPattern state head
            let loweredTail, tailBindings = lowerResolvedPattern state tail
            make (Hir.ConsPattern(loweredHead, loweredTail)), Map.fold (fun env name id -> Map.add name id env) headBindings tailBindings
        | TypeInfer.ResolvedTuplePattern items ->
            let values = List.map (lowerResolvedPattern state) items
            make (Hir.TuplePattern(List.map fst values)), values |> List.map snd |> List.fold (Map.fold (fun env name id -> Map.add name id env)) Map.empty
        | TypeInfer.ResolvedRecordPattern fields ->
            let values = fields |> List.map (fun (name, item) -> let lowered, bindings = lowerResolvedPattern state item in (name, lowered), bindings)
            make (Hir.RecordPattern(List.map fst values)), values |> List.map snd |> List.fold (Map.fold (fun env name id -> Map.add name id env)) Map.empty
        | TypeInfer.ResolvedMapPattern(clauses, tail) ->
            let values = clauses |> List.map (fun (key, value) ->
                let loweredKey, keyBindings = lowerResolvedPattern state key
                let loweredValue, valueBindings = lowerResolvedPattern state value
                (loweredKey, loweredValue), Map.fold (fun env name id -> Map.add name id env) keyBindings valueBindings)
            let loweredTail, tailBindings =
                match tail with
                | Some item -> let lowered, bindings = lowerResolvedPattern state item in Some lowered, bindings
                | None -> None, Map.empty
            let bindings = tailBindings :: List.map snd values |> List.fold (Map.fold (fun env name id -> Map.add name id env)) Map.empty
            make (Hir.MapPattern(List.map fst values, loweredTail)), bindings
        | TypeInfer.ResolvedUnionPattern(typeName, caseName, payload) ->
            match payload with
            | Some item ->
                let lowered, bindings = lowerResolvedPattern state item
                make (Hir.UnionPattern(typeName, caseName, Some lowered)), bindings
            | None -> make (Hir.UnionPattern(typeName, caseName, None)), Map.empty
        | TypeInfer.ResolvedTypePattern patternType -> make (Hir.TypePattern(representationType state patternType)), Map.empty

    let rec private lowerResolvedExpression state (expression: TypeInfer.ResolvedExpression) : Hir.Expression =
        let expressionType = representationType state expression.Type
        let make node = ({ Type = expressionType; Span = expression.Span; Node = node }: Hir.Expression)
        let lower = lowerResolvedExpression state
        match expression.Node with
        | TypeInfer.ResolvedConstant value -> make (Hir.Constant(resolvedLiteral value))
        | TypeInfer.ResolvedReference reference -> make (Hir.Reference(Hir.SymbolId reference.SymbolId))
        | TypeInfer.ResolvedLambda(binding, body) ->
            make (Hir.Lambda(Hir.SymbolId binding.SymbolId, binding.Name, representationType state binding.Type, lower body))
        | TypeInfer.ResolvedApply(fn, argument) ->
            let loweredArgument = lower argument
            let argumentWithEvidence =
                expression.RecordCoercion
                |> Option.map (fun evidence ->
                    let targetType = representationType state evidence.TargetType
                    ({ Type = targetType; Span = loweredArgument.Span; Node = Hir.RecordConvert(loweredArgument, targetType) }: Hir.Expression))
                |> Option.defaultValue loweredArgument
            make (Hir.Apply(lower fn, argumentWithEvidence))
        | TypeInfer.ResolvedPipe(argument, fn) ->
            let loweredArgument = lower argument
            let argumentWithEvidence =
                expression.RecordCoercion
                |> Option.map (fun evidence ->
                    let targetType = representationType state evidence.TargetType
                    ({ Type = targetType; Span = loweredArgument.Span; Node = Hir.RecordConvert(loweredArgument, targetType) }: Hir.Expression))
                |> Option.defaultValue loweredArgument
            make (Hir.Pipe(argumentWithEvidence, lower fn))
        | TypeInfer.ResolvedIf(condition, whenTrue, whenFalse) -> make (Hir.If(lower condition, lower whenTrue, lower whenFalse))
        | TypeInfer.ResolvedPrimitive(operation, arguments) -> make (Hir.Primitive(resolvedOperation operation, List.map lower arguments))
        | TypeInfer.ResolvedTuple items -> make (Hir.Tuple(List.map lower items))
        | TypeInfer.ResolvedList items -> make (Hir.List(List.map lower items))
        | TypeInfer.ResolvedRecord fields -> make (Hir.Record(fields |> List.map (fun (name, value) -> name, lower value)))
        | TypeInfer.ResolvedFieldGet(target, name) -> make (Hir.FieldGet(lower target, name))
        | TypeInfer.ResolvedSome item -> make (Hir.SomeValue(lower item))
        | TypeInfer.ResolvedNone -> make Hir.NoneValue
        | TypeInfer.ResolvedCons(head, tail) -> make (Hir.Cons(lower head, lower tail))
        | TypeInfer.ResolvedAppend(left, right) -> make (Hir.Append(lower left, lower right))
        | TypeInfer.ResolvedLet(binding, recursive, value, body) ->
            make (Hir.Let(Hir.SymbolId binding.SymbolId, binding.Name, recursive, lower value, lower body))
        | TypeInfer.ResolvedLetRecursiveGroup(bindings, body) ->
            let values = bindings |> List.map (fun binding -> Hir.SymbolId binding.Binding.SymbolId, binding.Binding.Name, representationType state binding.Binding.Type, lower binding.Value)
            make (Hir.LetRecursiveGroup(values, lower body))
        | TypeInfer.ResolvedFail value -> make (Hir.Fail(lower value))
        | TypeInfer.ResolvedMatch(target, cases) ->
            let loweredCases =
                cases |> List.map (fun case ->
                    let pattern, _ = lowerResolvedPattern state case.Pattern
                    ({ Pattern = pattern; Guard = Option.map lower case.Guard; Body = lower case.Body; Span = case.Span }: Hir.MatchCase))
            make (Hir.Match(lower target, loweredCases))
        | TypeInfer.ResolvedRange(first, last) -> make (Hir.Range(lower first, lower last))
        | TypeInfer.ResolvedRecordUpdate(preserveTargetRepresentation, target, fields) ->
            let loweredTarget = lower target
            let resultType = if preserveTargetRepresentation then loweredTarget.Type else expressionType
            ({ Type = resultType
               Span = expression.Span
               Node = Hir.RecordUpdate(loweredTarget, fields |> List.map (fun (name, value) -> name, lower value)) }: Hir.Expression)
        | TypeInfer.ResolvedIndexGet(target, index) -> make (Hir.IndexGet(lower target, lower index))
        | TypeInfer.ResolvedInterpolate parts ->
            parts |> List.map (function TypeInfer.ResolvedTextPart text -> Hir.TextPart text | TypeInfer.ResolvedValuePart value -> Hir.ValuePart(lower value)) |> Hir.Interpolate |> make
        | TypeInfer.ResolvedMap entries ->
            entries |> List.map (function TypeInfer.ResolvedKeyValue(key, value) -> Hir.KeyValue(lower key, lower value) | TypeInfer.ResolvedSpread value -> Hir.Spread(lower value)) |> Hir.Map |> make
        | TypeInfer.ResolvedForEach(binding, source, body) ->
            make (Hir.ForEach(Hir.SymbolId binding.SymbolId, binding.Name, representationType state binding.Type, lower source, lower body))
        | TypeInfer.ResolvedTypeToken tokenType -> make (Hir.TypeToken(representationType state tokenType))

    let lower (externs: CompilerExtern list) (resolved: TypeInfer.ResolvedProgram) : Result<Hir.Program, CompilerDiagnostic list> =
        let exportMetadata name span enabled = if enabled then Some({ Name = name; Span = span }: Hir.ExportMetadata) else None
        let attributesOf (statement: TypeInfer.ResolvedStatement) =
            statement.Attributes
            |> List.map (fun attribute -> ({ Name = attribute.Name; Arguments = attribute.Arguments; Span = attribute.Span }: Hir.AttributeMetadata))
        let nextSynthetic = resolved.Symbols |> List.map _.SymbolId |> function [] -> 0 | values -> List.max values + 1
        let state =
            { NextSymbol = nextSynthetic
              AvailableSymbols = resolved.Symbols
              Diagnostics = [] }
        let resolvedSchemes =
            resolved.Symbols
            |> List.choose (fun symbol -> symbol.Scheme |> Option.map (fun scheme -> Hir.SymbolId symbol.SymbolId, scheme))
            |> Map.ofList
        let schemeFor id fallback = resolvedSchemes |> Map.tryFind id |> Option.defaultValue fallback
        let syntheticSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)
        let builtins =
            BuiltinSignatures.builtinSchemes
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (name, _) -> name, findSymbol state TypeInfer.BuiltinSymbol name syntheticSpan)
            |> Map.ofList
        let externSymbols = externs |> List.sortBy _.Name |> List.map (fun value -> value.Name, findSymbol state TypeInfer.ExternSymbol value.Name syntheticSpan) |> Map.ofList
        let typeDefinitions =
            resolved.ResolvedStatements
            |> List.choose (fun statement ->
                match statement.Node with
                | TypeInfer.ResolvedTypeDeclaration definition -> Some(statement, definition)
                | _ -> None)
        let unionDefinitions =
            typeDefinitions
            |> List.choose (fun (statement, definition) ->
                match definition.Definition with
                | TUnion(name, cases) -> Some(statement, definition, name, cases)
                | _ -> None)
        let constructorSymbols =
            unionDefinitions
            |> List.collect (fun (_, definition, name, cases) ->
                cases
                |> Map.toList
                |> List.map (fun (caseName, _) ->
                    name, caseName, findSymbol state TypeInfer.UnionCaseSymbol $"{name}.{caseName}" definition.Binding.Span))
        let mutable declarations =
            let builtinDeclarations =
                BuiltinSignatures.builtinSchemes |> Map.toList |> List.sortBy fst |> List.map (fun (name, scheme) ->
                    let (Forall(_, valueType)) = scheme
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    ({ Id = builtins.[name]; Name = name; Scheme = schemeFor builtins.[name] scheme; Attributes = []; IsRecursive = false; IsExported = false; Export = None; Span = span; Value = { Type = valueType; Span = span; Node = Hir.Intrinsic name } }: Hir.Declaration))
            let externalDeclarations =
                externs |> List.sortBy _.Name |> List.map (fun external ->
                    let (Forall(_, valueType)) = external.Scheme
                    let span = Span.mk (Span.pos 0 0) (Span.pos 0 0)
                    let node =
                        match external.Kind with
                        | RuntimeContextIntrinsic -> Hir.RuntimeContext
                        | PortableRuntimeIntrinsic -> Hir.Intrinsic external.Name
                        | UnsupportedHostExtern -> Hir.HostExternal(external.Name, external.Arity)
                    ({ Id = externSymbols.[external.Name]; Name = external.Name; Scheme = schemeFor externSymbols.[external.Name] external.Scheme; Attributes = []; IsRecursive = false; IsExported = false; Export = None; Span = span; Value = { Type = valueType; Span = span; Node = node } }: Hir.Declaration))
            builtinDeclarations @ externalDeclarations @ (constructorSymbols |> List.map (fun (typeName, caseName, id) ->
                let _, definition, _, cases = unionDefinitions |> List.find (fun (_, _, name, _) -> name = typeName)
                let unionType = TNamed typeName
                let payloadType = cases.[caseName]
                let span = definition.Binding.Span
                let value =
                    match payloadType with
                    | None -> ({ Type = unionType; Span = span; Node = Hir.UnionCase(typeName, caseName, None) }: Hir.Expression)
                    | Some itemType ->
                        let parameter = newSymbol state
                        let body = ({ Type = unionType; Span = span; Node = Hir.UnionCase(typeName, caseName, Some({ Type = itemType; Span = span; Node = Hir.Reference parameter })) }: Hir.Expression)
                        ({ Type = TFun(itemType, unionType); Span = span; Node = Hir.Lambda(parameter, "value", itemType, body) }: Hir.Expression)
                ({ Id = id; Name = $"{typeName}.{caseName}"; Scheme = Types.generalize Map.empty value.Type; Attributes = []; IsRecursive = false; IsExported = false; Export = None; Span = span; Value = value }: Hir.Declaration)))
        let mutable entry: Hir.Expression option = None
        let mutable errors = []
        for resolvedStatement in resolved.ResolvedStatements do
            let isExported = resolvedStatement.Export.IsSome
            match resolvedStatement.Node with
            | TypeInfer.ResolvedValueDeclaration declaration ->
                let binding = declaration.Binding
                let value = lowerResolvedExpression state declaration.Value
                declarations <- declarations @
                    [ ({ Id = Hir.SymbolId binding.SymbolId
                         Name = binding.Name
                         Scheme = declaration.Scheme
                         Attributes = attributesOf resolvedStatement
                         IsRecursive = declaration.IsRecursive
                         IsExported = isExported
                         Export = exportMetadata binding.Name binding.Span isExported
                         Span = binding.Span
                         Value = value }: Hir.Declaration) ]
            | TypeInfer.ResolvedExpressionStatement expression ->
                let value = lowerResolvedExpression state expression
                let id = newSymbol state
                let name = $"$expression{Hir.symbolNumber id}"
                declarations <- declarations @
                    [ ({ Id = id
                         Name = name
                         Scheme = Types.generalize Map.empty value.Type
                         Attributes = []
                         IsRecursive = false
                         IsExported = false
                         Export = None
                         Span = value.Span
                         Value = value }: Hir.Declaration) ]
                entry <- Some { Type = value.Type; Span = value.Span; Node = Hir.Reference id }
            | TypeInfer.ResolvedPatternDeclaration declaration ->
                let span = resolvedStatement.Span
                let value = lowerResolvedExpression state declaration.Value
                let loweredPattern, patternBindings = lowerResolvedPattern state declaration.Pattern
                let bindingType = value.Type
                let temporaryId = newSymbol state
                declarations <- declarations @
                    [ ({ Id = temporaryId
                         Name = $"$pattern{Hir.symbolNumber temporaryId}"
                         Scheme = Types.generalize Map.empty bindingType
                         Attributes = []
                         IsRecursive = false
                         IsExported = false
                         Export = None
                         Span = span
                         Value = value }: Hir.Declaration) ]
                let target = ({ Type = bindingType; Span = span; Node = Hir.Reference temporaryId }: Hir.Expression)
                let unitBody = ({ Type = TUnit; Span = span; Node = Hir.Constant Hir.Unit }: Hir.Expression)
                let check = ({ Type = TUnit; Span = span; Node = Hir.Match(target, [ { Pattern = loweredPattern; Guard = None; Body = unitBody; Span = span } ]) }: Hir.Expression)
                let checkId = newSymbol state
                declarations <- declarations @
                    [ ({ Id = checkId; Name = $"$patternCheck{Hir.symbolNumber checkId}"; Scheme = Forall([], TUnit); Attributes = []; IsRecursive = false; IsExported = false; Export = None; Span = span; Value = check }: Hir.Declaration) ]
                for binding, scheme in declaration.Bindings do
                    let name = binding.Name
                    let inferred = representationType state binding.Type
                    match patternBindings |> Map.tryFind name with
                    | None -> errors <- errors @ [ Diagnostic.hir "FSH1006" $"Pattern binding '{name}' was not resolved." (Some span) ]
                    | Some patternId ->
                        let body = ({ Type = inferred; Span = span; Node = Hir.Reference patternId }: Hir.Expression)
                        let projection = ({ Type = inferred; Span = span; Node = Hir.Match(target, [ { Pattern = loweredPattern; Guard = None; Body = body; Span = span } ]) }: Hir.Expression)
                        declarations <- declarations @
                            [ ({ Id = Hir.SymbolId binding.SymbolId; Name = name; Scheme = scheme; Attributes = attributesOf resolvedStatement; IsRecursive = false; IsExported = isExported; Export = (exportMetadata name span isExported); Span = span; Value = projection }: Hir.Declaration) ]
            | TypeInfer.ResolvedRecursiveDeclarations recursiveDeclarations ->
                for declaration in recursiveDeclarations do
                    let binding = declaration.Binding
                    let value = lowerResolvedExpression state declaration.Value
                    declarations <- declarations @
                        [ ({ Id = Hir.SymbolId binding.SymbolId
                             Name = binding.Name
                             Scheme = declaration.Scheme
                             Attributes = attributesOf resolvedStatement
                             IsRecursive = true
                             IsExported = isExported
                             Export = exportMetadata binding.Name binding.Span isExported
                             Span = binding.Span
                             Value = value }: Hir.Declaration) ]
            | TypeInfer.ResolvedImportedDeclaration declaration ->
                let binding = declaration.Binding
                declarations <- declarations @
                    [ ({ Id = Hir.SymbolId binding.SymbolId
                         Name = binding.Name
                         Scheme = declaration.Scheme
                         Attributes = attributesOf resolvedStatement
                         IsRecursive = false
                         IsExported = false
                         Export = None
                         Span = binding.Span
                         Value = { Type = representationType state binding.Type; Span = binding.Span; Node = Hir.TargetImport(declaration.Import.ModulePath, declaration.Import.MemberName) } }: Hir.Declaration) ]
            | TypeInfer.ResolvedTypeDeclaration _ -> ()
        let importedRecords =
            typeDefinitions
            |> List.choose (fun (_, definition) ->
                definition.Import
                |> Option.map (fun import ->
                    ({ Name = definition.Binding.Name
                       ModulePath = import.ModulePath
                       MemberName = import.MemberName
                       IsUnion = match definition.Definition with TUnion _ -> true | _ -> false
                       Fields = definition.Fields
                       Span = definition.Binding.Span }: Hir.ImportedRecord)))
        let hirTypes =
            typeDefinitions
            |> List.map (fun (statement, definition) ->
                ({ Id = Hir.SymbolId definition.Binding.SymbolId
                   Name = definition.Binding.Name
                   IsRecursive = definition.IsRecursive
                   Definition = definition.Definition
                   Attributes = attributesOf statement
                   Import = importedRecords |> List.tryFind (fun imported -> imported.Name = definition.Binding.Name)
                   Span = definition.Binding.Span }: Hir.TypeDeclaration))
        let diagnostics = errors @ state.Diagnostics
        if diagnostics.IsEmpty then Ok { Types = hirTypes; ImportedRecords = importedRecords; Declarations = declarations; Entry = entry }
        else Error diagnostics
