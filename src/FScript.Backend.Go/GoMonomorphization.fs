namespace FScript.Compiler

open System
open System.Collections.Generic
open FScript.Language

module GoMonomorphization =
    exception private Failure of CompilerDiagnostic

    type private State =
        { Program: Lir.Program
          Declarations: Map<Lir.SymbolId, Lir.Declaration>
          Functions: Map<Lir.FunctionId, Lir.Function>
          DeclarationKeys: Dictionary<GoSpecialization.SpecializationKey, Lir.SymbolId>
          FunctionKeys: Dictionary<string, Lir.FunctionId>
          OutputDeclarations: ResizeArray<Lir.Declaration>
          OutputFunctions: ResizeArray<Lir.Function>
          mutable NextSymbol: int
          mutable NextFunction: int }

    let private diagnostic code message span =
        { Code = code; Stage = GoGeneration; Message = message; Span = Some span }

    let private fail code message span = raise (Failure(diagnostic code message span))
    let private symbolNumber (Lir.SymbolId value) = value
    let private functionNumber (Lir.FunctionId value) = value

    let private freshSymbol state =
        let value = Lir.SymbolId state.NextSymbol
        state.NextSymbol <- state.NextSymbol + 1
        value

    let private freshFunction state =
        let value = Lir.FunctionId state.NextFunction
        state.NextFunction <- state.NextFunction + 1
        value

    let private mergeSubstitution left right = right |> Map.fold (fun state key value -> Map.add key value state) left
    let private closedType span value =
        if GoSpecialization.containsVariable value then
            fail "FSB4007" $"Reachable value has open type '{Types.typeToString value}' after Go specialization." span
        value

    let private bindingKey value = Types.typeToString value
    type private Environment = Map<Lir.SymbolId, Map<string, Lir.SymbolId>>

    let private bind original concrete replacement environment =
        let values = environment |> Map.tryFind original |> Option.defaultValue Map.empty
        environment |> Map.add original (values |> Map.add (bindingKey concrete) replacement)

    let private lookup span original concrete environment =
        environment
        |> Map.tryFind original
        |> Option.bind (Map.tryFind (bindingKey concrete))
        |> Option.defaultWith (fun () -> fail "FSB4007" $"No concrete specialization exists for local symbol {symbolNumber original} at '{Types.typeToString concrete}'." span)

    let rec private patternBindingIds (plan: Lir.PatternPlan) =
        [ yield! plan.Bindings |> List.map _.Id
          for mapPlan in plan.Maps do
              for clause in mapPlan.Clauses do
                  yield! patternBindingIds clause.Key
                  yield! patternBindingIds clause.Value
              yield! mapPlan.Tail |> Option.map patternBindingIds |> Option.defaultValue [] ]

    let rec private references bound (expression: Lir.Expression) =
        let nested = references bound
        let many values = values |> List.map nested |> Set.unionMany
        match expression.Node with
        | Lir.Load id -> if Set.contains id bound then Set.empty else Set.singleton id
        | Lir.MakeClosure(_, captures) -> Set.difference (Set.ofList captures) bound
        | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right)
        | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> Set.union (nested left) (nested right)
        | Lir.Branch(condition, whenTrue, whenFalse) -> many [ condition; whenTrue; whenFalse ]
        | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> many values
        | Lir.Record fields -> fields |> List.map (snd >> nested) |> Set.unionMany
        | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> nested target
        | Lir.Let(id, _, recursive, binding, body) ->
            Set.union (references (if recursive then Set.add id bound else bound) binding) (references (Set.add id bound) body)
        | Lir.LetRecursiveGroup(bindings, body) ->
            let scope = bindings |> List.fold (fun current (id, _, _, _) -> Set.add id current) bound
            references scope body :: (bindings |> List.map (fun (_, _, _, value) -> references scope value)) |> Set.unionMany
        | Lir.DecisionTree(target, cases) ->
            nested target :: (cases |> List.collect (fun item ->
                let scope = Set.union bound (patternBindingIds item.Plan |> Set.ofList)
                [ yield! item.Guard |> Option.map (references scope) |> Option.toList; references scope item.Body ])) |> Set.unionMany
        | Lir.RecordUpdate(target, fields) -> nested target :: (fields |> List.map (snd >> nested)) |> Set.unionMany
        | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart value -> Some(nested value)) |> Set.unionMany
        | Lir.UnionCase(_, _, payload) -> payload |> Option.map nested |> Option.defaultValue Set.empty
        | Lir.Map entries -> entries |> List.map (function Lir.KeyValue(key, value) -> Set.union (nested key) (nested value) | Lir.Spread value -> nested value) |> Set.unionMany
        | Lir.ForEach(id, _, _, source, body) -> Set.union (nested source) (references (Set.add id bound) body)
        | _ -> Set.empty

    let private collectLoads target substitution expression =
        let results = ResizeArray<Type>()
        let rec visit (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id when id = target -> results.Add(GoSpecialization.apply substitution value.Type)
            | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right)
            | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> visit left; visit right
            | Lir.Branch(a, b, c) -> visit a; visit b; visit c
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> List.iter visit values
            | Lir.Record fields | Lir.RecordUpdate(_, fields) -> fields |> List.iter (snd >> visit); match value.Node with Lir.RecordUpdate(target, _) -> visit target | _ -> ()
            | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> visit target
            | Lir.Let(_, _, _, binding, body) -> visit binding; visit body
            | Lir.LetRecursiveGroup(bindings, body) -> bindings |> List.iter (fun (_, _, _, item) -> visit item); visit body
            | Lir.DecisionTree(target, cases) -> visit target; cases |> List.iter (fun item -> Option.iter visit item.Guard; visit item.Body)
            | Lir.Interpolate parts -> parts |> List.iter (function Lir.TextPart _ -> () | Lir.ValuePart item -> visit item)
            | Lir.UnionCase(_, _, payload) -> Option.iter visit payload
            | Lir.Map entries -> entries |> List.iter (function Lir.KeyValue(key, item) -> visit key; visit item | Lir.Spread item -> visit item)
            | Lir.ForEach(_, _, _, source, body) -> visit source; visit body
            | _ -> ()
        visit expression
        results |> Seq.distinct |> Seq.sortBy Types.typeToString |> Seq.toList

    let specialize (program: Lir.Program) : Result<Lir.Program, CompilerDiagnostic list> =
        try
            let allSymbols =
                [ yield! program.Declarations |> List.map (_.Id >> symbolNumber)
                  yield! program.Types |> List.map (_.Id >> symbolNumber)
                  for fn in program.Functions do yield symbolNumber fn.Parameter ]
            let state =
                { Program = program
                  Declarations = program.Declarations |> List.map (fun item -> item.Id, item) |> Map.ofList
                  Functions = program.Functions |> List.map (fun item -> item.Id, item) |> Map.ofList
                  DeclarationKeys = Dictionary()
                  FunctionKeys = Dictionary()
                  OutputDeclarations = ResizeArray()
                  OutputFunctions = ResizeArray()
                  NextSymbol = (if List.isEmpty allSymbols then 1 else List.max allSymbols + 1)
                  NextFunction = (program.Functions |> List.map (_.Id >> functionNumber) |> function [] -> 1 | values -> List.max values + 1) }

            let rec transformPattern substitution environment (plan: Lir.PatternPlan) =
                let mutable scoped = environment
                let bindings =
                    plan.Bindings |> List.map (fun binding ->
                        let concrete = GoSpecialization.apply substitution binding.Type |> closedType plan.Span
                        let replacement = freshSymbol state
                        scoped <- bind binding.Id concrete replacement scoped
                        { binding with Id = replacement; Type = concrete })
                let checks =
                    plan.Checks |> List.map (function
                        | Lir.TypeCheck(path, valueType) -> Lir.TypeCheck(path, GoSpecialization.apply substitution valueType)
                        | value -> value)
                let maps =
                    plan.Maps |> List.map (fun mapPlan ->
                        let clauses =
                            mapPlan.Clauses |> List.map (fun clause ->
                                let key, keyEnvironment = transformPattern substitution scoped clause.Key
                                scoped <- keyEnvironment
                                let value, valueEnvironment = transformPattern substitution scoped clause.Value
                                scoped <- valueEnvironment
                                { clause with Key = key; Value = value })
                        let tail =
                            mapPlan.Tail |> Option.map (fun tail ->
                                let transformed, tailEnvironment = transformPattern substitution scoped tail
                                scoped <- tailEnvironment
                                transformed)
                        { mapPlan with Clauses = clauses; Tail = tail })
                { plan with Checks = checks; Bindings = bindings; Maps = maps }, scoped

            and requestFunction substitution environment original concrete span =
                let concrete = closedType span concrete
                let key = $"{functionNumber original}:{Types.typeToString concrete}"
                match state.FunctionKeys.TryGetValue key with
                | true, existing -> existing
                | _ ->
                    let source = state.Functions.[original]
                    let template = GoSpecialization.apply substitution (TFun(source.ParameterType, source.ReturnType))
                    let inferred =
                        match GoSpecialization.unifyInstantiation template concrete with
                        | Ok value -> mergeSubstitution substitution value
                        | Error error -> fail "FSB4007" error span
                    let replacement = freshFunction state
                    state.FunctionKeys.Add(key, replacement)
                    let parameterType = GoSpecialization.apply inferred source.ParameterType |> closedType source.Span
                    let parameter = freshSymbol state
                    let bodyEnvironment = bind source.Parameter parameterType parameter environment
                    let body = transform inferred bodyEnvironment source.Body
                    let captures = references (Set.singleton parameter) body |> Set.toList |> List.sortBy symbolNumber
                    state.OutputFunctions.Add
                        { source with Id = replacement; Parameter = parameter; ParameterType = parameterType
                                      ReturnType = body.Type; Captures = captures; Body = body }
                    replacement

            and requestDeclaration original concrete span =
                let source = state.Declarations.[original]
                let rec rootName (value: Lir.Expression) =
                    match value.Node with
                    | Lir.Load id when state.Declarations.ContainsKey id -> Some state.Declarations.[id].Name
                    | Lir.CallClosure(fn, _) -> rootName fn
                    | _ -> None
                let rec finalArgument (value: Lir.Expression) = match value.Node with Lir.CallClosure(_, argument) -> argument | _ -> value
                let isBottom =
                    match source.Value.Node with
                    | Lir.Fail _ -> true
                    | Lir.CallClosure _ when rootName source.Value = Some "List.tail" -> match (finalArgument source.Value).Node with Lir.List [] -> true | _ -> false
                    | _ -> false
                let concrete =
                    (if isBottom then
                         let defaults = Types.ftvType concrete |> Seq.map (fun variable -> variable, TUnit) |> Map.ofSeq
                         GoSpecialization.apply defaults concrete
                     else concrete)
                    |> closedType span
                let key = GoSpecialization.specializationKey (symbolNumber original) concrete
                match state.DeclarationKeys.TryGetValue key with
                | true, existing -> existing
                | _ ->
                    let inferred =
                        match GoSpecialization.unifyInstantiation source.Type concrete with
                        | Ok value -> value
                        | Error error -> fail "FSB4007" error span
                    if GoSpecialization.containsVariable source.Type && GoSpecialization.effect source.Value = GoSpecialization.Observable && not isBottom then
                        fail "FSB4008" $"Generalized binding '{source.Name}' has observable initialization and cannot be cloned safely." source.Span
                    let replacement = freshSymbol state
                    state.DeclarationKeys.Add(key, replacement)
                    let value = transform inferred Map.empty source.Value
                    state.OutputDeclarations.Add { source with Id = replacement; Type = concrete; Value = value }
                    replacement

            and transform substitution environment (expression: Lir.Expression) : Lir.Expression =
                let rec rootDeclarationName (value: Lir.Expression) =
                    match value.Node with
                    | Lir.Load id when state.Declarations.ContainsKey id -> Some state.Declarations.[id].Name
                    | Lir.CallClosure(fn, _) -> rootDeclarationName fn
                    | _ -> None
                let rec isBottom (value: Lir.Expression) =
                    match value.Node with
                    | Lir.Fail _ -> true
                    | Lir.CallClosure _ when rootDeclarationName value = Some "List.tail" ->
                        let rec finalArgument (current: Lir.Expression) = match current.Node with Lir.CallClosure(_, argument) -> argument | _ -> current
                        match (finalArgument value).Node with Lir.List [] -> true | _ -> false
                    | Lir.Branch(_, whenTrue, whenFalse) -> isBottom whenTrue && isBottom whenFalse
                    | Lir.DecisionTree(_, cases) -> not cases.IsEmpty && cases |> List.forall (_.Body >> isBottom)
                    | _ -> false
                let representationInsensitive =
                    match expression.Node with
                    | Lir.Operation(Lir.StructuralEqual, values) -> values |> List.exists (_.Type >> GoSpecialization.containsVariable)
                    | Lir.CallClosure _ ->
                        match rootDeclarationName expression with
                        | Some("Option.isNone" | "Option.isSome" | "Map.count") -> true
                        | _ -> isBottom expression
                    | _ -> isBottom expression
                let rec expressionVariables (value: Lir.Expression) =
                    let own = Types.ftvType (GoSpecialization.apply substitution value.Type)
                    let children =
                        match value.Node with
                        | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right) | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> [ left; right ]
                        | Lir.Branch(a, b, c) -> [ a; b; c ]
                        | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> values
                        | Lir.Record fields -> List.map snd fields
                        | Lir.FieldGet(value, _) | Lir.SomeValue value | Lir.Fail value | Lir.RecordConvert(value, _) -> [ value ]
                        | Lir.Let(_, _, _, binding, body) -> [ binding; body ]
                        | Lir.LetRecursiveGroup(bindings, body) -> (bindings |> List.map (fun (_, _, _, item) -> item)) @ [ body ]
                        | Lir.DecisionTree(target, cases) -> target :: (cases |> List.collect (fun item -> (Option.toList item.Guard) @ [ item.Body ]))
                        | Lir.RecordUpdate(target, fields) -> target :: List.map snd fields
                        | Lir.Interpolate parts -> parts |> List.choose (function Lir.ValuePart item -> Some item | _ -> None)
                        | Lir.UnionCase(_, _, payload) -> Option.toList payload
                        | Lir.Map entries -> entries |> List.collect (function Lir.KeyValue(key, item) -> [ key; item ] | Lir.Spread item -> [ item ])
                        | Lir.ForEach(_, _, _, source, body) -> [ source; body ]
                        | _ -> []
                    children |> List.fold (fun variables child -> Set.union variables (expressionVariables child)) own
                let effectiveSubstitution =
                    if representationInsensitive then
                        expressionVariables expression
                        |> Set.fold (fun current variable -> Map.add variable TUnit current) substitution
                    else substitution
                let concreteType = GoSpecialization.apply effectiveSubstitution expression.Type
                let recurse = transform effectiveSubstitution environment
                let node =
                    match expression.Node with
                    | Lir.Load id when state.Declarations.ContainsKey id ->
                        let declaration = state.Declarations.[id]
                        let requestedType = if GoSpecialization.containsVariable declaration.Type then concreteType else declaration.Type
                        Lir.Load(requestDeclaration id requestedType expression.Span)
                    | Lir.Load id -> Lir.Load(lookup expression.Span id (closedType expression.Span concreteType) environment)
                    | Lir.MakeClosure(functionId, _) ->
                        let specialized = requestFunction effectiveSubstitution environment functionId concreteType expression.Span
                        let fn = state.OutputFunctions |> Seq.find (fun item -> item.Id = specialized)
                        Lir.MakeClosure(specialized, fn.Captures)
                    | Lir.CallClosure(fn, argument) -> Lir.CallClosure(recurse fn, recurse argument)
                    | Lir.PipeClosure(argument, fn) -> Lir.PipeClosure(recurse argument, recurse fn)
                    | Lir.Branch(condition, whenTrue, whenFalse) -> Lir.Branch(recurse condition, recurse whenTrue, recurse whenFalse)
                    | Lir.Operation(operation, values) -> Lir.Operation(operation, List.map recurse values)
                    | Lir.Tuple values ->
                        let itemTypes = match concreteType with TTuple items -> items | _ -> List.map (fun (value: Lir.Expression) -> value.Type) values
                        Lir.Tuple(List.map2 (fun itemType (value: Lir.Expression) ->
                            let valueType = GoSpecialization.apply substitution value.Type
                            let inferred = match GoSpecialization.unifyInstantiation valueType itemType with Ok extra -> mergeSubstitution effectiveSubstitution extra | Error _ -> effectiveSubstitution
                            transform inferred environment value) itemTypes values)
                    | Lir.List values ->
                        let itemType = match concreteType with TList item -> item | _ -> TVar -1
                        Lir.List(values |> List.map (fun (value: Lir.Expression) ->
                            let valueType = GoSpecialization.apply substitution value.Type
                            let inferred = match GoSpecialization.unifyInstantiation valueType itemType with Ok extra -> mergeSubstitution effectiveSubstitution extra | Error _ -> effectiveSubstitution
                            transform inferred environment value))
                    | Lir.Record fields -> Lir.Record(fields |> List.map (fun (name, value) -> name, recurse value))
                    | Lir.FieldGet(target, name) -> Lir.FieldGet(recurse target, name)
                    | Lir.SomeValue value -> Lir.SomeValue(recurse value)
                    | Lir.Cons(head, tail) -> Lir.Cons(recurse head, recurse tail)
                    | Lir.Append(left, right) -> Lir.Append(recurse left, recurse right)
                    | Lir.Let(id, name, recursive, binding, body) ->
                        let bindingType = GoSpecialization.apply effectiveSubstitution binding.Type
                        if GoSpecialization.containsVariable bindingType then
                            let uses = collectLoads id effectiveSubstitution body
                            if uses.IsEmpty then
                                if GoSpecialization.effect binding = GoSpecialization.Observable then
                                    fail "FSB4008" $"Generalized local binding '{name}' has observable initialization and cannot be omitted." binding.Span
                                (transform effectiveSubstitution environment body).Node
                            else
                                if GoSpecialization.effect binding = GoSpecialization.Observable then
                                    fail "FSB4008" $"Generalized local binding '{name}' has observable initialization and cannot be cloned safely." binding.Span
                                let mutable scoped = environment
                                let clones =
                                    uses |> List.map (fun useType ->
                                        let inferred =
                                            match GoSpecialization.unifyInstantiation bindingType useType with
                                            | Ok value -> mergeSubstitution effectiveSubstitution value
                                            | Error error -> fail "FSB4007" error binding.Span
                                        let replacement = freshSymbol state
                                        scoped <- bind id useType replacement scoped
                                        replacement, transform inferred (if recursive then scoped else environment) binding)
                                let transformedBody = transform effectiveSubstitution scoped body
                                (List.foldBack (fun (replacement, value) (current: Lir.Expression) ->
                                    ({ Type = current.Type; Span = expression.Span; Node = Lir.Let(replacement, name, recursive, value, current) }: Lir.Expression)) clones transformedBody).Node
                        else
                            let replacement = freshSymbol state
                            let scoped = bind id bindingType replacement environment
                            Lir.Let(replacement, name, recursive, transform effectiveSubstitution (if recursive then scoped else environment) binding, transform effectiveSubstitution scoped body)
                    | Lir.LetRecursiveGroup(bindings, body) ->
                        let concreteBindings = bindings |> List.map (fun (id, name, valueType, value) -> id, name, GoSpecialization.apply effectiveSubstitution valueType, value)
                        if concreteBindings |> List.exists (fun (_, _, valueType, _) -> GoSpecialization.containsVariable valueType) then
                            fail "FSB4007" "A reachable local recursive group remains polymorphic after specialization." expression.Span
                        let replacements = concreteBindings |> List.map (fun (id, _, valueType, _) -> id, valueType, freshSymbol state)
                        let scoped = replacements |> List.fold (fun current (id, valueType, replacement) -> bind id valueType replacement current) environment
                        Lir.LetRecursiveGroup(
                            List.map2 (fun (_, name, valueType, value) (_, _, replacement) -> replacement, name, valueType, transform effectiveSubstitution scoped value) concreteBindings replacements,
                            transform effectiveSubstitution scoped body)
                    | Lir.Fail value -> Lir.Fail(recurse value)
                    | Lir.DecisionTree(target, cases) ->
                        let transformedCases = cases |> List.map (fun item ->
                            let plan, scoped = transformPattern effectiveSubstitution environment item.Plan
                            { item with Plan = plan; Guard = Option.map (transform effectiveSubstitution scoped) item.Guard; Body = transform effectiveSubstitution scoped item.Body })
                        Lir.DecisionTree(recurse target, transformedCases)
                    | Lir.Range(first, last) -> Lir.Range(recurse first, recurse last)
                    | Lir.RecordUpdate(target, fields) -> Lir.RecordUpdate(recurse target, fields |> List.map (fun (name, value) -> name, recurse value))
                    | Lir.RecordConvert(target, targetType) -> Lir.RecordConvert(recurse target, GoSpecialization.apply effectiveSubstitution targetType)
                    | Lir.IndexGet(target, index) -> Lir.IndexGet(recurse target, recurse index)
                    | Lir.Interpolate parts -> Lir.Interpolate(parts |> List.map (function Lir.TextPart text -> Lir.TextPart text | Lir.ValuePart value -> Lir.ValuePart(recurse value)))
                    | Lir.UnionCase(typeName, caseName, payload) -> Lir.UnionCase(typeName, caseName, Option.map recurse payload)
                    | Lir.Map entries -> Lir.Map(entries |> List.map (function Lir.KeyValue(key, value) -> Lir.KeyValue(recurse key, recurse value) | Lir.Spread value -> Lir.Spread(recurse value)))
                    | Lir.ForEach(id, name, itemType, source, body) ->
                        let concreteItem = GoSpecialization.apply effectiveSubstitution itemType |> closedType expression.Span
                        let replacement = freshSymbol state
                        Lir.ForEach(replacement, name, concreteItem, recurse source, transform effectiveSubstitution (bind id concreteItem replacement environment) body)
                    | Lir.TypeToken valueType -> Lir.TypeToken(GoSpecialization.apply effectiveSubstitution valueType)
                    | value -> value
                let resultType =
                    match node with
                    | Lir.Load id -> state.OutputDeclarations |> Seq.tryFind (fun declaration -> declaration.Id = id) |> Option.map _.Type |> Option.defaultValue concreteType
                    | _ -> concreteType
                { expression with Type = resultType; Node = node }

            for declaration in program.Declarations do
                if GoSpecialization.effect declaration.Value = GoSpecialization.Observable && not (GoSpecialization.containsVariable declaration.Type) then
                    requestDeclaration declaration.Id declaration.Type declaration.Span |> ignore
            let entry = program.Entry |> Option.map (transform Map.empty Map.empty)
            Ok
                { program with
                    Functions = state.OutputFunctions |> Seq.sortBy (_.Id >> functionNumber) |> Seq.toList
                    Declarations = state.OutputDeclarations |> Seq.toList
                    Entry = entry }
        with
        | Failure value -> Error [ value ]
        | ex -> Error [ { Code = "FSB4001"; Stage = GoGeneration; Message = ex.Message; Span = None } ]
