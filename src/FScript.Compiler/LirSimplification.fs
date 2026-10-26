namespace FScript.Compiler

/// Backend-private simplification over the closed, monomorphized LIR program.
module LirSimplification =
    open LirEffects

    let private expressionSize expression =
        let rec size (value: Lir.Expression) =
            let many values = values |> List.sumBy size
            1 +
            match value.Node with
            | Lir.CallClosure(left, right) | Lir.PipeClosure(left, right)
            | Lir.TailCallClosure(left, right) | Lir.TailPipeClosure(left, right)
            | Lir.Cons(left, right) | Lir.Append(left, right) | Lir.Range(left, right) | Lir.IndexGet(left, right) -> many [ left; right ]
            | Lir.Branch(a, b, c) -> many [ a; b; c ]
            | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> many values
            | Lir.Record fields -> fields |> List.sumBy (snd >> size)
            | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.Fail target | Lir.RecordConvert(target, _) -> size target
            | Lir.Let(_, _, _, binding, body) -> many [ binding; body ]
            | Lir.LetRecursiveGroup(bindings, body) -> size body + (bindings |> List.sumBy (fun (_, _, _, binding) -> size binding))
            | Lir.DecisionTree(target, cases) -> size target + (cases |> List.sumBy (fun item -> size item.Body + (item.Guard |> Option.map size |> Option.defaultValue 0)))
            | Lir.RecordUpdate(target, fields) -> size target + (fields |> List.sumBy (snd >> size))
            | Lir.Interpolate parts -> parts |> List.sumBy (function Lir.TextPart _ -> 0 | Lir.ValuePart item -> size item)
            | Lir.UnionCase(_, _, payload) -> payload |> Option.map size |> Option.defaultValue 0
            | Lir.Map entries -> entries |> List.sumBy (function Lir.KeyValue(key, item) -> size key + size item | Lir.Spread item -> size item)
            | Lir.ForEach(_, _, _, source, body) -> size source + size body
            | _ -> 0
        size expression

    let private mapExpression (mapper: Lir.Expression -> Lir.Expression) (expression: Lir.Expression) =
        let recurse = mapper
        let withNode node = { expression with Node = node }
        match expression.Node with
        | Lir.CallClosure(fn, argument) -> withNode (Lir.CallClosure(recurse fn, recurse argument))
        | Lir.PipeClosure(argument, fn) -> withNode (Lir.PipeClosure(recurse argument, recurse fn))
        | Lir.TailCallClosure(fn, argument) -> withNode (Lir.TailCallClosure(recurse fn, recurse argument))
        | Lir.TailPipeClosure(argument, fn) -> withNode (Lir.TailPipeClosure(recurse argument, recurse fn))
        | Lir.Branch(a, b, c) -> withNode (Lir.Branch(recurse a, recurse b, recurse c))
        | Lir.Operation(operation, values) -> withNode (Lir.Operation(operation, List.map recurse values))
        | Lir.Tuple values -> withNode (Lir.Tuple(List.map recurse values))
        | Lir.List values -> withNode (Lir.List(List.map recurse values))
        | Lir.Record fields -> withNode (Lir.Record(fields |> List.map (fun (name, value) -> name, recurse value)))
        | Lir.FieldGet(target, name) -> withNode (Lir.FieldGet(recurse target, name))
        | Lir.SomeValue value -> withNode (Lir.SomeValue(recurse value))
        | Lir.Cons(left, right) -> withNode (Lir.Cons(recurse left, recurse right))
        | Lir.Append(left, right) -> withNode (Lir.Append(recurse left, recurse right))
        | Lir.Let(id, name, recursive, binding, body) -> withNode (Lir.Let(id, name, recursive, recurse binding, recurse body))
        | Lir.LetRecursiveGroup(bindings, body) -> withNode (Lir.LetRecursiveGroup(bindings |> List.map (fun (id, name, valueType, value) -> id, name, valueType, recurse value), recurse body))
        | Lir.Fail value -> withNode (Lir.Fail(recurse value))
        | Lir.DecisionTree(target, cases) -> withNode (Lir.DecisionTree(recurse target, cases |> List.map (fun item -> { item with Guard = Option.map recurse item.Guard; Body = recurse item.Body })))
        | Lir.Range(left, right) -> withNode (Lir.Range(recurse left, recurse right))
        | Lir.RecordUpdate(target, fields) -> withNode (Lir.RecordUpdate(recurse target, fields |> List.map (fun (name, value) -> name, recurse value)))
        | Lir.RecordConvert(target, targetType) -> withNode (Lir.RecordConvert(recurse target, targetType))
        | Lir.IndexGet(target, index) -> withNode (Lir.IndexGet(recurse target, recurse index))
        | Lir.Interpolate parts -> withNode (Lir.Interpolate(parts |> List.map (function Lir.TextPart text -> Lir.TextPart text | Lir.ValuePart value -> Lir.ValuePart(recurse value))))
        | Lir.UnionCase(typeName, caseName, payload) -> withNode (Lir.UnionCase(typeName, caseName, Option.map recurse payload))
        | Lir.Map entries -> withNode (Lir.Map(entries |> List.map (function Lir.KeyValue(key, value) -> Lir.KeyValue(recurse key, recurse value) | Lir.Spread value -> Lir.Spread(recurse value))))
        | Lir.ForEach(id, name, itemType, source, body) -> withNode (Lir.ForEach(id, name, itemType, recurse source, recurse body))
        | _ -> expression

    let optimize (program: Lir.Program) =
        let program = LirAggregateElimination.optimize program
        let functions = program.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
        let declarations = program.Declarations |> List.map (fun declaration -> declaration.Id, declaration) |> Map.ofList
        let effect = LirEffects.create program

        let rec substitute symbol replacement (expression: Lir.Expression) =
            match expression.Node with
            | Lir.Load id when id = symbol -> replacement
            | _ -> mapExpression (substitute symbol replacement) expression

        let replicable (expression: Lir.Expression) =
            match expression.Node with
            | Lir.Constant _ | Lir.Load _ | Lir.NoneValue | Lir.TypeToken _ -> true
            | Lir.FieldGet({ Node = Lir.Load _ }, _) -> true
            | _ -> false

        let rec simplify (expression: Lir.Expression) =
            let recurse = simplify
            let value = mapExpression recurse expression
            match value.Node with
            | Lir.Branch({ Node = Lir.Constant(Lir.Bool true) }, whenTrue, _) -> whenTrue
            | Lir.Branch({ Node = Lir.Constant(Lir.Bool false) }, _, whenFalse) -> whenFalse
            | Lir.Let(id, _, false, binding, body) when body.Node = Lir.Load id -> binding
            | Lir.Let(id, _, false, binding, body) when not (Set.contains id (LirOptimization.loadedSymbols body)) && effect binding = Pure -> body
            | Lir.Let(id, _, false, binding, body) when replicable binding && effect binding = Pure -> substitute id binding body |> recurse
            | Lir.CallClosure({ Node = Lir.MakeClosure(functionId, _) }, argument)
                when functions |> Map.tryFind functionId |> Option.exists (fun fn -> expressionSize fn.Body <= 18) ->
                let fn = functions.[functionId]
                { expression with Node = Lir.Let(fn.Parameter, "$inline", false, argument, fn.Body) } |> recurse
            | Lir.PipeClosure(argument, { Node = Lir.MakeClosure(functionId, _) })
                when functions |> Map.tryFind functionId |> Option.exists (fun fn -> expressionSize fn.Body <= 18) ->
                let fn = functions.[functionId]
                { expression with Node = Lir.Let(fn.Parameter, "$inline", false, argument, fn.Body) } |> recurse
            | _ -> value

        let optimizedFunctions = program.Functions |> List.map (fun fn -> { fn with Body = simplify fn.Body })
        let captureMap =
            optimizedFunctions
            |> List.map (fun fn -> fn.Id, (LirLowering.capturedReferences (Set.singleton fn.Parameter) fn.Body |> Set.toList |> List.sortBy Lir.symbolNumber))
            |> Map.ofList
        let rec rewriteCaptures expression =
            let value = mapExpression rewriteCaptures expression
            match value.Node with
            | Lir.MakeClosure(id, captures) -> { value with Node = Lir.MakeClosure(id, Map.tryFind id captureMap |> Option.defaultValue captures) }
            | _ -> value
        let optimized =
            { program with
                Functions = optimizedFunctions |> List.map (fun fn -> { fn with Captures = captureMap.[fn.Id]; Body = rewriteCaptures fn.Body })
                Declarations = program.Declarations |> List.map (fun declaration -> { declaration with Value = declaration.Value |> simplify |> rewriteCaptures })
                Entry = program.Entry |> Option.map (simplify >> rewriteCaptures) }
        let optimizedDeclarations = optimized.Declarations |> List.map (fun declaration -> declaration.Id, declaration) |> Map.ofList
        let optimizedFunctions = optimized.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
        let reachableDeclarations = System.Collections.Generic.HashSet<Lir.SymbolId>()
        let reachableFunctions = System.Collections.Generic.HashSet<Lir.FunctionId>()
        let rec visitExpression (expression: Lir.Expression) =
            for id in LirOptimization.loadedSymbols expression do
                match Map.tryFind id optimizedDeclarations with
                | Some declaration when reachableDeclarations.Add id -> visitExpression declaration.Value
                | _ -> ()
            let rec visitClosures (value: Lir.Expression) =
                match value.Node with
                | Lir.MakeClosure(id, _) when reachableFunctions.Add id ->
                    match Map.tryFind id optimizedFunctions with Some fn -> visitExpression fn.Body | None -> ()
                | _ -> mapExpression (fun child -> visitClosures child; child) value |> ignore
            visitClosures expression
        for declaration in optimized.Declarations do
            if declaration.IsExported || effect declaration.Value = Observable then
                if reachableDeclarations.Add declaration.Id then visitExpression declaration.Value
        optimized.Entry |> Option.iter visitExpression
        let retainedFunctions = optimized.Functions |> List.filter (fun fn -> reachableFunctions.Contains fn.Id)
        let functionMap = retainedFunctions |> List.mapi (fun index fn -> fn.Id, Lir.FunctionId index) |> Map.ofList
        let rec rewriteFunctionIds expression =
            let value = mapExpression rewriteFunctionIds expression
            match value.Node with
            | Lir.MakeClosure(id, captures) -> { value with Node = Lir.MakeClosure(functionMap.[id], captures) }
            | _ -> value
        { optimized with
            Functions = retainedFunctions |> List.map (fun fn -> { fn with Id = functionMap.[fn.Id]; Body = rewriteFunctionIds fn.Body })
            Declarations = optimized.Declarations |> List.filter (fun declaration -> reachableDeclarations.Contains declaration.Id) |> List.map (fun declaration -> { declaration with Value = rewriteFunctionIds declaration.Value })
            Entry = optimized.Entry |> Option.map rewriteFunctionIds }
