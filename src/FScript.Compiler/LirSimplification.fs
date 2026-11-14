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

    let private mapExpression = LirTraversal.mapChildren

    let private validDivision (dividend: int64) (divisor: int64) =
        divisor <> 0L && (divisor <> -1L || dividend <> System.Int64.MinValue)

    let private fold operation left right =
        match operation, left, right with
        | (Lir.IntAdd | Lir.NumericAdd), Lir.Int a, Lir.Int b -> Some(Lir.Int(a + b))
        | (Lir.IntSubtract | Lir.NumericSubtract), Lir.Int a, Lir.Int b -> Some(Lir.Int(a - b))
        | (Lir.IntMultiply | Lir.NumericMultiply), Lir.Int a, Lir.Int b -> Some(Lir.Int(a * b))
        | (Lir.IntDivide | Lir.NumericDivide), Lir.Int a, Lir.Int b when validDivision a b -> Some(Lir.Int(a / b))
        | Lir.IntRemainder, Lir.Int a, Lir.Int b when validDivision a b -> Some(Lir.Int(a % b))
        | (Lir.FloatAdd | Lir.NumericAdd), Lir.Float a, Lir.Float b -> Some(Lir.Float(a + b))
        | (Lir.FloatSubtract | Lir.NumericSubtract), Lir.Float a, Lir.Float b -> Some(Lir.Float(a - b))
        | (Lir.FloatMultiply | Lir.NumericMultiply), Lir.Float a, Lir.Float b -> Some(Lir.Float(a * b))
        | (Lir.FloatDivide | Lir.NumericDivide), Lir.Float a, Lir.Float b -> Some(Lir.Float(a / b))
        | Lir.NumericLessThan, Lir.Int a, Lir.Int b -> Some(Lir.Bool(a < b))
        | Lir.NumericGreaterThan, Lir.Int a, Lir.Int b -> Some(Lir.Bool(a > b))
        | Lir.NumericLessThanOrEqual, Lir.Int a, Lir.Int b -> Some(Lir.Bool(a <= b))
        | Lir.NumericGreaterThanOrEqual, Lir.Int a, Lir.Int b -> Some(Lir.Bool(a >= b))
        | Lir.NumericLessThan, Lir.Float a, Lir.Float b -> Some(Lir.Bool(a < b))
        | Lir.NumericGreaterThan, Lir.Float a, Lir.Float b -> Some(Lir.Bool(a > b))
        | Lir.NumericLessThanOrEqual, Lir.Float a, Lir.Float b -> Some(Lir.Bool(a <= b))
        | Lir.NumericGreaterThanOrEqual, Lir.Float a, Lir.Float b -> Some(Lir.Bool(a >= b))
        | Lir.StructuralEqual, Lir.Float a, Lir.Float b -> Some(Lir.Bool(a = b))
        | Lir.StructuralEqual, _, _ -> Some(Lir.Bool(left = right))
        | _ -> None

    /// Fold primitive operations only after their operands have been proven to be literals.
    let foldPrimitive (value: Lir.Expression) =
        match value.Node with
        | Lir.Operation(operation, [{ Node = Lir.Constant left }; { Node = Lir.Constant right }]) ->
            match fold operation left right with
            // Non-finite results stay in the operation form accepted by LIR artifacts.
            | Some(Lir.Float result) when not (System.Double.IsFinite result) -> value
            | Some result -> { value with Node = Lir.Constant result }
            | None -> value
        | _ -> value

    let optimize (program: Lir.Program) =
        let program = LirAggregateElimination.optimize program
        let functions = program.Functions |> List.map (fun fn -> fn.Id, fn) |> Map.ofList
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

        let rec capturedByClosure symbol (expression: Lir.Expression) =
            match expression.Node with
            | Lir.MakeClosure(_,captures) -> List.contains symbol captures
            | _ ->
                let mutable found=false
                mapExpression (fun child -> found <- found || capturedByClosure symbol child; child) expression |> ignore
                found

        let rec simplify known (expression: Lir.Expression) =
            let recurse = simplify known
            let withNode node = { expression with Node = node }
            match expression.Node with
            | Lir.Load id ->
                match Map.tryFind id known with
                | Some literal -> withNode (Lir.Constant literal)
                | None -> expression
            | Lir.Branch(condition, whenTrue, whenFalse) ->
                let condition = recurse condition
                match condition.Node with
                | Lir.Constant(Lir.Bool true) -> recurse whenTrue
                | Lir.Constant(Lir.Bool false) -> recurse whenFalse
                | _ -> withNode (Lir.Branch(condition, recurse whenTrue, recurse whenFalse))
            | Lir.Operation((Lir.BooleanAnd | Lir.BooleanOr) as operation, [left; right]) ->
                let left = recurse left
                match left.Node with
                | Lir.Constant(Lir.Bool value) ->
                    if value = (operation = Lir.BooleanOr) then left else recurse right
                | _ -> withNode (Lir.Operation(operation, [left; recurse right]))
            | Lir.Let(id, name, false, binding, body) ->
                let binding = recurse binding
                let body =
                    match binding.Node with
                    | Lir.Constant literal -> simplify (Map.add id literal known) body
                    | _ -> recurse body
                match body.Node with
                | Lir.Load loaded when loaded = id -> binding
                | _ when not (Set.contains id (LirOptimization.loadedSymbols body)) && effect binding = Pure -> body
                | _ when replicable binding && effect binding = Pure && not (capturedByClosure id body) ->
                    substitute id binding body |> recurse
                | _ -> withNode (Lir.Let(id, name, false, binding, body))
            | _ ->
                let value = mapExpression recurse expression |> foldPrimitive
                match value.Node with
                | Lir.CallClosure({ Node = Lir.MakeClosure(functionId, _) }, argument)
                | Lir.PipeClosure(argument, { Node = Lir.MakeClosure(functionId, _) })
                    when functions |> Map.tryFind functionId |> Option.exists (fun fn -> expressionSize fn.Body <= 18) ->
                    let fn = functions.[functionId]
                    withNode (Lir.Let(fn.Parameter, "$inline", false, argument, fn.Body)) |> recurse
                | _ -> value

        // Populate facts in initialization order. A call, field read, or import
        // can never enter this table merely because its type or contract is pure.
        let mutable globals = Map.empty
        let optimizedDeclarations =
            program.Declarations |> List.map (fun declaration ->
                let value = simplify globals declaration.Value
                match value.Node with
                | Lir.Constant literal when not declaration.IsRecursive ->
                    globals <- Map.add declaration.Id literal globals
                | _ -> ()
                { declaration with Value = value })
        let optimizedFunctions = program.Functions |> List.map (fun fn -> { fn with Body = simplify globals fn.Body })
        let rec rewriteCaptures captureMap expression =
            let value = mapExpression (rewriteCaptures captureMap) expression
            match value.Node with
            | Lir.MakeClosure(id, captures) -> { value with Node = Lir.MakeClosure(id, Map.tryFind id captureMap |> Option.defaultValue captures) }
            | _ -> value
        let rec refreshCaptures (functions: Lir.Function list) =
            let captureMap =
                functions
                |> List.map (fun fn -> fn.Id, (LirLowering.capturedReferences (Set.singleton fn.Parameter) fn.Body |> Set.toList |> List.sortBy Lir.symbolNumber))
                |> Map.ofList
            if functions |> List.forall (fun fn -> fn.Captures = captureMap.[fn.Id]) then functions, captureMap
            else
                functions
                |> List.map (fun fn -> { fn with Captures = captureMap.[fn.Id]; Body = rewriteCaptures captureMap fn.Body })
                |> refreshCaptures
        let optimizedFunctions, captureMap = refreshCaptures optimizedFunctions
        let rewrite = rewriteCaptures captureMap
        let optimized =
            { program with
                Functions = optimizedFunctions
                Declarations = optimizedDeclarations |> List.map (fun declaration -> { declaration with Value = rewrite declaration.Value })
                Entry = program.Entry |> Option.map (simplify globals >> rewrite) }
        let effect = LirEffects.create optimized
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
