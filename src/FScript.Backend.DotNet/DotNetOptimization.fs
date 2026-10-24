namespace FScript.Compiler

open FScript.Language

/// Assembly-only closure propagation and beta reduction over concrete LIR.
module internal DotNetOptimization =
    let private mapChildren (mapper: Lir.Expression -> Lir.Expression) (expression: Lir.Expression) =
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
        let mutable functions = program.Functions |> List.map (fun fn -> fn.Id,fn) |> Map.ofList
        let mutable origins = program.Functions |> List.map (fun fn -> fn.Id,fn.Id) |> Map.ofList
        let mutable nextFunction=program.Functions |> List.map (fun fn -> Lir.functionNumber fn.Id) |> List.fold max -1 |> (+) 1
        let clones=System.Collections.Generic.Dictionary<Lir.FunctionId * Lir.SymbolId list,Lir.FunctionId>()
        let mutable knownGlobals =
            program.Declarations |> List.choose (fun declaration ->
                match declaration.Value.Node with
                | Lir.MakeClosure _ | Lir.Intrinsic _ | Lir.HostExternal _ -> Some(declaration.Id,declaration.Value)
                | _ -> None) |> Map.ofList
        let graph=
            functions |> Map.map (fun _ fn ->
                let mutable dependencies=Set.empty
                let rec visit (value: Lir.Expression) =
                    match value.Node with
                    | Lir.MakeClosure(id,_) -> dependencies <- Set.add id dependencies
                    | Lir.Load symbol ->
                        match Map.tryFind symbol knownGlobals |> Option.map _.Node with
                        | Some(Lir.MakeClosure(id,_)) -> dependencies <- Set.add id dependencies
                        | _ -> ()
                    | _ -> ()
                    mapChildren (fun child -> visit child; child) value |> ignore
                visit fn.Body
                dependencies)
        let recursiveFunctions=
            graph |> Map.toList |> List.choose (fun (root,dependencies) ->
                let rec reaches seen pending =
                    if Set.contains root pending then true else
                        let pending=Set.difference pending seen
                        if pending.IsEmpty then false else
                            let next=pending |> Seq.map (fun id -> Map.tryFind id graph |> Option.defaultValue Set.empty) |> Set.unionMany
                            reaches (Set.union seen pending) next
                if reaches Set.empty dependencies then Some root else None) |> Set.ofList
        let mutable nextSymbol =
            [ yield! program.Declarations |> List.map (fun declaration -> Lir.symbolNumber declaration.Id)
              for fn in program.Functions do
                  yield Lir.symbolNumber fn.Parameter
                  yield! LirOptimization.loadedSymbols fn.Body |> Seq.map Lir.symbolNumber
              yield! program.Entry |> Option.map LirOptimization.loadedSymbols |> Option.defaultValue Set.empty |> Seq.map Lir.symbolNumber ]
            |> List.fold max 0 |> (+) 1
        let rec reserve (value: Lir.Expression) =
            let ids=
                match value.Node with
                | Lir.Let(id,_,_,_,_) | Lir.ForEach(id,_,_,_,_) -> [id]
                | Lir.LetRecursiveGroup(bindings,_) -> bindings |> List.map (fun (id,_,_,_) -> id)
                | Lir.DecisionTree(_,cases) -> cases |> List.collect (fun case -> Lir.planBindingIds case.Plan |> Set.toList)
                | _ -> []
            for id in ids do nextSymbol <- max nextSymbol (Lir.symbolNumber id+1)
            mapChildren (fun child -> reserve child; child) value |> ignore
        program.Functions |> List.iter (fun fn -> reserve fn.Body)
        program.Declarations |> List.iter (fun declaration -> reserve declaration.Value)
        program.Entry |> Option.iter reserve
        let fresh () =
            let result=Lir.SymbolId nextSymbol
            nextSymbol <- nextSymbol+1
            result
        // Every expansion receives fresh lexical identities, including pattern bindings.
        let rec clone (fn: Lir.Function) captures =
            let mutable renamed=List.zip fn.Captures captures |> Map.ofList
            let bind id =
                match Map.tryFind id renamed with
                | Some replacement -> replacement
                | None -> let replacement=fresh () in renamed <- Map.add id replacement renamed; replacement
            let parameter=bind fn.Parameter
            let rec plan (value: Lir.PatternPlan) =
                { value with
                    Bindings=value.Bindings |> List.map (fun binding -> {binding with Id=bind binding.Id})
                    Maps=value.Maps |> List.map (fun map -> {map with Clauses=map.Clauses |> List.map (fun clause -> {Key=plan clause.Key;Value=plan clause.Value}); Tail=Option.map plan map.Tail}) }
            let rec collect (value: Lir.Expression) =
                match value.Node with
                | Lir.Let(id,_,_,_,_) | Lir.ForEach(id,_,_,_,_) -> bind id |> ignore
                | Lir.LetRecursiveGroup(bindings,_) -> bindings |> List.iter (fun (id,_,_,_) -> bind id |> ignore)
                | Lir.DecisionTree(_,cases) -> cases |> List.iter (fun case -> plan case.Plan |> ignore)
                | _ -> ()
                mapChildren (fun child -> collect child; child) value |> ignore
            collect fn.Body
            let rename id = Map.tryFind id renamed |> Option.defaultValue id
            let rec rewrite value =
                let value=mapChildren rewrite value
                let node =
                    match value.Node with
                    | Lir.Load id -> Lir.Load(rename id)
                    | Lir.MakeClosure(id,captures) ->
                        let mapped=List.map rename captures
                        if mapped=captures then Lir.MakeClosure(id,captures)
                        else
                            let clonedId=
                                match clones.TryGetValue((id,mapped)) with
                                | true,existing -> existing
                                | _ ->
                                    let newId=Lir.FunctionId nextFunction
                                    nextFunction <- nextFunction+1
                                    clones.Add((id,mapped),newId)
                                    let original=functions.[id]
                                    // Reserve the identity before visiting recursive closure groups.
                                    functions <- Map.add newId {original with Id=newId; Captures=List.sort mapped} functions
                                    origins <- Map.add newId origins.[id] origins
                                    let parameter,body=clone original mapped
                                    functions <- Map.add newId {original with Id=newId; Parameter=parameter; Captures=List.sort mapped; Body=body} functions
                                    newId
                            Lir.MakeClosure(clonedId,List.sort mapped)
                    | Lir.Let(id,name,recursive,binding,body) -> Lir.Let(rename id,name,recursive,binding,body)
                    | Lir.LetRecursiveGroup(bindings,body) -> Lir.LetRecursiveGroup(bindings |> List.map (fun (id,name,t,binding) -> rename id,name,t,binding),body)
                    | Lir.ForEach(id,name,t,source,body) -> Lir.ForEach(rename id,name,t,source,body)
                    | Lir.DecisionTree(target,cases) -> Lir.DecisionTree(target,cases |> List.map (fun case -> {case with Plan=plan case.Plan}))
                    | node -> node
                {value with Node=node}
            parameter,rewrite fn.Body
        let replicable (value: Lir.Expression) =
            match value.Node with
            | Lir.Load _ | Lir.Constant _ | Lir.MakeClosure _ | Lir.Intrinsic _ | Lir.TargetImport _ | Lir.HostExternal _ -> true
            | _ -> false
        let fold (value: Lir.Expression) =
            let constant=
                match value.Node with
                | Lir.Operation(operation,[{Node=Lir.Constant(Lir.Int a)};{Node=Lir.Constant(Lir.Int b)}]) ->
                    match operation with
                    | Lir.IntAdd | Lir.NumericAdd -> Some(Lir.Int(a+b))
                    | Lir.IntSubtract | Lir.NumericSubtract -> Some(Lir.Int(a-b))
                    | Lir.IntMultiply | Lir.NumericMultiply -> Some(Lir.Int(a*b))
                    | Lir.NumericLessThan -> Some(Lir.Bool(a<b))
                    | Lir.NumericGreaterThan -> Some(Lir.Bool(a>b))
                    | Lir.NumericLessThanOrEqual -> Some(Lir.Bool(a<=b))
                    | Lir.NumericGreaterThanOrEqual -> Some(Lir.Bool(a>=b))
                    | Lir.StructuralEqual -> Some(Lir.Bool(a=b))
                    | _ -> None
                | _ -> None
            match constant with Some constant -> {value with Node=Lir.Constant constant} | None -> value
        let rec rewrite active (known: Map<Lir.SymbolId,Lir.Expression>) (expression: Lir.Expression) =
            let recurse = rewrite active known
            let reduceCall (fn: Lir.Expression) (argument: Lir.Expression) pipe =
                match fn.Node with
                | Lir.MakeClosure(id,captures) when not (Set.contains origins.[id] active || Set.contains origins.[id] recursiveFunctions) ->
                    let original=functions.[id]
                    // Captures of a lifted body still refer to their original lexical IDs.
                    // Local aliases provide the values after the enclosing expansion was cloned.
                    let parameter,body=clone original captures
                    let binding={expression with Node=Lir.Let(parameter,"$nativeArgument",false,argument,body)}
                    rewrite (Set.add origins.[id] active) known binding
                | Lir.Let(id,name,false,binding,body) ->
                    if pipe then
                        let argumentId=fresh ()
                        let load={argument with Node=Lir.Load argumentId}
                        rewrite active known {expression with Node=Lir.Let(argumentId,"$nativePipe",false,argument,{expression with Node=Lir.Let(id,name,false,binding,{expression with Node=Lir.CallClosure(body,load)})})}
                    else
                        rewrite active known {expression with Node=Lir.Let(id,name,false,binding,{expression with Node=Lir.CallClosure(body,argument)})}
                | _ -> {expression with Node=if pipe then Lir.PipeClosure(argument,fn) else Lir.CallClosure(fn,argument)}
            match expression.Node with
            | Lir.Load id ->
                match Map.tryFind id known with
                | Some {Node=Lir.MakeClosure(functionId,_)} when Set.contains origins.[functionId] recursiveFunctions -> expression
                | Some replacement -> replacement
                | None -> expression
            | Lir.Let(id,name,false,binding,body) ->
                let binding=recurse binding
                match binding.Node with
                | Lir.Let(innerId,innerName,false,innerBinding,innerBody) ->
                    recurse {expression with Node=Lir.Let(innerId,innerName,false,innerBinding,{expression with Node=Lir.Let(id,name,false,innerBody,body)})}
                | _ when replicable binding ->
                    let body=rewrite active (Map.add id binding known) body
                    {expression with Node=Lir.Let(id,name,false,binding,body)}
                | _ -> {expression with Node=Lir.Let(id,name,false,binding,recurse body)}
            | Lir.CallClosure(fn,argument) | Lir.TailCallClosure(fn,argument) -> reduceCall (recurse fn) (recurse argument) false
            | Lir.PipeClosure(argument,fn) | Lir.TailPipeClosure(argument,fn) -> reduceCall (recurse fn) (recurse argument) true
            | _ -> mapChildren recurse expression |> fold
        let rec hoist (declaration: Lir.Declaration) (value: Lir.Expression) =
            match value.Node with
            | Lir.Let(id,name,false,binding,body) ->
                {declaration with Id=id;Name=name;Type=binding.Type;Value=binding;IsExported=false;Export=None;IsRecursive=false} :: hoist declaration body
            | _ ->
                match value.Node with
                | Lir.MakeClosure _ | Lir.Constant _ | Lir.NoneValue | Lir.TypeToken _ -> knownGlobals <- Map.add declaration.Id value knownGlobals
                | _ -> ()
                [{declaration with Value=value}]
        let optimizedDeclarations=program.Declarations |> List.collect (fun declaration -> hoist declaration (rewrite Set.empty knownGlobals declaration.Value))
        let optimizedEntry=program.Entry |> Option.map (rewrite Set.empty knownGlobals)
        let optimizedFunctions=program.Functions |> List.map (fun fn -> {fn with Body=rewrite (Set.singleton fn.Id) knownGlobals fn.Body})
        let originalIds=program.Functions |> List.map _.Id |> Set.ofList
        let clonedFunctions=functions |> Map.toList |> List.choose (fun (id,fn) -> if Set.contains id originalIds then None else Some fn)
        let result=
            { program with
                Functions=optimizedFunctions @ clonedFunctions |> List.sortBy _.Id
                Declarations=optimizedDeclarations
                Entry=optimizedEntry }
        result |> LirOptimization.optimize

