namespace FScript.Compiler

open FScript.Language

module LirTraversal =
    let mapChildren mapper (value: Lir.Expression) =
        let node =
            match value.Node with
            | Lir.CallClosure(a,b) -> Lir.CallClosure(mapper a,mapper b)
            | Lir.PipeClosure(a,b) -> Lir.PipeClosure(mapper a,mapper b)
            | Lir.TailCallClosure(a,b) -> Lir.TailCallClosure(mapper a,mapper b)
            | Lir.TailPipeClosure(a,b) -> Lir.TailPipeClosure(mapper a,mapper b)
            | Lir.Branch(a,b,c) -> Lir.Branch(mapper a,mapper b,mapper c)
            | Lir.Operation(op,values) -> Lir.Operation(op,List.map mapper values)
            | Lir.Tuple values -> Lir.Tuple(List.map mapper values)
            | Lir.List values -> Lir.List(List.map mapper values)
            | Lir.Record fields -> Lir.Record(fields |> List.map (fun (name,item) -> name,mapper item))
            | Lir.FieldGet(a,name) -> Lir.FieldGet(mapper a,name)
            | Lir.SomeValue a -> Lir.SomeValue(mapper a)
            | Lir.Cons(a,b) -> Lir.Cons(mapper a,mapper b)
            | Lir.Append(a,b) -> Lir.Append(mapper a,mapper b)
            | Lir.Let(id,name,recursive,binding,body) -> Lir.Let(id,name,recursive,mapper binding,mapper body)
            | Lir.LetRecursiveGroup(bindings,body) -> Lir.LetRecursiveGroup(bindings |> List.map (fun (id,name,t,binding) -> id,name,t,mapper binding),mapper body)
            | Lir.Fail a -> Lir.Fail(mapper a)
            | Lir.DecisionTree(a,cases) -> Lir.DecisionTree(mapper a,cases |> List.map (fun case -> {case with Guard=Option.map mapper case.Guard;Body=mapper case.Body}))
            | Lir.Range(a,b) -> Lir.Range(mapper a,mapper b)
            | Lir.RecordUpdate(a,fields) -> Lir.RecordUpdate(mapper a,fields |> List.map (fun (name,item) -> name,mapper item))
            | Lir.RecordConvert(a,t) -> Lir.RecordConvert(mapper a,t)
            | Lir.IndexGet(a,b) -> Lir.IndexGet(mapper a,mapper b)
            | Lir.Interpolate parts -> Lir.Interpolate(parts |> List.map (function Lir.TextPart text -> Lir.TextPart text | Lir.ValuePart item -> Lir.ValuePart(mapper item)))
            | Lir.UnionCase(name,case,payload) -> Lir.UnionCase(name,case,Option.map mapper payload)
            | Lir.Map entries -> Lir.Map(entries |> List.map (function Lir.KeyValue(a,b) -> Lir.KeyValue(mapper a,mapper b) | Lir.Spread a -> Lir.Spread(mapper a)))
            | Lir.ForEach(id,name,t,source,body) -> Lir.ForEach(id,name,t,mapper source,mapper body)
            | _ -> value.Node
        {value with Node=node}

    let symbols (program: Lir.Program) =
        let mutable result=Set.empty
        let add id=result<-Set.add id result
        let rec plan (value: Lir.PatternPlan) =
            for binding in value.Bindings do add binding.Id
            for map in value.Maps do
                for clause in map.Clauses do plan clause.Key;plan clause.Value
                Option.iter plan map.Tail
        let rec walk (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id -> add id
            | Lir.MakeClosure(_,captures) -> List.iter add captures
            | Lir.Let(id,_,_,_,_) | Lir.ForEach(id,_,_,_,_) -> add id
            | Lir.LetRecursiveGroup(bindings,_) -> for id,_,_,_ in bindings do add id
            | Lir.DecisionTree(_,cases) -> for case in cases do plan case.Plan
            | _ -> ()
            mapChildren (fun child -> walk child;child) value |> ignore
        for typ in program.Types do add typ.Id
        for declaration in program.Declarations do add declaration.Id;walk declaration.Value
        for fn in program.Functions do add fn.Parameter;List.iter add fn.Captures;walk fn.Body
        Option.iter walk program.Entry
        result

module LirResultPolicy =
    /// Preserve effects and failures, while removing the entry value as a reachability root.
    let discard (program: Lir.Program) =
        let effect=LirEffects.create program
        let entry=program.Entry |> Option.map (fun entry ->
            let unitValue: Lir.Expression={Type=TUnit;Span=entry.Span;Node=Lir.Constant Lir.Unit}
            if effect entry=LirEffects.Pure then unitValue else
                let maximum=LirTraversal.symbols program |> Seq.map Lir.symbolNumber |> Seq.fold max -1
                {unitValue with Node=Lir.Let(Lir.SymbolId(maximum+1),"$discardResult",false,entry,unitValue)})
        {program with Entry=entry}
