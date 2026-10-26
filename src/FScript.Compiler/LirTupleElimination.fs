namespace FScript.Compiler

open FScript.Language

/// Destructure local tuple constructors without allocating an intermediate tuple.
module LirTupleElimination =
    let optimize (program: Lir.Program) =
        let children=LirTraversal.mapChildren
        let mutable maximum=LirTraversal.symbols program |> Seq.map Lir.symbolNumber |> Seq.fold max -1
        let fresh ()=maximum <- maximum+1;Lir.SymbolId maximum
        let bindItems (items: Lir.Expression list) build =
            let bound=items |> List.map (fun item -> fresh (),item)
            let loads=bound |> List.map (fun (id,item) -> {item with Node=Lir.Load id})
            let body: Lir.Expression=build loads
            (bound,body) ||> List.foldBack (fun (id,binding) body -> {body with Node=Lir.Let(id,"$tupleItem",false,binding,body)})
        let unpack (items: Lir.Expression list) (case: Lir.DecisionCase) =
            if case.Guard.IsSome || not case.Plan.Maps.IsEmpty || (case.Plan.Bindings |> List.map _.Id |> List.distinct |> List.length)<>case.Plan.Bindings.Length then None else
                let checks=case.Plan.Checks |> List.forall (function Lir.TupleArityCheck([],count) -> count=items.Length | _ -> false)
                let projections=case.Plan.Bindings |> List.map (fun binding ->
                    match binding.Projection with
                    | [Lir.TupleItem index] when index>=0 && index<items.Length && items.[index].Type=binding.Type -> Some(binding,items.[index])
                    | _ -> None)
                if checks && List.forall Option.isSome projections then
                    Some(List.choose id projections |> List.foldBack (fun (binding,item) (body: Lir.Expression) -> {body with Node=Lir.Let(binding.Id,binding.Name,false,item,body)}) <| case.Body)
                else None
        let rec scalarUses id items (value: Lir.Expression) =
            match value.Node with
            | Lir.DecisionTree({Node=Lir.Load target},[case]) when target=id ->
                (unpack items case |> Option.isSome) && scalarUses id items case.Body
            | Lir.Load target when target=id -> false
            | Lir.MakeClosure(_,captures) when List.contains id captures -> false
            | _ ->
                let mutable safe=true
                children (fun child -> safe<-safe && scalarUses id items child;child) value |> ignore
                safe
        let rec replace id replacement (value: Lir.Expression) =
            match value.Node with Lir.Load target when target=id -> replacement | _ -> children (replace id replacement) value
        let rec rewrite (value: Lir.Expression) =
            let value=children rewrite value
            match value.Node with
            | Lir.DecisionTree({Node=Lir.Tuple items},[case]) ->
                // Probe first: guards, repeated bindings and coercions remain normal decisions.
                match unpack items case with
                | None -> value
                | Some _ -> bindItems items (fun loads -> unpack loads case |> Option.get) |> rewrite
            | Lir.Let(id,_,false,({Node=Lir.Tuple items} as binding),body) when scalarUses id items body ->
                bindItems items (fun loads -> replace id {binding with Node=Lir.Tuple loads} body) |> rewrite
            | _ -> value
        {program with
            Functions=program.Functions |> List.map (fun fn -> {fn with Body=rewrite fn.Body})
            Declarations=program.Declarations |> List.map (fun item -> {item with Value=rewrite item.Value})
            Entry=Option.map rewrite program.Entry}
