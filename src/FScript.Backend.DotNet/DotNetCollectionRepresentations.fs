namespace FScript.Compiler

open FScript.Language

/// Choose one internal element representation through closed collection consumers.
/// Public returns, escaping bindings and unknown callbacks retain their boundaries.
module internal DotNetCollectionRepresentations =
    let optimize (program: Lir.Program) =
        let children=LirTraversal.mapChildren
        let mutable functions=program.Functions |> List.map (fun fn -> fn.Id,fn) |> Map.ofList
        let definitions=program.Types |> List.map (fun item -> item.Name,item) |> Map.ofList
        let imported=program.ImportedRecords |> List.map _.Name |> Set.ofList
        let rec fields seen = function
            | TRecord values -> Some values
            | TNamed name when not (Set.contains name seen || imported.Contains name) ->
                match Map.tryFind name definitions with
                | Some item when item.Import.IsNone -> fields (Set.add name seen) item.Definition
                | _ -> None
            | _ -> None
        let compatible actual required =
            actual=required ||
            match fields Set.empty actual,fields Set.empty required with
            | Some actual,Some required -> required |> Map.forall (fun name t -> Map.tryFind name actual=Some t)
            | _ -> false
        let mutable uses=Map.empty
        let rec count (value: Lir.Expression) =
            match value.Node with
            | Lir.MakeClosure(id,_) -> uses <- Map.add id (1+(Map.tryFind id uses |> Option.defaultValue 0)) uses
            | _ -> ()
            children (fun child -> count child;child) value |> ignore
        for fn in program.Functions do count fn.Body
        for item in program.Declarations do count item.Value
        Option.iter count program.Entry
        let rec chain seen id =
            let fn=functions.[id]
            match fn.Body.Node with
            | Lir.MakeClosure(next,_) when not (Set.contains next seen) -> fn::chain (Set.add id seen) next
            | _ -> [fn]
        let callback (value: Lir.Expression) =
            match value.Node with
            | Lir.MakeClosure(id,_) -> Some(chain Set.empty id)
            | _ -> None
        let canAdapt actual (value: Lir.Expression) arity =
            callback value |> Option.exists (fun fns ->
                let last=List.last fns
                fns.Length=arity && compatible actual last.ParameterType &&
                (actual=last.ParameterType ||
                    (fns |> List.forall (fun fn -> Map.tryFind fn.Id uses=Some 1)) &&
                    not (functions |> Map.exists (fun _ fn -> List.contains last.Parameter fn.Captures))))
        let rec functionType id =
            let fn=functions.[id]
            TFun(fn.ParameterType,fn.ReturnType)
        let adapt actual (value: Lir.Expression) =
            let fns=callback value |> Option.get
            let last=List.last fns
            if last.ParameterType<>actual then
                let rec replace (node: Lir.Expression) =
                    match node.Node with
                    | Lir.Load id when id=last.Parameter ->
                        {node with Node=Lir.RecordConvert({node with Type=actual},node.Type)}
                    | _ -> children replace node
                functions <- Map.add last.Id {last with ParameterType=actual;Body=replace last.Body} functions
                for parent in fns |> List.rev |> List.tail do
                    match parent.Body.Node with
                    | Lir.MakeClosure(child,_) ->
                        let t=functionType child
                        functions <- Map.add parent.Id {parent with ReturnType=t;Body={parent.Body with Type=t}} functions
                    | _ -> ()
            match value.Node with
            | Lir.MakeClosure(id,_) -> {value with Type=functionType id}
            | _ -> value
        let rec spine (value: Lir.Expression) =
            match value.Node with
            | Lir.CallClosure(fn,arg) | Lir.TailCallClosure(fn,arg)
            | Lir.PipeClosure(arg,fn) | Lir.TailPipeClosure(arg,fn) ->
                let root,args=spine fn
                root,args@[arg]
            | _ -> value,[]
        let rebuild original rootType arguments =
            let mutable index=0
            let rec loop (value: Lir.Expression) : Lir.Expression =
                let call (fn: Lir.Expression) make =
                    let fn=loop fn
                    let argument=List.item index arguments
                    index <- index+1
                    let result=match fn.Type with TFun(_,result) -> result | _ -> invalidOp "Collection application signature."
                    {value with Type=result;Node=make fn argument}
                match value.Node with
                | Lir.CallClosure(fn,_) -> call fn (fun fn arg -> Lir.CallClosure(fn,arg))
                | Lir.TailCallClosure(fn,_) -> call fn (fun fn arg -> Lir.TailCallClosure(fn,arg))
                | Lir.PipeClosure(_,fn) -> call fn (fun fn arg -> Lir.PipeClosure(arg,fn))
                | Lir.TailPipeClosure(_,fn) -> call fn (fun fn arg -> Lir.TailPipeClosure(arg,fn))
                | _ -> {value with Type=rootType}
            loop original
        let rec safeUses id (value: Lir.Expression) =
            let root,args=spine value
            match root.Node,args with
            | Lir.Intrinsic name,[callback;{Node=Lir.Load source}] when source=id && (name="List.filter" || name="List.map") ->
                callback.Node |> function Lir.MakeClosure(_,captures) -> not (List.contains id captures) | _ -> false
            | Lir.Intrinsic "List.fold",[callback;initial;{Node=Lir.Load source}] when source=id ->
                (match callback.Node with Lir.MakeClosure(_,captures) -> not (List.contains id captures) | _ -> false) && safeUses id initial
            | Lir.Intrinsic "List.length",[{Node=Lir.Load source}] when source=id -> true
            | _ ->
                match value.Node with
                | Lir.Load source when source=id -> false
                | Lir.MakeClosure(_,captures) when List.contains id captures -> false
                | _ ->
                    let mutable safe=true
                    children (fun child -> safe <- safe && safeUses id child;child) value |> ignore
                    safe
        let rec rewrite free environment (value: Lir.Expression) =
            let recurse=rewrite false environment
            match value.Node with
            | Lir.Load id -> {value with Type=Map.tryFind id environment |> Option.defaultValue value.Type}
            | Lir.Let(id,name,false,binding,body) ->
                let binding=rewrite (safeUses id body) environment binding
                let body=rewrite free (Map.add id binding.Type environment) body
                {value with Type=body.Type;Node=Lir.Let(id,name,false,binding,body)}
            | _ ->
                let root,args=spine value
                match root.Node,args with
                | Lir.Intrinsic name,[cb;source] when name="List.map" || name="List.filter" ->
                    let source=rewrite true environment source
                    let actual=match source.Type with TList t -> t | _ -> invalidOp "Collection input."
                    let cb=recurse cb
                    if canAdapt actual cb 1 && (free || name="List.map") then
                        let cb=adapt actual cb
                        let output=
                            if name="List.filter" then actual
                            elif free then (callback cb |> Option.get |> List.last).ReturnType
                            else match value.Type with TList t -> t | _ -> invalidOp "Collection output."
                        let cbType=TFun(actual,if name="List.map" then output else TBool)
                        let cb={cb with Type=cbType}
                        rebuild value (TFun(cbType,TFun(source.Type,TList output))) [cb;source]
                    else children recurse value
                | Lir.Intrinsic "List.fold",[cb;initial;source] ->
                    let source=rewrite true environment source
                    let actual=match source.Type with TList t -> t | _ -> invalidOp "Collection input."
                    let cb=recurse cb
                    if canAdapt actual cb 2 then
                        let cb=adapt actual cb
                        let initial=recurse initial
                        let cbType=TFun(initial.Type,TFun(actual,value.Type))
                        rebuild value (TFun(cbType,TFun(initial.Type,TFun(source.Type,value.Type)))) [{cb with Type=cbType};initial;source]
                    else children recurse value
                | Lir.Intrinsic "List.length",[source] ->
                    let source=rewrite true environment source
                    rebuild value (TFun(source.Type,value.Type)) [source]
                | _ -> children recurse value
        let declarations=program.Declarations |> List.map (fun item -> {item with Value=rewrite false Map.empty item.Value})
        let entry=Option.map (rewrite false Map.empty) program.Entry
        // Rewriting a consumer can update a different lifted function. Always read
        // its latest body, and do not overwrite a specialized parameter afterwards.
        for id in functions |> Map.toList |> List.map fst do
            let fn=functions.[id]
            let body=rewrite false Map.empty fn.Body
            functions <- Map.add id {functions.[id] with Body=body} functions
        {program with Functions=functions |> Map.toList |> List.map snd;Declarations=declarations;Entry=entry}
        |> LirAggregateElimination.optimize |> LirOptimization.optimize
