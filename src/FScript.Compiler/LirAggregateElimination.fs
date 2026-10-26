namespace FScript.Compiler

open FScript.Language

/// Scalar replacement of nonescaping internal records. This operates on a private
/// backend copy of LIR and never changes the public artifact representation.
module LirAggregateElimination =
    let optimize (program: Lir.Program) =
        let children = LirTraversal.mapChildren
        let types = program.Types |> List.map (fun item -> item.Name,item) |> Map.ofList
        let imported = program.ImportedRecords |> List.map _.Name |> Set.ofList
        let rec fields seen = function
            | TRecord values -> Some values
            | TNamed name when not (Set.contains name seen) ->
                match Map.tryFind name types with
                | Some item when item.Import.IsNone && not (imported.Contains name) -> fields (Set.add name seen) item.Definition
                | _ -> None
            | _ -> None
        let fields = fields Set.empty
        // No imported getters/constructors or nested representation adaptations may
        // disappear merely because an enclosing record is temporary.
        let conversion source target =
            match fields source,fields target with
            | Some source,Some target -> target |> Map.forall (fun name t -> Map.tryFind name source=Some t)
            | _ -> false
        let symbolTypes = System.Collections.Generic.Dictionary<Lir.SymbolId,Type>()
        let mutable maximum = -1
        let note id = maximum <- max maximum (Lir.symbolNumber id)
        let rec collect (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id -> note id
            | Lir.Let(id,_,_,binding,_) -> note id;symbolTypes.[id] <- binding.Type
            | Lir.ForEach(id,_,t,_,_) -> note id;symbolTypes.[id] <- t
            | Lir.MakeClosure(_,captures) -> List.iter note captures
            | Lir.LetRecursiveGroup(bindings,_) -> bindings |> List.iter (fun (id,_,t,_) -> note id;symbolTypes.[id] <- t)
            | Lir.DecisionTree(_,cases) -> cases |> List.iter (fun item -> Lir.planBindingIds item.Plan |> Set.iter note)
            | _ -> ()
            children (fun child -> collect child;child) value |> ignore
        for item in program.Types do note item.Id
        for item in program.Declarations do note item.Id;symbolTypes.[item.Id] <- item.Type;collect item.Value
        for item in program.Functions do note item.Parameter;symbolTypes.[item.Parameter] <- item.ParameterType;collect item.Body
        Option.iter collect program.Entry
        let fresh () = maximum <- maximum+1;Lir.SymbolId maximum
        let bindFields (values: (string * Lir.Expression) list) (build: Map<string,Lir.Expression> -> Lir.Expression) =
            let bindings = values |> List.map (fun (name,value) -> name,fresh (),value)
            let loads = bindings |> List.map (fun (name,id,value) -> name,{value with Node=Lir.Load id}) |> Map.ofList
            let body = build loads
            (bindings,body) ||> List.foldBack (fun (_,id,binding) body -> {body with Node=Lir.Let(id,"$recordField",false,binding,body)})
        let literalFields (value: Lir.Expression) values =
            match fields value.Type with
            | Some declared -> values |> List.forall (fun (name,(item: Lir.Expression)) -> Map.tryFind name declared=Some item.Type)
            | None -> false
        let rec scalarUses id (value: Lir.Expression) =
            match value.Node with
            | Lir.FieldGet({Node=Lir.Load target},_) when target=id -> true
            | Lir.RecordUpdate(({Node=Lir.Load target} as original),updates) when target=id ->
                conversion original.Type value.Type && updates |> List.forall (snd >> scalarUses id)
            | Lir.RecordConvert(({Node=Lir.Load target} as original),t) when target=id -> conversion original.Type t
            | Lir.Load target when target=id -> false
            | Lir.MakeClosure(_,captures) when List.contains id captures -> false
            | _ ->
                let mutable safe=true
                children (fun child -> safe <- safe && scalarUses id child;child) value |> ignore
                safe
        let rec replace id replacement (value: Lir.Expression) =
            match value.Node with
            | Lir.Load target when target=id -> replacement
            | _ -> children (replace id replacement) value
        let rec simplify (expression: Lir.Expression) =
            let value = children simplify expression
            let again node = simplify {value with Node=node}
            // Move a consumer through administrative bindings, without moving the
            // computation itself or evaluating its input more than once.
            let through id name binding body make =
                again (Lir.Let(id,name,false,binding,{value with Node=make body}))
            match value.Node with
            | Lir.Load id when symbolTypes.ContainsKey id && symbolTypes.[id]<>value.Type && conversion symbolTypes.[id] value.Type ->
                again (Lir.RecordConvert({value with Type=symbolTypes.[id]},value.Type))
            | Lir.Let(id,name,false,{Node=Lir.Let(inner,innerName,false,binding,result)},body) ->
                through inner innerName binding result (fun result -> Lir.Let(id,name,false,result,body))
            | Lir.FieldGet({Node=Lir.Let(id,name,false,binding,body)},field) ->
                through id name binding body (fun body -> Lir.FieldGet(body,field))
            | Lir.RecordUpdate({Node=Lir.Let(id,name,false,binding,body)},updates) ->
                through id name binding body (fun body -> Lir.RecordUpdate(body,updates))
            | Lir.RecordConvert({Node=Lir.Let(id,name,false,binding,body)},target) ->
                through id name binding body (fun body -> Lir.RecordConvert(body,target))
            | Lir.FieldGet({Node=Lir.RecordConvert(original,target)},field) when conversion original.Type target ->
                again (Lir.FieldGet(original,field))
            | Lir.RecordConvert(original,target) when original.Type=target -> original
            | Lir.FieldGet(({Node=Lir.Record values} as original),field) when literalFields original values ->
                bindFields values (fun loads -> loads.[field]) |> simplify
            | Lir.RecordConvert(({Node=Lir.Record values} as original),target) when literalFields original values && conversion original.Type target ->
                bindFields values (fun loads -> {value with Node=Lir.Record(fields target |> Option.get |> Map.toList |> List.map (fun (name,_) -> name,loads.[name]))}) |> simplify
            | Lir.RecordUpdate(({Node=Lir.Record values} as original),updates) when literalFields original values && conversion original.Type value.Type ->
                bindFields values (fun loads -> bindFields updates (fun updated ->
                    let merged=Map.fold (fun fields name item -> Map.add name item fields) loads updated
                    {value with Node=Lir.Record(fields value.Type |> Option.get |> Map.toList |> List.map (fun (name,_) -> name,merged.[name]))})) |> simplify
            | Lir.Let(id,_,false,({Node=Lir.Record values} as binding),body) when literalFields binding values && scalarUses id body ->
                bindFields values (fun loads -> replace id {binding with Node=Lir.Record(Map.toList loads)} body) |> simplify
            | Lir.Let(id,name,false,({Node=Lir.RecordConvert(original,target)} as binding),body) when conversion original.Type target && scalarUses id body ->
                let sourceId=fresh ()
                let source={original with Node=Lir.Load sourceId}
                let projections=fields target |> Option.get |> Map.toList |> List.map (fun (field,t) -> field,({Type=t;Span=binding.Span;Node=Lir.FieldGet(source,field)}: Lir.Expression))
                again (Lir.Let(sourceId,"$recordSource",false,original,{value with Node=Lir.Let(id,name,false,{binding with Node=Lir.Record projections},body)}))
            | _ -> value
        // A lifted function may return a named value under a structural signature.
        // Make that implicit boundary conversion explicit, then construct directly
        // in the demanded representation instead of allocating and copying twice.
        let rec adapt target (value: Lir.Expression) =
            if value.Type=target || not (conversion value.Type target) then value else
                let withNode node = {value with Type=target;Node=node}
                match value.Node with
                | Lir.Let(id,name,recursive,binding,body) -> withNode (Lir.Let(id,name,recursive,binding,adapt target body))
                | Lir.Branch(condition,yes,no) -> withNode (Lir.Branch(condition,adapt target yes,adapt target no))
                | Lir.DecisionTree(input,cases) -> withNode (Lir.DecisionTree(input,cases |> List.map (fun item -> {item with Body=adapt target item.Body})))
                | _ -> withNode (Lir.RecordConvert(value,target)) |> simplify
        {program with
            Functions=program.Functions |> List.map (fun item -> {item with Body=item.Body |> simplify |> adapt item.ReturnType |> simplify})
            Declarations=program.Declarations |> List.map (fun item -> {item with Value=item.Value |> simplify |> adapt item.Type |> simplify})
            Entry=Option.map simplify program.Entry}
