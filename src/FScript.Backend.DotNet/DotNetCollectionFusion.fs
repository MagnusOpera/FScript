namespace FScript.Compiler

open FScript.Language

/// Pure, total, single-consumer pipelines become bounded tail loops in ordinary
/// LIR. Existing call reduction, scalar replacement and tail lowering do the rest.
module internal DotNetCollectionFusion =
    type private Stage = Map of Lir.Expression | Filter of Lir.Expression

    let optimize (program: Lir.Program) =
        let children=LirTraversal.mapChildren
        let effect=LirEffects.create program
        let mutable maximum = -1
        let note id=maximum <- max maximum (Lir.symbolNumber id)
        let rec collect (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id | Lir.Let(id,_,_,_,_) | Lir.ForEach(id,_,_,_,_) -> note id
            | Lir.MakeClosure(_,captures) -> List.iter note captures
            | Lir.LetRecursiveGroup(bindings,_) -> bindings |> List.iter (fun (id,_,_,_) -> note id)
            | Lir.DecisionTree(_,cases) -> cases |> List.iter (fun item -> Set.iter note (Lir.planBindingIds item.Plan))
            | _ -> ()
            children (fun child -> collect child;child) value |> ignore
        for item in program.Types do note item.Id
        for item in program.Declarations do note item.Id;collect item.Value
        for fn in program.Functions do note fn.Parameter;collect fn.Body
        Option.iter collect program.Entry
        let fresh ()=maximum <- maximum+1;Lir.SymbolId maximum
        let mutable nextFunction=program.Functions |> List.map (fun fn -> Lir.functionNumber fn.Id) |> List.fold max -1 |> (+) 1
        let freshFunction ()=let id=Lir.FunctionId nextFunction in nextFunction <- nextFunction+1;id
        let added=ResizeArray<Lir.Function>()
        let rec spine (value: Lir.Expression) =
            match value.Node with
            | Lir.CallClosure(fn,arg) | Lir.TailCallClosure(fn,arg)
            | Lir.PipeClosure(arg,fn) | Lir.TailPipeClosure(arg,fn) ->
                let root,args=spine fn
                root,args@[arg]
            | _ -> value,[]
        let rec countUses id (value: Lir.Expression) =
            match value.Node with
            | Lir.Load symbol when symbol=id -> 1
            | Lir.MakeClosure(_,captures) when List.contains id captures -> 2 // escaping
            | _ ->
                let mutable count=0
                children (fun child -> count <- count+countUses id child;child) value |> ignore
                count
        let rec replace id replacement (value: Lir.Expression) =
            match value.Node with
            | Lir.Load symbol when symbol=id -> replacement
            | _ -> children (replace id replacement) value
        let rec expose (value: Lir.Expression) =
            let value=children expose value
            match value.Node with
            | Lir.Let(id,_,false,binding,body) when effect binding=LirEffects.Pure && effect body=LirEffects.Pure && countUses id body=1 ->
                expose (replace id binding body)
            | _ -> value
        let create (original: Lir.Expression) (source: Lir.Expression) stages (folder: Lir.Expression) (initial: Lir.Expression) =
            let prefixes=ResizeArray<Lir.SymbolId * string * Lir.Expression>()
            let rec prepare (value: Lir.Expression) =
                match value.Node with
                | Lir.Let(id,name,false,binding,body) when effect binding=LirEffects.Pure -> prefixes.Add(id,name,binding);prepare body
                | _ -> value
            let stages=stages |> List.map (function Map fn -> Map(prepare fn) | Filter fn -> Filter(prepare fn))
            let folder=prepare folder
            let span=original.Span
            let make t node : Lir.Expression={Type=t;Span=span;Node=node}
            let load t id=make t (Lir.Load id)
            let bind id binding (body: Lir.Expression)=make body.Type (Lir.Let(id,"$fused",false,binding,body))
            let call (fn: Lir.Expression) argument =
                let result=match fn.Type with TFun(_,result) -> result | _ -> invalidOp "Fused callback signature."
                make result (Lir.CallClosure(fn,argument))
            let operation t op args=make t (Lir.Operation(op,args))
            let int value=make TInt (Lir.Constant(Lir.Int value))
            let stateType=original.Type
            let outer,inner=freshFunction (),freshFunction ()
            let self,index,state=fresh (),fresh (),fresh ()
            let loopType=TFun(TInt,TFun(stateType,stateType))
            let current=load TInt index
            let accumulator=load stateType state
            let first,last,sourceId,initialId=fresh (),fresh (),fresh (),fresh ()
            let isRange=match source.Node with Lir.Range _ -> true | _ -> false
            // The state expression must be evaluated once before the termination
            // check; range endpoints may be Int64.MinValue/MaxValue.
            let continueWith result =
                let stateId=fresh ()
                let nextState=load stateType stateId
                let nextIndex=
                    if isRange then make TInt (Lir.Branch(operation TBool Lir.NumericLessThanOrEqual [load TInt first;load TInt last],operation TInt Lir.IntAdd [current;int 1L],operation TInt Lir.IntSubtract [current;int 1L]))
                    else operation TInt Lir.IntAdd [current;int 1L]
                let next=call (call (load loopType self) nextIndex) nextState
                let body=if isRange then make stateType (Lir.Branch(operation TBool Lir.StructuralEqual [current;load TInt last],nextState,next)) else next
                bind stateId result body
            let rec applyStages input = function
                | [] -> continueWith (call (call folder accumulator) input)
                | Map mapper::rest ->
                    let mapped=call mapper input
                    let id=fresh ()
                    bind id mapped (applyStages (load mapped.Type id) rest)
                | Filter predicate::rest -> make stateType (Lir.Branch(call predicate input,applyStages input rest,continueWith accumulator))
            let itemType=match source.Type with TList t -> t | _ -> invalidOp "Fused source type."
            let body=
                if isRange then applyStages current stages else
                    let item=fresh ()
                    let input=make (TOption itemType) (Lir.IndexGet(load source.Type sourceId,current))
                    let plan: Lir.PatternPlan={Span=span;Checks=[Lir.SomeCheck []];Bindings=[{Id=item;Name="$item";Type=itemType;Projection=[Lir.OptionValue]}];Maps=[]}
                    let fallback: Lir.PatternPlan={Span=span;Checks=[];Bindings=[];Maps=[]}
                    let failure=make stateType (Lir.Fail(make TString (Lir.Constant(Lir.String "Invalid fused collection index."))))
                    let decision=make stateType (Lir.DecisionTree(input,[{Plan=plan;Guard=None;Body=applyStages (load itemType item) stages;Span=span};{Plan=fallback;Guard=None;Body=failure;Span=span}]))
                    let length=make TInt (Lir.CallClosure(make (TFun(source.Type,TInt)) (Lir.Intrinsic "List.length"),load source.Type sourceId))
                    make stateType (Lir.Branch(operation TBool Lir.NumericLessThan [current;length],decision,accumulator))
            let innerCaptures=LirLowering.capturedReferences (Set.singleton state) body |> Set.toList
            added.Add({Id=inner;Parameter=state;Name="$fusedState";ParameterType=stateType;Captures=innerCaptures;ReturnType=stateType;Span=span;Body=body})
            let closure=make (TFun(stateType,stateType)) (Lir.MakeClosure(inner,innerCaptures))
            let outerCaptures=LirLowering.capturedReferences (Set.singleton index) closure |> Set.toList
            added.Add({Id=outer;Parameter=index;Name="$fusedIndex";ParameterType=TInt;Captures=outerCaptures;ReturnType=closure.Type;Span=span;Body=closure})
            let start=if isRange then load TInt first else int 0L
            let invocation=call (call (load loopType self) start) (load initial.Type initialId)
            let result=make stateType (Lir.Let(self,"$fusedLoop",true,make loopType (Lir.MakeClosure(outer,outerCaptures)),invocation))
            let result=bind initialId initial result
            let result=
                match source.Node with
                | Lir.Range(a,b) -> bind first a (bind last b result)
                | _ -> bind sourceId source result
            (List.ofSeq prefixes,result) ||> List.foldBack (fun (id,name,binding) body -> make body.Type (Lir.Let(id,name,false,binding,body)))
        let rec rewrite (value: Lir.Expression) =
            // Match the complete producer chain before visiting its subexpressions.
            let root,args=spine value
            let rec producers source stages =
                let root,args=spine source
                match root.Node,args with
                | Lir.Intrinsic "List.map",[callback;input] -> producers input (Map callback::stages)
                | Lir.Intrinsic "List.filter",[callback;input] -> producers input (Filter callback::stages)
                | _ -> source,stages
            match root.Node,args with
            | Lir.Intrinsic "List.fold",[folder;initial;source] when effect value=LirEffects.Pure ->
                let source,stages=producers source []
                if stages.IsEmpty then children rewrite value else create value source stages folder initial
            | _ -> children rewrite value
        let functions=program.Functions |> List.map (fun fn -> {fn with Body=fn.Body |> expose |> rewrite})
        let declarations=program.Declarations |> List.map (fun item -> {item with Value=item.Value |> expose |> rewrite})
        let entry=program.Entry |> Option.map (expose >> rewrite)
        {program with Functions=functions @ List.ofSeq added;Declarations=declarations;Entry=entry}
        |> LirOptimization.optimize
