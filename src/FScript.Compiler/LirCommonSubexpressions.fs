namespace FScript.Compiler

open FScript.Language

/// Dominance-based value numbering. No expression is moved ahead of its first evaluation.
module LirCommonSubexpressions =
    // Spans are deliberately absent. Float bits retain signed zero and NaN payloads.
    type private Key = Key of Type * string * string list * Key list
    type private Environment =
        { Available: Map<Key, Lir.Expression>
          Values: Map<Lir.SymbolId, Key> }

    let optimize (program: Lir.Program) =
        let classify = LirEffects.create program
        let effects = System.Collections.Generic.Dictionary<Lir.Expression,LirEffects.Effect>(HashIdentity.Reference)
        let effect value =
            match effects.TryGetValue value with
            | true,result -> result
            | _ -> let result=classify value in effects.Add(value,result);result
        let types = program.Types |> List.map (fun value -> value.Name,value) |> Map.ofList
        let functions = program.Functions |> List.map (fun value -> value.Id,value) |> Map.ofList
        let mutable symbolTypes = Map.empty
        let mutable definitions = Map.empty
        let mutable maximum = -1
        let note id t =
            maximum <- max maximum (Lir.symbolNumber id)
            symbolTypes <- Map.add id t symbolTypes
        let rec plan (value: Lir.PatternPlan) =
            for binding in value.Bindings do note binding.Id binding.Type
            for map in value.Maps do
                for clause in map.Clauses do plan clause.Key; plan clause.Value
                Option.iter plan map.Tail
        let children mapper (value: Lir.Expression) =
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
        let rec collect (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id -> note id value.Type
            | Lir.Let(id,_,_,binding,_) -> note id binding.Type; definitions <- Map.add id binding definitions
            | Lir.LetRecursiveGroup(bindings,_) ->
                for id,_,t,binding in bindings do note id t; definitions <- Map.add id binding definitions
            | Lir.DecisionTree(_,cases) -> for case in cases do plan case.Plan
            | Lir.ForEach(id,_,t,_,_) -> note id t
            | _ -> ()
            children (fun child -> collect child;child) value |> ignore
        for typ in program.Types do maximum <- max maximum (Lir.symbolNumber typ.Id)
        for declaration in program.Declarations do
            note declaration.Id declaration.Type
            definitions <- Map.add declaration.Id declaration.Value definitions
            collect declaration.Value
        for fn in program.Functions do note fn.Parameter fn.ParameterType;collect fn.Body
        Option.iter collect program.Entry
        let generated = System.Collections.Generic.HashSet<Lir.SymbolId>()
        let pureGenerated = System.Collections.Generic.HashSet<Lir.SymbolId>()
        let fresh t =
            maximum <- maximum+1
            let id=Lir.SymbolId maximum
            symbolTypes <- Map.add id t symbolTypes
            generated.Add id |> ignore
            id
        let rec immutable seen = function
            | TNamed name | THostManaged(name,_) ->
                if Set.contains name seen then true else
                match Map.tryFind name types with
                | Some value when value.IsPure -> true
                | Some value when value.Import.IsNone ->
                    match value.Definition with THostManaged _ -> false | t -> immutable (Set.add name seen) t
                | _ -> false
            | TRecord fields -> fields |> Map.forall (fun _ t -> immutable seen t)
            | TUnion(name,_) when Map.tryFind name types |> Option.exists (fun value -> value.Import.IsSome) ->
                types.[name].IsPure
            | TUnion(_,cases) -> cases |> Map.forall (fun _ t -> Option.forall (immutable seen) t)
            | TTuple values -> List.forall (immutable seen) values
            | TList t | TOption t -> immutable seen t
            | TMap(a,b) -> immutable seen a && immutable seen b
            | TFun _ | TTask _ | TVar _ | TNumeric _ | TRecordShape _ -> false
            | _ -> true
        let rec callableType = function
            | TFun(a,b) -> callableType a && callableType b
            | t -> immutable Set.empty t
        let rec safeDependencies seen (value: Lir.Expression) =
            let symbol id =
                (Map.tryFind id symbolTypes |> Option.exists callableType) &&
                (if Set.contains id seen then true else
                    match Map.tryFind id symbolTypes,Map.tryFind id definitions with
                    | Some(TFun _),Some definition -> safeDependencies (Set.add id seen) definition
                    | _ -> true)
            match value.Node with
            | Lir.MakeClosure(id,captures) ->
                List.forall symbol captures &&
                (Map.tryFind id functions |> Option.forall (fun fn ->
                    // Closed function dependencies include globals as well as explicit captures.
                    LirOptimization.loadedSymbols fn.Body |> Set.forall symbol))
            | Lir.Load id -> symbol id
            | _ ->
                let mutable safe=true
                children (fun child -> safe <- safe && safeDependencies seen child; child) value |> ignore
                safe
        let key environment (value: Lir.Expression) =
            let rec build (value: Lir.Expression) =
                let node tag labels inputs =
                    let keys=List.map build inputs
                    if List.forall Option.isSome keys then Some(Key(value.Type,tag,labels,List.choose id keys)) else None
                match value.Node with
                | Lir.Load id -> Some(Map.tryFind id environment.Values |> Option.defaultValue (Key(value.Type,"load",[string (Lir.symbolNumber id)],[])))
                | Lir.Constant literal ->
                    let tag,text=match literal with
                                 | Lir.Unit -> "unit","" | Lir.Int n -> "int",string n
                                 | Lir.Float n -> "float",string (System.BitConverter.DoubleToInt64Bits n)
                                 | Lir.Bool b -> "bool",string b | Lir.String s -> "string",s
                    node tag [text] []
                | Lir.Intrinsic name -> node "intrinsic" [name] []
                | Lir.HostExternal(name,arity) -> node "external" [name;string arity] []
                | Lir.TargetImport(path,name) -> node "import" [path;name] []
                | Lir.MakeClosure _ -> None // Separately constructed callable handles retain their identity.
                | Lir.TypeToken t -> Some(Key(t,"type-token",[],[]))
                | Lir.NoneValue -> node "none" [] []
                | Lir.CallClosure(a,b) -> node "call" [] [a;b]
                | Lir.PipeClosure(a,b) -> node "call" [] [b;a]
                | Lir.Operation(op,values) -> node "operation" [string op] values
                | Lir.Tuple values -> node "tuple" [] values
                | Lir.List values -> node "list" [] values
                | Lir.Record fields -> node "record" (List.map fst fields) (List.map snd fields)
                | Lir.FieldGet(a,name) -> node "field" [name] [a]
                | Lir.SomeValue a -> node "some" [] [a]
                | Lir.Cons(a,b) -> node "cons" [] [a;b]
                | Lir.Append(a,b) -> node "append" [] [a;b]
                | Lir.Range(a,b) -> node "range" [] [a;b]
                | Lir.RecordUpdate(a,fields) -> node "update" (List.map fst fields) (a::List.map snd fields)
                | Lir.RecordConvert(a,_) -> node "convert" [] [a]
                | Lir.IndexGet(a,b) -> node "index" [] [a;b]
                | Lir.UnionCase(name,case,payload) -> node "union" [name;case] (Option.toList payload)
                | Lir.Interpolate parts ->
                    let labels=parts |> List.collect (function Lir.TextPart text -> ["text";text] | Lir.ValuePart _ -> ["value"])
                    node "interpolate" labels (parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart item -> Some item))
                | Lir.Map entries ->
                    let labels=entries |> List.map (function Lir.KeyValue _ -> "pair" | Lir.Spread _ -> "spread")
                    let inputs=entries |> List.collect (function Lir.KeyValue(a,b) -> [a;b] | Lir.Spread a -> [a])
                    node "map" labels inputs
                | _ -> None
            build value
        let empty = {Available=Map.empty;Values=Map.empty}
        let barrier environment = {environment with Available=Map.empty}
        let candidate (value: Lir.Expression) =
            match value.Node,value.Type with
            | Lir.Constant _,_ | Lir.Load _,_ | Lir.NoneValue,_ | Lir.TypeToken _,_
            | _,TFun _ | _,TTask _ -> false
            | _ -> immutable Set.empty value.Type && safeDependencies Set.empty value && effect value=LirEffects.Pure
        let atom (value: Lir.Expression) = match value.Node with Lir.Constant _ | Lir.Load _ | Lir.NoneValue | Lir.TypeToken _ | Lir.MakeClosure _ | Lir.Intrinsic _ | Lir.HostExternal _ -> true | _ -> false
        let finish _ value = value
        let rec rewrite environment (value: Lir.Expression) continuation =
            let bind environment rewritten reusable =
                let signature = if reusable then key environment rewritten else None
                match signature |> Option.bind (fun signature -> Map.tryFind signature environment.Available) with
                | Some existing -> continuation environment {existing with Span=value.Span}
                | None when atom rewritten -> continuation environment rewritten
                | None ->
                    let id=fresh rewritten.Type
                    if reusable then pureGenerated.Add id |> ignore
                    let load={rewritten with Node=Lir.Load id}
                    let environment=
                        match signature with
                        | Some signature -> {environment with Available=Map.add signature load environment.Available; Values=Map.add id signature environment.Values}
                        | None -> environment
                    let body=continuation environment load
                    {value with Type=body.Type; Node=Lir.Let(id,"$cse",false,rewritten,body)}
            let completed environment rewritten =
                bind (if effect value=LirEffects.Observable then barrier environment else environment) rewritten (candidate value)
            let rec operands environment values continuation =
                match values with
                | [] -> continuation environment []
                | first::rest -> rewrite environment first (fun environment first -> operands environment rest (fun environment rest -> continuation environment (first::rest)))
            match value.Node with
            | Lir.Let(id,name,false,binding,body) ->
                rewrite environment binding (fun environment binding ->
                    let scoped=match key environment binding with Some signature -> {environment with Values=Map.add id signature environment.Values} | None -> environment
                    let body=rewrite scoped body continuation
                    {value with Type=body.Type; Node=Lir.Let(id,name,false,binding,body)})
            | Lir.Let(id,name,true,binding,body) ->
                let binding=rewrite empty binding finish
                let body=rewrite (barrier environment) body continuation
                {value with Type=body.Type; Node=Lir.Let(id,name,true,binding,body)}
            | Lir.LetRecursiveGroup(bindings,body) ->
                let bindings=bindings |> List.map (fun (id,name,t,binding) -> id,name,t,rewrite empty binding finish)
                let body=rewrite (barrier environment) body continuation
                {value with Type=body.Type; Node=Lir.LetRecursiveGroup(bindings,body)}
            | Lir.Branch(condition,left,right) ->
                rewrite environment condition (fun environment condition ->
                    completed environment {value with Node=Lir.Branch(condition,rewrite environment left finish,rewrite environment right finish)})
            | Lir.Operation((Lir.BooleanAnd | Lir.BooleanOr) as op,[left;right]) ->
                rewrite environment left (fun environment left -> completed environment {value with Node=Lir.Operation(op,[left;rewrite environment right finish])})
            | Lir.DecisionTree(target,cases) ->
                rewrite environment target (fun environment target ->
                    let scoped=if immutable Set.empty target.Type then environment else barrier environment
                    let mutable preceding=scoped
                    let cases=cases |> List.map (fun case ->
                        let guardEnvironment=preceding
                        let bodyEnvironment=if case.Guard |> Option.exists (fun guard -> effect guard=LirEffects.Observable) then barrier preceding else preceding
                        preceding<-bodyEnvironment
                        {case with Guard=case.Guard |> Option.map (fun guard -> rewrite guardEnvironment guard finish);Body=rewrite bodyEnvironment case.Body finish})
                    completed environment {value with Node=Lir.DecisionTree(target,cases)})
            | Lir.ForEach(id,name,t,source,body) ->
                rewrite environment source (fun environment source -> completed environment {value with Node=Lir.ForEach(id,name,t,source,rewrite empty body finish)})
            | Lir.PipeClosure(argument,fn) | Lir.TailPipeClosure(argument,fn) ->
                operands environment [argument;fn] (fun environment values ->
                    let node=match value.Node with Lir.PipeClosure _ -> Lir.PipeClosure(values.[0],values.[1]) | _ -> Lir.TailPipeClosure(values.[0],values.[1])
                    completed environment {value with Node=node})
            | _ ->
                // Rebuild strict children in precisely their source evaluation order.
                let mutable inputs=[]
                children (fun child -> inputs<-child::inputs;child) value |> ignore
                operands environment (List.rev inputs) (fun environment values ->
                    let mutable remaining=values
                    let rewritten=children (fun _ -> let child=List.head remaining in remaining<-List.tail remaining;child) value
                    completed environment rewritten)
        // Drop unshared administrative temporaries; retained ones are ordinary lexical lets.
        let cleanup value =
            let rec substitute id (binding: Lir.Expression) (value: Lir.Expression) =
                match value.Node with Lir.Load found when found=id -> {binding with Span=value.Span} | _ -> children (substitute id binding) value
            let mutable result=value
            let mutable changed=true
            while changed do
                changed<-false
                let counts=System.Collections.Generic.Dictionary<Lir.SymbolId,int>()
                let captured=System.Collections.Generic.HashSet<Lir.SymbolId>()
                let rec count (value: Lir.Expression) =
                    match value.Node with
                    | Lir.Load id -> counts.[id]<-(match counts.TryGetValue id with true,n -> n | _ -> 0)+1
                    | Lir.MakeClosure(_,captures) -> for id in captures do captured.Add id |> ignore
                    | _ -> ()
                    children (fun child -> count child;child) value |> ignore
                count result
                let rec clean value =
                    let value=children clean value
                    match value.Node with
                    | Lir.Let(id,_,false,binding,body) when generated.Contains id && not (captured.Contains id) ->
                        match counts.TryGetValue id with
                        | true,1 -> changed<-true;substitute id binding body
                        | false,_ when pureGenerated.Contains id -> changed<-true;body
                        | _ -> value
                    | _ -> value
                result<-clean result
            result
        let optimizeExpression environment value = rewrite environment value finish |> cleanup
        let functions=program.Functions |> List.map (fun fn -> {fn with Body=optimizeExpression empty fn.Body})
        let mutable globals=empty
        let declarations=program.Declarations |> List.map (fun declaration ->
            let original=declaration.Value
            let value=optimizeExpression globals original
            if effect original=LirEffects.Observable then globals<-barrier globals
            elif immutable Set.empty original.Type || (match original.Node with Lir.Load _ -> true | _ -> false) then
                match key globals original with
                | Some signature ->
                    let load={original with Node=Lir.Load declaration.Id}
                    globals<-{globals with Available=(if candidate original then Map.add signature load globals.Available else globals.Available);Values=Map.add declaration.Id signature globals.Values}
                | None -> ()
            {declaration with Value=value})
        {program with Functions=functions;Declarations=declarations;Entry=program.Entry |> Option.map (optimizeExpression globals)}
