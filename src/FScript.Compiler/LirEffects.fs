namespace FScript.Compiler

open FScript.Language

/// Pure means total as well as deterministic and effect-free. Unknowns stay observable.
module LirEffects =
    type Effect = Pure | Observable
    let private combine values = if List.contains Observable values then Observable else Pure

    let private totalIntrinsics =
        set [ "ignore"; "Int.tryParse"; "Float.tryParse"; "Bool.tryParse"; "Int.toString"; "Float.toString"; "Bool.toString"
              "String.replace"; "String.indexOf"; "String.toLower"; "String.toUpper"; "String.substring"; "String.concat"; "String.split"; "String.endsWith"
              "List.empty"; "List.contains"; "List.rev"; "List.distinct"; "List.length"; "List.tryItem"; "List.tryHead"; "List.append"
              "Option.defaultValue"; "Option.isNone"; "Option.isSome"
              "Map.empty"; "Map.tryGet"; "Map.containsKey"; "Map.add"; "Map.ofList"; "Map.count"; "Map.remove" ]

    let higherOrder = set ["List.map";"List.iter";"List.choose";"List.collect";"List.exists";"List.fold";"List.filter";"List.tryFind";"List.tryGet";"Option.defaultWith";"Option.map";"Map.fold";"Map.filter";"Map.choose";"Map.map";"Map.iter"]

    let create (program: Lir.Program) =
        let declarations = program.Declarations |> List.map (fun value -> value.Id,value) |> Map.ofList
        let functions = program.Functions |> List.map (fun value -> value.Id,value) |> Map.ofList
        let types = program.Types |> List.map (fun value -> value.Name,value) |> Map.ofList
        let mutable summaries = functions |> Map.map (fun _ _ -> Observable)
        let rec arity = function TFun(_,result) -> 1 + arity result | _ -> 0
        let rec stable seen = function
            | TNamed name | THostManaged(name,_) ->
                if Set.contains name seen then true else
                match Map.tryFind name types with
                | Some value when value.IsPure -> true
                | Some value when value.Import.IsNone ->
                    match value.Definition with
                    | THostManaged _ -> false
                    | definition -> stable (Set.add name seen) definition
                | _ -> false
            | TRecord fields -> fields |> Map.forall (fun _ value -> stable seen value)
            | TUnion(_,cases) -> cases |> Map.forall (fun _ value -> value |> Option.forall (stable seen))
            | TTuple values -> List.forall (stable seen) values
            | TList value | TOption value -> stable seen value
            | TMap(key,value) -> stable seen key && stable seen value
            | TVar _ | TNumeric _ | TRecordShape _ | TTask _ -> false
            | _ -> true
        let externalConstruction = function
            | TNamed name | THostManaged(name,_) -> Map.tryFind name types |> Option.exists (fun value -> value.Import.IsSome)
            | _ -> false
        let rec hasCallback seen = function
            | TFun _ -> true
            | TTuple values -> List.exists (hasCallback seen) values
            | TList value | TOption value | TTask value -> hasCallback seen value
            | TMap(key,value) -> hasCallback seen key || hasCallback seen value
            | TRecord fields | TRecordShape(_,fields,_) | THostManaged(_,fields) -> fields |> Map.exists (fun _ value -> hasCallback seen value)
            | TUnion(_,cases) -> cases |> Map.exists (fun _ value -> value |> Option.exists (hasCallback seen))
            | TNamed name when not (Set.contains name seen) -> Map.tryFind name types |> Option.exists (fun value -> hasCallback (Set.add name seen) value.Definition)
            | _ -> false
        let rec resolve environment seen (value: Lir.Expression) =
            match value.Node with
            | Lir.Load id when not (Set.contains id seen) ->
                match Map.tryFind id environment |> Option.orElseWith (fun () -> Map.tryFind id declarations |> Option.map _.Value) with
                | Some binding -> resolve environment (Set.add id seen) binding
                | None -> value
            | _ -> value
        let externalMetadata =
            program.Declarations
            |> List.choose (fun value -> match value.Value.Node with Lir.HostExternal _ | Lir.TargetImport _ -> Some(value.Value.Node,value) | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (node,values) ->
                let bindings=values |> List.map snd
                let arity=match bindings |> List.map _.ExternalArity |> List.distinct with [Some count] -> Some count | _ -> None
                node,(List.forall (fun (binding: Lir.Declaration) -> binding.IsPure) bindings,arity))
            |> Map.ofList
        let declaredPure node = externalMetadata |> Map.tryFind node |> Option.exists fst
        let declaredArity node = externalMetadata |> Map.tryFind node |> Option.bind snd
        let nonzeroDivision operation (values: Lir.Expression list) =
            match operation,values with
            | Lir.NumericDivide,[left;_] when left.Type=TFloat -> true
            | _,[left;{Node=Lir.Constant(Lir.Int divisor)}] when divisor<>0L ->
                divisor <> -1L || (match left.Node with Lir.Constant(Lir.Int dividend) -> dividend<>System.Int64.MinValue | _ -> false)
            | _ -> false
        let exhaustive (cases: Lir.DecisionCase list) =
            cases |> List.tryLast |> Option.exists (fun (case: Lir.DecisionCase) -> case.Guard.IsNone && case.Plan.Checks.IsEmpty && case.Plan.Maps.IsEmpty)
        let rec effect environment (value: Lir.Expression) =
            let child = effect environment
            match value.Node with
            | Lir.Constant _ | Lir.Load _ | Lir.MakeClosure _ | Lir.NoneValue | Lir.Intrinsic _ | Lir.HostExternal _ | Lir.RuntimeContext | Lir.TypeToken _ -> Pure
            | Lir.TargetImport _ -> if declaredArity value.Node=Some 0 && not (declaredPure value.Node) then Observable else Pure
            | Lir.CallClosure _ | Lir.PipeClosure _ | Lir.TailCallClosure _ | Lir.TailPipeClosure _ ->
                let rec spine (value: Lir.Expression) (arguments: Lir.Expression list) =
                    match value.Node with
                    | Lir.CallClosure(fn,arg) | Lir.TailCallClosure(fn,arg) | Lir.PipeClosure(arg,fn) | Lir.TailPipeClosure(arg,fn) -> spine fn (arg::arguments)
                    | _ -> value,arguments
                let root,args=spine value []
                combine (child root :: (args |> List.map child) @ [apply environment Set.empty root args])
            | Lir.Fail _ | Lir.ForEach _ -> Observable
            | Lir.Operation((Lir.IntDivide | Lir.IntRemainder | Lir.NumericDivide) as operation,values) ->
                if nonzeroDivision operation values then combine (List.map child values) else Observable
            | Lir.Operation(Lir.StructuralEqual,values) -> if List.forall (fun (value: Lir.Expression) -> stable Set.empty value.Type) values then combine (List.map child values) else Observable
            | Lir.Operation(_,values) | Lir.Tuple values | Lir.List values -> combine (List.map child values)
            | Lir.Branch(a,b,c) -> combine [child a;child b;child c]
            | Lir.Record fields -> if externalConstruction value.Type then Observable else combine (fields |> List.map (snd >> child))
            | Lir.FieldGet(target,_) -> if stable Set.empty target.Type then child target else Observable
            | Lir.RecordConvert(target,resultType) -> if stable Set.empty target.Type && not (externalConstruction resultType) then child target else Observable
            | Lir.SomeValue target -> child target
            | Lir.Cons(a,b) | Lir.Append(a,b) | Lir.Range(a,b) | Lir.IndexGet(a,b) -> combine [child a;child b]
            | Lir.Let(id,_,false,binding,body) -> combine [child binding;effect (Map.add id binding environment) body]
            | Lir.Let(id,_,true,binding,body) ->
                let scoped=Map.add id binding environment
                combine [effect scoped binding;effect scoped body]
            | Lir.LetRecursiveGroup(bindings,body) ->
                let scoped=bindings |> List.fold (fun scoped (id,_,_,binding) -> Map.add id binding scoped) environment
                combine (effect scoped body :: (bindings |> List.map (fun (_,_,_,binding) -> effect scoped binding)))
            | Lir.DecisionTree(target,cases) ->
                if not (stable Set.empty target.Type && exhaustive cases) then Observable else
                combine (child target :: (cases |> List.collect (fun case -> child case.Body :: (case.Guard |> Option.map child |> Option.toList))))
            | Lir.RecordUpdate(target,fields) -> if stable Set.empty target.Type && not (externalConstruction value.Type) then combine (child target :: (fields |> List.map (snd >> child))) else Observable
            | Lir.Interpolate parts ->
                parts |> List.map (function Lir.TextPart _ -> Pure | Lir.ValuePart item -> if stable Set.empty item.Type then child item else Observable) |> combine
            | Lir.UnionCase(_,_,payload) -> payload |> Option.map child |> Option.defaultValue Pure
            | Lir.Map entries -> entries |> List.collect (function Lir.KeyValue(key,item) -> [child key;child item] | Lir.Spread item -> [child item]) |> combine
        and apply environment seen (value: Lir.Expression) (arguments: Lir.Expression list) =
            let resolved=resolve environment Set.empty value
            let externalCall count isPure =
                if arguments.Length<count then Pure
                elif arguments.Length<>count || not isPure then Observable
                else
                    arguments |> List.map (fun argument ->
                        match argument.Type with
                        | TFun _ ->
                            let rec dummyArguments valueType : Lir.Expression list =
                                match valueType with
                                | TFun(input,result) -> {Type=input;Span=argument.Span;Node=Lir.Load(Lir.SymbolId -1)} :: dummyArguments result
                                | _ -> []
                            apply environment seen argument (dummyArguments argument.Type)
                        | t when hasCallback Set.empty t -> Observable
                        | _ -> Pure) |> combine
            match resolved.Node with
            | Lir.MakeClosure(id,_) ->
                if Set.contains id seen then Observable else
                match Map.tryFind id functions,arguments with
                | Some fn,_::rest ->
                    let current=Map.tryFind id summaries |> Option.defaultValue Observable
                    if current=Observable || rest.IsEmpty then current
                    else apply environment (Set.add id seen) fn.Body rest
                | _ -> Observable
            | Lir.HostExternal(_,count) -> externalCall count (declaredPure resolved.Node)
            | Lir.TargetImport _ ->
                match declaredArity resolved.Node with
                | Some count -> externalCall count (declaredPure resolved.Node)
                | None -> Observable
            | Lir.Intrinsic name ->
                let count=Map.tryFind name BuiltinSignatures.builtinSchemes |> Option.map (fun (Forall(_,t)) -> arity t) |> Option.defaultValue (arity resolved.Type)
                // Known intrinsic signatures identify which arguments are actually invoked.
                if List.contains name ["Console.writeLine";"Console.readLine";"Fs.readText"] then Observable
                elif arguments.Length<count then Pure
                elif arguments.Length=count && Set.contains name totalIntrinsics then
                    if List.contains name ["List.contains";"List.distinct"] && not (arguments |> List.forall (fun value -> stable Set.empty value.Type)) then Observable else Pure
                elif arguments.Length=count && Set.contains name higherOrder then
                    match arguments,Map.tryFind name BuiltinSignatures.builtinSchemes with
                    | callback::_,Some(Forall(_,TFun(callbackType,_))) ->
                        let rec dummy valueType : Lir.Expression list =
                            match valueType with
                            | TFun(input,result) -> {Type=input;Span=callback.Span;Node=Lir.Load(Lir.SymbolId -1)} :: dummy result
                            | _ -> []
                        apply environment seen callback (dummy callbackType)
                    | _ -> Observable
                else Observable
            | Lir.Let(id,_,false,binding,body) -> combine [effect environment binding;apply (Map.add id binding environment) seen body arguments]
            | Lir.Branch(condition,left,right) -> combine [effect environment condition;apply environment seen left arguments;apply environment seen right arguments]
            | _ -> Observable
        // Starting at Observable prevents recursive cycles from being mistaken for totality.
        let mutable changed=true
        while changed do
            changed <- false
            for KeyValue(id,fn) in functions do
                let next=effect Map.empty fn.Body
                if next<>summaries.[id] then summaries<-Map.add id next summaries;changed<-true
        effect Map.empty
