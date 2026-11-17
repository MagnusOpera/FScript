namespace FScript.Language

/// Generalization and forwarding rules shared by registered opaque host types.
/// Only type arguments participate; private payloads are never inspected.
module internal OpaqueInference =
    // Live variables belong to an existing opaque host value. Latent variables become
    // live when a factory is called, even if it returns only a captured sender.
    type OpaqueEffect =
        { Live: Set<int>
          Latent: Map<int, Set<int>>
          FactoryDelay: int option }

    let noOpaque = { Live=Set.empty; Latent=Map.empty; FactoryDelay=None }
    let combineOpaqueEffects effects =
        let effects=Seq.toList effects
        let combined=effects |> List.fold (fun combined effect ->
            { Live=Set.union combined.Live effect.Live
              Latent=effect.Latent |> Map.fold (fun latent depth variables ->
                  let existing=Map.tryFind depth latent |> Option.defaultValue Set.empty
                  Map.add depth (Set.union existing variables) latent) combined.Latent
              FactoryDelay=None }) noOpaque
        match effects with [effect] -> {combined with FactoryDelay=effect.FactoryDelay} | _ -> combined
    let mapOpaqueEffect mapType effect =
        let apply variables =
            variables |> Set.fold (fun result variable ->
                Set.union result (Types.ftvType (mapType (TVar variable)))) Set.empty
        { Live=apply effect.Live; Latent=effect.Latent |> Map.map (fun _ -> apply)
          FactoryDelay=effect.FactoryDelay }
    let opaqueVariables typeDefs typ =
        let rec collect visited typ =
            if Set.contains typ visited then Set.empty
            else
                let visited = Set.add typ visited
                let nested = collect visited
                match typ with
                | TApplied(name, arguments) ->
                    match Map.tryFind name typeDefs with
                    | Some(TGenericDefinition(_, THostManaged _)) ->
                        arguments |> List.map Types.ftvType |> List.fold Set.union Set.empty
                    | _ ->
                        let expanded = Types.expand typeDefs typ
                        if expanded = typ then Set.empty else nested expanded
                | TNamed _ ->
                    let expanded = Types.expand typeDefs typ
                    if expanded = typ then Set.empty else nested expanded
                // A function's result has not been allocated yet.
                | TFun _ -> Set.empty
                | TList item | TOption item | TTask item -> nested item
                | TTuple items -> items |> List.map nested |> List.fold Set.union Set.empty
                | TRecord fields | TRecordShape(_, fields, _) -> fields |> Map.values |> Seq.map nested |> Seq.fold Set.union Set.empty
                | TUnion(_, cases) -> cases |> Map.values |> Seq.choose id |> Seq.map nested |> Seq.fold Set.union Set.empty
                | TMap(key, value) -> Set.union (nested key) (nested value)
                | _ -> Set.empty
        collect Set.empty typ
    let recursiveOpaqueEffects effects =
        let combined=combineOpaqueEffects effects
        let latent=combined.Latent |> Map.values |> Seq.fold Set.union Set.empty
        {combined with Latent=if latent.IsEmpty then Map.empty else Map.ofList [1,latent]}

    let retainOpaqueEffect typ effect =
        let variables=Types.ftvType typ
        { Live=Set.intersect variables effect.Live
          Latent=effect.Latent |> Map.map (fun _ -> Set.intersect variables) |> Map.filter (fun _ variables -> not variables.IsEmpty)
          FactoryDelay=effect.FactoryDelay }

    type InferenceEnv =
        { Schemes: Map<string,Scheme>
          OpaqueEffects: Map<string,OpaqueEffect>
          Frames: ResizeArray<ResizeArray<OpaqueEffect>> }
        member env.ContainsKey name = env.Schemes.ContainsKey name
        member env.TryFind name = env.Schemes.TryFind name
        member env.Add(name,scheme) = { env with Schemes=env.Schemes.Add(name,scheme); OpaqueEffects=env.OpaqueEffects.Remove name }
        member env.AddValue(name,scheme,effect) =
            let (Forall(_,typ))=scheme
            let effect=retainOpaqueEffect typ effect
            { env with
                Schemes=env.Schemes.Add(name,scheme)
                OpaqueEffects=if effect=noOpaque then env.OpaqueEffects.Remove name else env.OpaqueEffects.Add(name,effect) }

    // Keep only variables belonging to an existing instance monomorphic.
    let generalizeValue typeDefs effect generalize typ =
        let (Forall(variables, body)) = generalize typ
        let live = Set.union effect.Live (opaqueVariables typeDefs typ)
        Forall(variables |> List.filter (fun variable -> not (Set.contains variable live)), body)

    // Prove forwarding from syntax and previously proved forwarding functions.
    // Unknown calls retain the conservative allocation rule; parameter shadowing
    // cannot inherit a proof from a function with the same name.
    let rec factoryDelay (known: Map<string,int>) expression =
        let delay = factoryDelay known
        let all values =
            let delays=List.map delay values
            if List.forall Option.isSome delays then Some(match List.choose id delays with [] -> 0 | values -> List.min values)
            else None
        let call fn argument =
            match forwardingFunction known fn,delay argument with
            | Some functionDelay,Some argumentDelay -> Some(functionDelay+argumentDelay)
            | _ -> None
        match expression with
        | ETypeApply(value,_,_) -> delay value
        | EVar _ | EUnit _ | ELiteral _ | ENone _ | ETypeOf _ | ENameOf _ -> Some 0
        | EParen(value,_) | EFieldGet(value,_,_) | ESome(value,_) -> delay value
        | ELambda(parameter,body,_) -> factoryDelay (known.Remove parameter.Name) body |> Option.map ((+) 1)
        | EApply(fn,argument,_) | EBinOp("|>",argument,fn,_) -> call fn argument
        | EIf(condition,left,right,_) ->
            match delay condition,delay left,delay right with
            | Some _,Some left,Some right -> Some(min left right)
            | _ -> None
        | ETuple(items,_) | EList(items,_) -> all items
        | ERecord(fields,_) | EStructuralRecord(fields,_) -> all (List.map snd fields)
        | ELet(name,value,body,false,_,_,_) when (delay value).IsSome ->
            let scoped=match forwardingFunction known value with Some value -> known.Add(name,value) | None -> known.Remove name
            factoryDelay scoped body
        | EMatch(target,cases,_) ->
            // Pattern binders may shadow known functions. Discard proofs for the
            // branch instead of assuming a projected callable is a forwarder.
            let branches=cases |> List.map (fun (_,guard,body,_) ->
                if guard |> Option.exists (fun value -> (factoryDelay Map.empty value).IsNone) then None
                else factoryDelay Map.empty body)
            if (delay target).IsNone || branches |> List.exists Option.isNone then None
            else Some(match List.choose id branches with [] -> 0 | values -> List.min values)
        | _ -> None

    and forwardingFunction known expression =
        match expression with
        | ETypeApply(value,_,_) -> forwardingFunction known value
        | EVar(name,_) -> Map.tryFind name known
        | EParen(value,_) -> forwardingFunction known value
        | ELambda(parameter,body,_) -> factoryDelay (Map.remove parameter.Name known) body
        | _ -> None
