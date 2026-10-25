namespace FScript.Compiler

open FScript.Language

/// Shared concrete type substitutions and deterministic specialization identities.
module NativeSpecialization =
    type Substitution = Map<int, Type>

    type SpecializationKey =
        { Symbol: int
          ConcreteType: string }

    let rec containsVariable = function
        | TVar _ | TNumeric _ | TRecordShape _ -> true
        | TFun(argument, result) | TMap(argument, result) -> containsVariable argument || containsVariable result
        | TTuple items -> List.exists containsVariable items
        | TList item | TOption item | TTask item -> containsVariable item
        | TRecord fields -> fields |> Map.exists (fun _ -> containsVariable)
        | TUnion(_, cases) -> cases |> Map.exists (fun _ -> Option.exists containsVariable)
        | _ -> false

    let rec apply (substitution: Substitution) = function
        | (TVar variable | TNumeric variable) as value -> substitution |> Map.tryFind variable |> Option.defaultValue value
        | TRecordShape(id, fields, constructible) ->
            substitution |> Map.tryFind id |> Option.defaultValue (TRecordShape(id, Map.map (fun _ -> apply substitution) fields, constructible))
        | TFun(argument, result) -> TFun(apply substitution argument, apply substitution result)
        | TTuple items -> TTuple(List.map (apply substitution) items)
        | TList item -> TList(apply substitution item)
        | TOption item -> TOption(apply substitution item)
        | TMap(key, value) -> TMap(apply substitution key, apply substitution value)
        | TTask item -> TTask(apply substitution item)
        | TRecord fields -> TRecord(fields |> Map.map (fun _ -> apply substitution))
        | TUnion(name, cases) -> TUnion(name, cases |> Map.map (fun _ -> Option.map (apply substitution)))
        | value -> value

    let unifyInstantiation template concrete : Result<Substitution, string> =
        let rec loop substitution template concrete =
            match apply substitution template, concrete with
            | TRecordShape(variable, _, _), value | TVar variable, value | TNumeric variable, value ->
                match substitution |> Map.tryFind variable with
                | None -> Ok(Map.add variable value substitution)
                | Some existing when existing = value -> Ok substitution
                | Some existing -> Error $"Type variable '{variable}' maps to both {Types.typeToString existing} and {Types.typeToString value}."
            | TFun(ta, tr), TFun(ca, cr) | TMap(ta, tr), TMap(ca, cr) ->
                loop substitution ta ca |> Result.bind (fun next -> loop next tr cr)
            | TTuple left, TTuple right when left.Length = right.Length ->
                List.zip left right |> List.fold (fun state (l, r) -> state |> Result.bind (fun next -> loop next l r)) (Ok substitution)
            | TList left, TList right | TOption left, TOption right | TTask left, TTask right -> loop substitution left right
            | TRecord left, TRecord right when left.Count = right.Count && (left |> Map.forall (fun name _ -> Map.containsKey name right)) ->
                left |> Map.toList |> List.fold (fun state (name, item) -> state |> Result.bind (fun next -> loop next item right.[name])) (Ok substitution)
            | TUnion(left, _), TUnion(right, _) when left = right -> Ok substitution
            | left, right when left = right -> Ok substitution
            | left, right -> Error $"Cannot specialize {Types.typeToString left} as {Types.typeToString right}."
        loop Map.empty template concrete

    let private typeKey value = Types.typeToString value

    let specializationKey symbol concreteType =
        { Symbol = symbol
          ConcreteType = typeKey concreteType }
