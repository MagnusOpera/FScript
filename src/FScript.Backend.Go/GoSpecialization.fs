namespace FScript.Compiler

open FScript.Language

/// Closed storage shapes consumed by the Go source emitter. This is deliberately
/// backend-private in purpose even though its helpers are visible to the test assembly.
module GoSpecialization =
    let private map2 mapper left right =
        match left, right with
        | Ok leftValue, Ok rightValue -> Ok(mapper leftValue rightValue)
        | Error error, _ | _, Error error -> Error error

    type Substitution = Map<int, Type>

    type Representation =
        | Unit
        | Int
        | Float
        | Bool
        | String
        | TypeToken
        | Function of Representation * Representation
        | Tuple of Representation list
        | List of Representation
        | Option of Representation
        | Task of Representation
        | OrderedMap of Representation * Representation
        | StructuralRecord of (string * Representation) list
        | Named of name: string * recursive: bool
        | Union of name: string * recursive: bool
        | Bottom

    type SpecializationKey =
        { Symbol: int
          ConcreteType: string }

    type Effect =
        | Pure
        | Observable

    let rec containsVariable = function
        | TVar _ -> true
        | TFun(argument, result) | TMap(argument, result) -> containsVariable argument || containsVariable result
        | TTuple items -> List.exists containsVariable items
        | TList item | TOption item | TTask item -> containsVariable item
        | TRecord fields -> fields |> Map.exists (fun _ -> containsVariable)
        | TUnion(_, cases) -> cases |> Map.exists (fun _ -> Option.exists containsVariable)
        | _ -> false

    let rec apply (substitution: Substitution) = function
        | TVar variable as value -> substitution |> Map.tryFind variable |> Option.defaultValue value
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
            | TVar variable, value ->
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

    let tryRepresentation (types: Lir.TypeDeclaration list) value : Result<Representation, string> =
        let declarations = types |> List.map (fun item -> item.Name, item) |> Map.ofList
        let rec representation = function
            | TUnit -> Ok Unit
            | TInt -> Ok Int
            | TFloat -> Ok Float
            | TBool -> Ok Bool
            | TString -> Ok String
            | TTypeToken -> Ok TypeToken
            | TVar variable -> Error $"Open type variable '{variable}' has no Go representation."
            | TTask item -> representation item |> Result.map Task
            | TFun(argument, result) -> map2 (fun a r -> Function(a, r)) (representation argument) (representation result)
            | TTuple items -> items |> List.map representation |> List.fold (fun state item -> map2 (fun values value -> values @ [ value ]) state item) (Ok []) |> Result.map Tuple
            | TList item -> representation item |> Result.map List
            | TOption item -> representation item |> Result.map Option
            | TMap(key, value) -> map2 (fun k v -> OrderedMap(k, v)) (representation key) (representation value)
            | TRecord fields ->
                fields |> Map.toList |> List.map (fun (name, item) -> representation item |> Result.map (fun value -> name, value))
                |> List.fold (fun state item -> map2 (fun values value -> values @ [ value ]) state item) (Ok [])
                |> Result.map StructuralRecord
            | TNamed name ->
                match declarations |> Map.tryFind name with
                | Some declaration ->
                    match declaration.Definition with
                    | TUnion _ -> Ok(Union(name, declaration.IsRecursive))
                    | _ -> Ok(Named(name, declaration.IsRecursive))
                | None -> Error $"Unknown named Go representation '{name}'."
            | TUnion(name, _) ->
                let recursive = declarations |> Map.tryFind name |> Option.exists _.IsRecursive
                Ok(Union(name, recursive))
        representation value

    let rec effect (expression: Lir.Expression) =
        let combine values = if List.exists ((=) Observable) values then Observable else Pure
        let child = effect
        match expression.Node with
        | Lir.Constant _ | Lir.Load _ | Lir.MakeClosure _ | Lir.NoneValue | Lir.Intrinsic _
        | Lir.HostExternal _ | Lir.RuntimeContext | Lir.TargetImport _ | Lir.TypeToken _ -> Pure
        | Lir.CallClosure _ | Lir.PipeClosure _ | Lir.Fail _ -> Observable
        | Lir.Branch(condition, whenTrue, whenFalse) -> combine [ child condition; child whenTrue; child whenFalse ]
        | Lir.Operation(_, values) | Lir.Tuple values | Lir.List values -> values |> List.map child |> combine
        | Lir.Record fields -> fields |> List.map (snd >> child) |> combine
        | Lir.FieldGet(target, _) | Lir.SomeValue target | Lir.RecordConvert(target, _) -> child target
        | Lir.Cons(head, tail) | Lir.Append(head, tail) | Lir.Range(head, tail) | Lir.IndexGet(head, tail) -> combine [ child head; child tail ]
        | Lir.Let(_, _, _, binding, body) -> combine [ child binding; child body ]
        | Lir.LetRecursiveGroup(bindings, body) -> combine (child body :: (bindings |> List.map (fun (_, _, _, value) -> child value)))
        | Lir.DecisionTree(target, cases) ->
            child target :: (cases |> List.collect (fun item -> [ yield! item.Guard |> Option.map child |> Option.toList; child item.Body ])) |> combine
        | Lir.RecordUpdate(target, fields) -> child target :: (fields |> List.map (snd >> child)) |> combine
        | Lir.Interpolate parts -> parts |> List.choose (function Lir.TextPart _ -> None | Lir.ValuePart value -> Some(child value)) |> combine
        | Lir.UnionCase(_, _, payload) -> payload |> Option.map child |> Option.defaultValue Pure
        | Lir.Map entries -> entries |> List.collect (function Lir.KeyValue(key, value) -> [ child key; child value ] | Lir.Spread value -> [ child value ]) |> combine
        | Lir.ForEach(_, _, _, source, body) -> combine [ child source; child body ]
