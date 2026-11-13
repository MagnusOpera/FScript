namespace FScript.Language

module internal MatchCoverage =
    // Specialize the pattern matrix one constructor at a time (Maranget).
    type private Constructor =
        | Boolean of bool
        | EmptyOption
        | PresentOption
        | EmptyList
        | NonEmptyList
        | Tuple
        | Record of string list
        | EmptyMap
        | NonEmptyMap
        | UnionCase of string

    type private CoveragePattern =
        | Any
        | Constructed of Constructor * CoveragePattern list
        | Restricted

    let private expandType (typeDefs: Map<string, Type>) = function
        | TNamed name as typ -> Map.tryFind name typeDefs |> Option.defaultValue typ
        | typ -> typ

    let private recordConstructor (fields: Map<string, Type>) =
        let names, types = fields |> Map.toList |> List.unzip
        Record names, types

    let private constructors = function
        | TBool -> [ Boolean false, []; Boolean true, [] ]
        | TOption item -> [ EmptyOption, []; PresentOption, [ item ] ]
        | TList item as listType -> [ EmptyList, []; NonEmptyList, [ item; listType ] ]
        | TTuple items -> [ Tuple, items ]
        | TRecord fields
        | TRecordShape (_, fields, _)
        | THostManaged (_, fields) -> [ recordConstructor fields ]
        | TMap _ -> [ EmptyMap, []; NonEmptyMap, [] ]
        | TUnion (_, cases) ->
            [ for KeyValue(name, payload) in cases -> UnionCase name, Option.toList payload ]
        | _ -> []

    let check typeDefs targetType (cases: (Pattern * Expr option * Expr * Span) list) span =
        let rec normalize typ pattern =
            let typ = expandType typeDefs typ
            match pattern, typ with
            | (PWildcard _ | PVar _), _ -> Any
            | PLiteral (LBool value, _), _ -> Constructed(Boolean value, [])
            | PNone _, _ -> Constructed(EmptyOption, [])
            | PSome (item, _), TOption itemType ->
                Constructed(PresentOption, [ normalize itemType item ])
            | PNil _, _ -> Constructed(EmptyList, [])
            | PCons (head, tail, _), TList itemType ->
                Constructed(NonEmptyList, [ normalize itemType head; normalize typ tail ])
            | PTuple (items, _), TTuple itemTypes ->
                Constructed(Tuple, List.map2 normalize itemTypes items)
            | PRecord (fields, _), (TRecord types | TRecordShape (_, types, _) | THostManaged (_, types)) ->
                let patterns = Map.ofList fields
                let items = [ for KeyValue(name, fieldType) in types ->
                                match Map.tryFind name patterns with
                                | Some field -> normalize fieldType field
                                | None -> Any ]
                Constructed(fst (recordConstructor types), items)
            | PUnionCase (_, name, payload, _), TUnion (_, types) ->
                let items =
                    match types.TryFind name, payload with
                    | Some (Some payloadType), Some pattern -> [ normalize payloadType pattern ]
                    | _ -> []
                Constructed(UnionCase name, items)
            | PMap ([], None, _), _ -> Constructed(EmptyMap, [])
            | PMap ([ (key, value) ], tail, _), TMap (keyType, valueType)
                when normalize keyType key = Any
                     && normalize valueType value = Any
                     && (tail |> Option.forall (fun rest -> normalize typ rest = Any)) ->
                Constructed(NonEmptyMap, [])
            | _ -> Restricted

        let rec uniqueBindings (names: System.Collections.Generic.HashSet<string>) = function
            | PVar (name, _) -> names.Add name
            | PSome (item, _) | PUnionCase (_, _, Some item, _) -> uniqueBindings names item
            | PCons (head, tail, _) -> uniqueBindings names head && uniqueBindings names tail
            | PTuple (items, _) -> List.forall (uniqueBindings names) items
            | PRecord (fields, _) -> List.forall (snd >> uniqueBindings names) fields
            | PMap (entries, tail, _) ->
                entries |> List.forall (fun (key, value) -> uniqueBindings names key && uniqueBindings names value)
                && Option.forall (uniqueBindings names) tail
            | _ -> true

        let wildcards types = List.map (fun _ -> Any) types

        let rec uncovered types rows =
            match types, rows with
            | [], [] -> Some []
            | [], _ -> None
            | typ :: rest, _ ->
                let defaultRows = rows |> List.choose (function Any :: tail -> Some tail | _ -> None)
                let heads =
                    rows |> List.choose (function Constructed(head, _) :: _ -> Some head | _ -> None) |> Set.ofList
                let available = constructors (expandType typeDefs typ)
                let missing = available |> List.tryFind (fun (head, _) -> not (Set.contains head heads))
                match available, missing with
                | [], _ ->
                    uncovered rest defaultRows
                    |> Option.map (fun tail -> Any :: tail)
                | _, Some (head, arguments) ->
                    uncovered rest defaultRows
                    |> Option.map (fun tail -> Constructed(head, wildcards arguments) :: tail)
                | _, None ->
                    available
                    |> List.tryPick (fun (head, arguments) ->
                        let specialized =
                            rows |> List.choose (function
                                | Constructed(current, children) :: tail when current = head -> Some (children @ tail)
                                | Any :: tail -> Some (wildcards arguments @ tail)
                                | _ -> None)
                        uncovered (arguments @ rest) specialized
                        |> Option.map (fun witness ->
                            let children, tail = List.splitAt arguments.Length witness
                            Constructed(head, children) :: tail))

        let rec describe = function
            | Constructed(Boolean value, _) -> if value then "true" else "false"
            | Constructed(EmptyOption, _) -> "None"
            | Constructed(PresentOption, [ item ]) -> "Some " + describe item
            | Constructed(EmptyList, _) -> "[]"
            | Constructed(NonEmptyList, [ head; tail ]) -> $"({describe head} :: {describe tail})"
            | Constructed(Tuple, items) -> "(" + (List.map describe items |> String.concat ", ") + ")"
            | Constructed(Record names, items) ->
                let fields = List.map2 (fun name item -> $"{name} = {describe item}") names items
                "{ " + String.concat "; " fields + " }"
            | Constructed(EmptyMap, _) -> "{}"
            | Constructed(NonEmptyMap, _) -> "{ [_] = _ }"
            | Constructed(UnionCase name, []) -> name
            | Constructed(UnionCase name, [ item ]) -> name + " " + describe item
            | _ -> "_"

        if not (cases |> List.exists (function (PWildcard _ | PVar _), None, _, _ -> true | _ -> false)) then
            let rows =
                cases |> List.choose (fun (pattern, guard, _, _) ->
                    if Option.isNone guard && uniqueBindings (System.Collections.Generic.HashSet<string>()) pattern then
                        Some [ normalize targetType pattern ]
                    else None)
            match uncovered [ targetType ] rows with
            | Some [ Any ] ->
                raise (TypeException { Message = "Non-exhaustive match. Add an unguarded catch-all case."; Span = span })
            | Some [ example ] ->
                raise (TypeException { Message = $"Non-exhaustive match. Missing case: {describe example}."; Span = span })
            | _ -> ()
