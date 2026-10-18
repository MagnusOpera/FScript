namespace FScript.Language

open System
open System.Globalization

module BuiltinFunctions =
    open HostInterop
    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let private fail message =
        raise (EvalException { Message = message; Span = unknownSpan })

    let private scheme name =
        match BuiltinSignatures.builtinSchemes |> Map.tryFind name with
        | Some value -> value
        | None -> failwith $"Missing builtin signature for '{name}'"

    open Execution
    let private implementations = Collections.Generic.Dictionary<ExternalFunction, (Value -> Value -> Execution<Value>) -> Value list -> Execution<Value>>(HashIdentity.Reference)
    let private resumable name arity implementation =
        let descriptor =
            { Name = name; Scheme = scheme name; Arity = arity
              Impl = fun context arguments -> implementation (fun f a -> Execution.result(context.Apply f a)) arguments |> Execution.run }
        implementations.Add(descriptor, implementation)
        descriptor
    let internal tryExecution descriptor =
        match implementations.TryGetValue descriptor with
        | true, implementation -> Some implementation
        | false, _ -> None

    let private tryParseInt64 (text: string) =
#if FABLE_COMPILER
        match Int64.TryParse(text) with
        | true, value -> Some value
        | _ -> None
#else
        match Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, value -> Some value
        | _ -> None
#endif

    let private tryParseDouble (text: string) =
#if FABLE_COMPILER
        match Double.TryParse(text) with
        | true, value -> Some value
        | _ -> None
#else
        match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value -> Some value
        | _ -> None
#endif

    let private expectString functionName args =
        match args with
        | [ VString value ] -> value
        | _ -> fail $"{functionName} expects (string)"

    let private expectInt functionName args =
        match args with
        | [ VInt value ] -> value
        | _ -> fail $"{functionName} expects (int)"

    let private expectBool functionName args =
        match args with
        | [ VBool value ] -> value
        | _ -> fail $"{functionName} expects (bool)"

    let private expectMap functionName args =
        match args with
        | [ VMap value ] -> value
        | _ -> fail $"{functionName} expects (map)"

    let private expectList functionName args =
        match args with
        | [ VList value ] -> value
        | _ -> fail $"{functionName} expects (list)"

    let private asStringKey functionName value =
        match value with
        | VString key -> MKString key
        | _ -> fail $"{functionName} expects string key"

    let private asStringList functionName values =
        let rec loop remaining acc =
            match remaining with
            | [] -> List.rev acc
            | VString value :: tail -> loop tail (value :: acc)
            | _ -> fail $"{functionName} expects (string, string list)"
        loop values []

    let private mapKeyToValue key =
        match key with
        | MKString value -> VString value
        | MKInt value -> VInt value
        | MKBool value -> VBool value
        | MKUnionCase (typeName, caseName) -> VUnionCase(typeName, caseName, None)

    let private valueToMapKey functionName value =
        match value with
        | VString key -> MKString key
        | VInt key -> MKInt key
        | VBool key -> MKBool key
        | VUnionCase (typeName, caseName, None) -> MKUnionCase(typeName, caseName)
        | _ -> fail $"{functionName} expects a string, int, bool, or payload-free union key"

    let rec private valueEquals a b =
        match a, b with
        | VUnit, VUnit -> true
        | VInt x, VInt y -> x = y
        | VFloat x, VFloat y -> x = y
        | VBool x, VBool y -> x = y
        | VString x, VString y -> x = y
        | VList xs, VList ys ->
            xs.Length = ys.Length && List.forall2 valueEquals xs ys
        | VTuple xs, VTuple ys ->
            xs.Length = ys.Length && List.forall2 valueEquals xs ys
        | RecordFields xf, RecordFields yf ->
            xf.Count = yf.Count
            && (Set.ofSeq xf.Keys = Set.ofSeq yf.Keys)
            && (xf |> Map.forall (fun k xv -> valueEquals xv yf.[k]))
        | VMap xf, VMap yf ->
            xf.Count = yf.Count
            && (Set.ofSeq xf.Keys = Set.ofSeq yf.Keys)
            && (xf |> Map.forall (fun k xv -> valueEquals xv yf.[k]))
        | VOption None, VOption None -> true
        | VOption (Some x), VOption (Some y) -> valueEquals x y
        | VUnionCase (tx, cx, px), VUnionCase (ty, cy, py) ->
            tx = ty && cx = cy
            &&
            match px, py with
            | None, None -> true
            | Some xv, Some yv -> valueEquals xv yv
            | _ -> false
        | VTypeToken tx, VTypeToken ty -> tx = ty
        | _ -> false

    let private distinctValues (values: Value list) =
        let folder acc value =
            if acc |> List.exists (fun existing -> valueEquals existing value) then acc else value :: acc

        values |> List.fold folder [] |> List.rev

    let private builtinIgnore : ExternalFunction =
        { Name = "ignore"
          Scheme = scheme "ignore"
          Arity = 1
          Impl = (fun _ _ -> VUnit) }

    let private builtinIntTryParse : ExternalFunction =
        { Name = "Int.tryParse"
          Scheme = scheme "Int.tryParse"
          Arity = 1
          Impl =
            (fun _ args ->
                let text = expectString "Int.tryParse" args
                match tryParseInt64 text with
                | Some value -> VOption (Some (VInt value))
                | None -> VOption None) }

    let private builtinFloatTryParse : ExternalFunction =
        { Name = "Float.tryParse"
          Scheme = scheme "Float.tryParse"
          Arity = 1
          Impl =
            (fun _ args ->
                let text = expectString "Float.tryParse" args
                match tryParseDouble text with
                | Some value -> VOption (Some (VFloat value))
                | None -> VOption None) }

    let private builtinBoolTryParse : ExternalFunction =
        { Name = "Bool.tryParse"
          Scheme = scheme "Bool.tryParse"
          Arity = 1
          Impl =
            (fun _ args ->
                let text = expectString "Bool.tryParse" args
                match Boolean.TryParse(text) with
                | true, value -> VOption (Some (VBool value))
                | _ -> VOption None) }

    let private builtinIntToString : ExternalFunction =
        { Name = "Int.toString"
          Scheme = scheme "Int.toString"
          Arity = 1
          Impl = (fun _ args -> expectInt "Int.toString" args |> fun value -> VString (value.ToString(CultureInfo.InvariantCulture))) }

    let private builtinFloatToString : ExternalFunction =
        { Name = "Float.toString"
          Scheme = scheme "Float.toString"
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VFloat value ] -> VString (value.ToString("G17", CultureInfo.InvariantCulture))
                | _ -> fail "Float.toString expects (float)") }

    let private builtinBoolToString : ExternalFunction =
        { Name = "Bool.toString"
          Scheme = scheme "Bool.toString"
          Arity = 1
          Impl =
            (fun _ args ->
                let value = expectBool "Bool.toString" args
                VString (if value then "true" else "false")) }

    let private builtinStringReplace : ExternalFunction =
        { Name = "String.replace"
          Scheme = scheme "String.replace"
          Arity = 3
          Impl =
            (fun _ args ->
                match args with
                | [ VString oldValue; VString newValue; VString source ] ->
                    VString(source.Replace(oldValue, newValue))
                | _ -> fail "String.replace expects (string, string, string)") }

    let private builtinStringIndexOf : ExternalFunction =
        { Name = "String.indexOf"
          Scheme = scheme "String.indexOf"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString value; VString source ] ->
                    let index = source.IndexOf(value, StringComparison.Ordinal)
                    if index >= 0 then VOption (Some (VInt (int64 index))) else VOption None
                | _ -> fail "String.indexOf expects (string, string)") }

    let private builtinStringToLower : ExternalFunction =
        { Name = "String.toLower"
          Scheme = scheme "String.toLower"
          Arity = 1
          Impl = (fun _ args -> expectString "String.toLower" args |> fun source -> VString(source.ToLowerInvariant())) }

    let private builtinStringToUpper : ExternalFunction =
        { Name = "String.toUpper"
          Scheme = scheme "String.toUpper"
          Arity = 1
          Impl = (fun _ args -> expectString "String.toUpper" args |> fun source -> VString(source.ToUpperInvariant())) }

    let private builtinStringSubstring : ExternalFunction =
        { Name = "String.substring"
          Scheme = scheme "String.substring"
          Arity = 3
          Impl =
            (fun _ args ->
                match args with
                | [ VInt start; VInt length; VString source ] ->
                    let startIndex = int start
                    let sliceLength = int length
                    if startIndex < 0 || sliceLength < 0 || startIndex > source.Length || startIndex + sliceLength > source.Length then
                        VOption None
                    else
                        VOption (Some (VString (source.Substring(startIndex, sliceLength))))
                | _ -> fail "String.substring expects (string, int, int)") }

    let private builtinStringConcat : ExternalFunction =
        { Name = "String.concat"
          Scheme = scheme "String.concat"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString separator; VList values ] ->
                    let strings = asStringList "String.concat" values
                    VString(String.Join(separator, strings))
                | _ -> fail "String.concat expects (string, string list)") }

    let private builtinStringSplit : ExternalFunction =
        { Name = "String.split"
          Scheme = scheme "String.split"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString separator; VString source ] ->
                    source.Split([| separator |], StringSplitOptions.None)
                    |> Array.toList
                    |> List.map VString
                    |> VList
                | _ -> fail "String.split expects (string, string)") }

    let private builtinStringEndsWith : ExternalFunction =
        { Name = "String.endsWith"
          Scheme = scheme "String.endsWith"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString suffix; VString source ] -> VBool (source.EndsWith(suffix, StringComparison.Ordinal))
                | _ -> fail "String.endsWith expects (string, string)") }

    let private builtinListEmpty : ExternalFunction =
        { Name = "List.empty"
          Scheme = scheme "List.empty"
          Arity = 0
          Impl = (fun _ _ -> VList []) }

    let private builtinListMap : ExternalFunction =
        resumable "List.map" 2 (fun apply args -> execution {
            match args with
            | [ mapper; VList values ] ->
                let! results = Execution.map (apply mapper) values
                return VList results
            | _ -> return fail "List.map expects (mapper, values)"
        })

    let private builtinListIter : ExternalFunction =
        resumable "List.iter" 2 (fun apply args -> execution {
            match args with
            | [ iterator; VList values ] ->
                for value in values do
                    let! _ = apply iterator value
                    ()
                return VUnit
            | _ -> return fail "List.iter expects (iterator, values)"
        })

    let private builtinListChoose : ExternalFunction =
        resumable "List.choose" 2 (fun apply args -> execution {
            match args with
            | [ callback; VList values ] ->
                let! results =
                    Execution.map (fun value -> execution {
                        let! result = apply callback value
                        match result with
                        | VOption result -> return result
                        | _ -> return fail "List.choose chooser must return option"
                    }) values
                return VList(List.choose id results)
            | _ -> return fail "List.choose expects (callback, values)"
        })

    let private builtinListCollect : ExternalFunction =
        resumable "List.collect" 2 (fun apply args -> execution {
            match args with
            | [ callback; VList values ] ->
                let! results =
                    Execution.map (fun value -> execution {
                        let! result = apply callback value
                        match result with
                        | VList result -> return result
                        | _ -> return fail "List.collect collector must return list"
                    }) values
                return VList(List.concat results)
            | _ -> return fail "List.collect expects (callback, values)"
        })

    let private builtinListExists : ExternalFunction =
        resumable "List.exists" 2 (fun apply args -> execution {
            match args with
            | [ predicate; VList values ] ->
                let rec find index remaining = execution {
                    match remaining with
                    | [] -> return VBool false
                    | head :: tail ->
                        let! result = apply predicate head
                        match result with
                        | VBool true -> return VBool true
                        | VBool false -> return! find (index + 1L) tail
                        | _ -> return fail "List.exists predicate must return bool"
                }
                return! find 0L values
            | _ -> return fail "List.exists expects (predicate, values)"
        })

    let private builtinListContains : ExternalFunction =
        { Name = "List.contains"
          Scheme = scheme "List.contains"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ needle; VList values ] ->
                    values
                    |> List.exists (fun value ->
                        match value, needle with
                        | VUnit, VUnit -> true
                        | VInt x, VInt y -> x = y
                        | VFloat x, VFloat y -> x = y
                        | VBool x, VBool y -> x = y
                        | VString x, VString y -> x = y
                        | _ -> false)
                    |> VBool
                | _ -> fail "List.contains expects (needle, values)") }

    let private builtinListRev : ExternalFunction =
        { Name = "List.rev"
          Scheme = scheme "List.rev"
          Arity = 1
          Impl = (fun _ args -> expectList "List.rev" args |> List.rev |> VList) }

    let private builtinListDistinct : ExternalFunction =
        { Name = "List.distinct"
          Scheme = scheme "List.distinct"
          Arity = 1
          Impl = (fun _ args -> expectList "List.distinct" args |> distinctValues |> VList) }

    let private builtinListFold : ExternalFunction =
        resumable "List.fold" 3 (fun apply args -> execution {
            match args with
            | [ folder; state; VList values ] ->
                return! values |> Execution.fold (fun acc value -> execution {
                    let! next = apply folder acc
                    return! apply next value
                }) state
            | _ -> return fail "List.fold expects (folder, state, values)"
        })

    let private builtinListFilter : ExternalFunction =
        resumable "List.filter" 2 (fun apply args -> execution {
            match args with
            | [ predicate; VList values ] ->
                let! results =
                    values |> Execution.map (fun value -> execution {
                        let! result = apply predicate value
                        match result with
                        | VBool accepted -> return if accepted then Some value else None
                        | _ -> return fail "List.filter predicate must return bool"
                    })
                return VList(List.choose id results)
            | _ -> return fail "List.filter expects (predicate, values)"
        })

    let private builtinListLength : ExternalFunction =
        { Name = "List.length"
          Scheme = scheme "List.length"
          Arity = 1
          Impl = (fun _ args -> expectList "List.length" args |> List.length |> int64 |> VInt) }

    let private builtinListTryFind : ExternalFunction =
        resumable "List.tryFind" 2 (fun apply args -> execution {
            match args with
            | [ predicate; VList values ] ->
                let rec find index remaining = execution {
                    match remaining with
                    | [] -> return VOption None
                    | head :: tail ->
                        let! result = apply predicate head
                        match result with
                        | VBool true -> return VOption(Some head)
                        | VBool false -> return! find (index + 1L) tail
                        | _ -> return fail "List.tryFind predicate must return bool"
                }
                return! find 0L values
            | _ -> return fail "List.tryFind expects (predicate, values)"
        })

    let private builtinListTryGet : ExternalFunction =
        resumable "List.tryGet" 2 (fun apply args -> execution {
            match args with
            | [ predicate; VList values ] ->
                let rec find index remaining = execution {
                    match remaining with
                    | [] -> return VOption None
                    | head :: tail ->
                        let! result = apply predicate head
                        match result with
                        | VBool true -> return VOption(Some(VInt index))
                        | VBool false -> return! find (index + 1L) tail
                        | _ -> return fail "List.tryGet predicate must return bool"
                }
                return! find 0L values
            | _ -> return fail "List.tryGet expects (predicate, values)"
        })

    let private builtinListTryItem : ExternalFunction =
        { Name = "List.tryItem"
          Scheme = scheme "List.tryItem"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VInt index; VList values ] ->
                    let rec loop remaining current =
                        match remaining with
                        | [] -> None
                        | head :: tail ->
                            if current = index then Some head else loop tail (current + 1L)

                    if index < 0L then VOption None else loop values 0L |> VOption
                | _ -> fail "List.tryItem expects (index, values)") }

    let private builtinListTryHead : ExternalFunction =
        { Name = "List.tryHead"
          Scheme = scheme "List.tryHead"
          Arity = 1
          Impl = (fun _ args -> expectList "List.tryHead" args |> List.tryHead |> VOption) }

    let private builtinListTail : ExternalFunction =
        { Name = "List.tail"
          Scheme = scheme "List.tail"
          Arity = 1
          Impl =
            (fun _ args ->
                match expectList "List.tail" args with
                | [] -> fail "List.tail expects a non-empty list"
                | _ :: rest -> VList rest) }

    let private builtinListAppend : ExternalFunction =
        { Name = "List.append"
          Scheme = scheme "List.append"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VList left; VList right ] -> VList (left @ right)
                | _ -> fail "List.append expects (left, right)") }

    let private builtinOptionDefaultValue : ExternalFunction =
        { Name = "Option.defaultValue"
          Scheme = scheme "Option.defaultValue"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ fallback; VOption (Some current) ] -> current
                | [ fallback; VOption None ] -> fallback
                | _ -> fail "Option.defaultValue expects (fallback, value)") }

    let private builtinOptionDefaultWith : ExternalFunction =
        resumable "Option.defaultWith" 2 (fun apply args -> execution {
            match args with
            | [ _; VOption(Some current) ] -> return current
            | [ fallback; VOption None ] -> return! apply fallback VUnit
            | _ -> return fail "Option.defaultWith expects (fallback, value)"
        })

    let private builtinOptionIsNone : ExternalFunction =
        { Name = "Option.isNone"
          Scheme = scheme "Option.isNone"
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VOption None ] -> VBool true
                | [ VOption (Some _) ] -> VBool false
                | _ -> fail "Option.isNone expects (value)") }

    let private builtinOptionIsSome : ExternalFunction =
        { Name = "Option.isSome"
          Scheme = scheme "Option.isSome"
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VOption None ] -> VBool false
                | [ VOption (Some _) ] -> VBool true
                | _ -> fail "Option.isSome expects (value)") }

    let private builtinOptionMap : ExternalFunction =
        resumable "Option.map" 2 (fun apply args -> execution {
            match args with
            | [ mapper; VOption(Some current) ] ->
                let! result = apply mapper current
                return VOption(Some result)
            | [ _; VOption None ] -> return VOption None
            | _ -> return fail "Option.map expects (mapper, value)"
        })

    let private builtinMapEmpty : ExternalFunction =
        { Name = "Map.empty"
          Scheme = scheme "Map.empty"
          Arity = 0
          Impl = (fun _ _ -> VMap Map.empty) }

    let private builtinMapTryGet : ExternalFunction =
        { Name = "Map.tryGet"
          Scheme = scheme "Map.tryGet"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ key; VMap values ] -> values.TryFind(valueToMapKey "Map.tryGet" key) |> VOption
                | _ -> fail "Map.tryGet expects (key, values)") }

    let private builtinMapContainsKey : ExternalFunction =
        { Name = "Map.containsKey"
          Scheme = scheme "Map.containsKey"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ key; VMap values ] -> VBool (values.ContainsKey(valueToMapKey "Map.containsKey" key))
                | _ -> fail "Map.containsKey expects (key, values)") }

    let private builtinMapAdd : ExternalFunction =
        { Name = "Map.add"
          Scheme = scheme "Map.add"
          Arity = 3
          Impl =
            (fun _ args ->
                match args with
                | [ key; value; VMap values ] -> values.Add(valueToMapKey "Map.add" key, value) |> VMap
                | _ -> fail "Map.add expects (key, value, values)") }

    let private builtinMapOfList : ExternalFunction =
        { Name = "Map.ofList"
          Scheme = scheme "Map.ofList"
          Arity = 1
          Impl =
            (fun _ args ->
                match args with
                | [ VList pairs ] ->
                    let folder state entry =
                        match entry with
                        | VTuple [ key; value ] -> state |> Map.add (valueToMapKey "Map.ofList" key) value
                        | _ -> fail "Map.ofList expects [(key * value)]"
                    pairs |> List.fold folder Map.empty |> VMap
                | _ -> fail "Map.ofList expects (pairs)") }

    let private builtinMapFold : ExternalFunction =
        resumable "Map.fold" 3 (fun apply args -> execution {
            match args with
            | [ folder; state; VMap values ] ->
                return! Map.toList values |> Execution.fold (fun acc (key, value) -> execution {
                    let! first = apply folder acc
                    let! second = apply first (mapKeyToValue key)
                    return! apply second value
                }) state
            | _ -> return fail "Map.fold expects (folder, state, values)"
        })

    let private builtinMapCount : ExternalFunction =
        { Name = "Map.count"
          Scheme = scheme "Map.count"
          Arity = 1
          Impl =
            (fun _ args ->
                let values = expectMap "Map.count" args
                values.Count |> int64 |> VInt) }

    let private builtinMapFilter : ExternalFunction =
        resumable "Map.filter" 2 (fun apply args -> execution {
            match args with
            | [ callback; VMap values ] ->
                let! result =
                    Map.toList values |> Execution.fold (fun acc (key, value) -> execution {
                        let! next = apply callback (mapKeyToValue key)
                        let! result = apply next value
                        match result with
                        | VBool true -> return Map.add key value acc
                        | VBool false -> return acc
                        | _ -> return fail "Map.filter predicate must return bool"
                    }) Map.empty
                return VMap result
            | _ -> return fail "Map.filter expects (callback, values)"
        })

    let private builtinMapChoose : ExternalFunction =
        resumable "Map.choose" 2 (fun apply args -> execution {
            match args with
            | [ callback; VMap values ] ->
                let! result =
                    Map.toList values |> Execution.fold (fun acc (key, value) -> execution {
                        let! next = apply callback (mapKeyToValue key)
                        let! result = apply next value
                        match result with
                        | VOption(Some chosen) -> return Map.add key chosen acc
                        | VOption None -> return acc
                        | _ -> return fail "Map.choose chooser must return option"
                    }) Map.empty
                return VMap result
            | _ -> return fail "Map.choose expects (callback, values)"
        })

    let private builtinMapMap : ExternalFunction =
        resumable "Map.map" 2 (fun apply args -> execution {
            match args with
            | [ callback; VMap values ] ->
                let! result =
                    Map.toList values |> Execution.fold (fun acc (key, value) -> execution {
                        let! result = apply callback value
                        return Map.add key result acc
                    }) Map.empty
                return VMap result
            | _ -> return fail "Map.map expects (callback, values)"
        })

    let private builtinMapIter : ExternalFunction =
        resumable "Map.iter" 2 (fun apply args -> execution {
            match args with
            | [ callback; VMap values ] ->
                let! result =
                    Map.toList values |> Execution.fold (fun acc (key, value) -> execution {
                        let! next = apply callback (mapKeyToValue key)
                        let! result = apply next value
                        return acc
                    }) Map.empty
                return VUnit
            | _ -> return fail "Map.iter expects (callback, values)"
        })

    let private builtinMapRemove : ExternalFunction =
        { Name = "Map.remove"
          Scheme = scheme "Map.remove"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ key; VMap values ] -> values.Remove(valueToMapKey "Map.remove" key) |> VMap
                | _ -> fail "Map.remove expects (key, values)") }

    let builtinExterns : ExternalFunction list =
        [ builtinIgnore
          builtinIntTryParse
          builtinFloatTryParse
          builtinBoolTryParse
          builtinIntToString
          builtinFloatToString
          builtinBoolToString
          builtinStringReplace
          builtinStringIndexOf
          builtinStringToLower
          builtinStringToUpper
          builtinStringSubstring
          builtinStringConcat
          builtinStringSplit
          builtinStringEndsWith
          builtinListEmpty
          builtinListMap
          builtinListIter
          builtinListChoose
          builtinListCollect
          builtinListExists
          builtinListContains
          builtinListRev
          builtinListDistinct
          builtinListFold
          builtinListFilter
          builtinListLength
          builtinListTryFind
          builtinListTryGet
          builtinListTryItem
          builtinListTryHead
          builtinListTail
          builtinListAppend
          builtinOptionDefaultValue
          builtinOptionDefaultWith
          builtinOptionIsNone
          builtinOptionIsSome
          builtinOptionMap
          builtinMapEmpty
          builtinMapTryGet
          builtinMapContainsKey
          builtinMapAdd
          builtinMapOfList
          builtinMapFold
          builtinMapCount
          builtinMapFilter
          builtinMapChoose
          builtinMapMap
          builtinMapIter
          builtinMapRemove ]
