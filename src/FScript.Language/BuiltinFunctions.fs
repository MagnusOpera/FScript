namespace FScript.Language

open System
open System.Globalization

module BuiltinFunctions =
    let private unknownSpan = Span.mk (Span.pos 0 0) (Span.pos 0 0)

    let private fail message =
        raise (EvalException { Message = message; Span = unknownSpan })

    let private scheme name =
        match BuiltinSignatures.builtinSchemes |> Map.tryFind name with
        | Some value -> value
        | None -> failwith $"Missing builtin signature for '{name}'"

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
        | VRecord xf, VRecord yf ->
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

    let private builtinPrint : ExternalFunction =
        { Name = "print"
          Scheme = scheme "print"
          Arity = 1
          Impl =
            (fun _ args ->
                let text = expectString "print" args
                Console.WriteLine(text)
                VUnit) }

    let private builtinIntTryParse : ExternalFunction =
        { Name = "Int.tryParse"
          Scheme = scheme "Int.tryParse"
          Arity = 1
          Impl =
            (fun _ args ->
                let text = expectString "Int.tryParse" args
                match Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                | true, value -> VOption (Some (VInt value))
                | _ -> VOption None) }

    let private builtinFloatTryParse : ExternalFunction =
        { Name = "Float.tryParse"
          Scheme = scheme "Float.tryParse"
          Arity = 1
          Impl =
            (fun _ args ->
                let text = expectString "Float.tryParse" args
                match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, value -> VOption (Some (VFloat value))
                | _ -> VOption None) }

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
        { Name = "List.map"
          Scheme = scheme "List.map"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ mapper; VList values ] ->
                    values |> List.map (fun value -> ctx.Apply mapper value) |> VList
                | _ -> fail "List.map expects (mapper, values)") }

    let private builtinListIter : ExternalFunction =
        { Name = "List.iter"
          Scheme = scheme "List.iter"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ iterator; VList values ] ->
                    values |> List.iter (fun value -> ctx.Apply iterator value |> ignore)
                    VUnit
                | _ -> fail "List.iter expects (iterator, values)") }

    let private builtinListChoose : ExternalFunction =
        { Name = "List.choose"
          Scheme = scheme "List.choose"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ chooser; VList values ] ->
                    values
                    |> List.choose (fun value ->
                        match ctx.Apply chooser value with
                        | VOption result -> result
                        | _ -> fail "List.choose chooser must return option")
                    |> VList
                | _ -> fail "List.choose expects (chooser, values)") }

    let private builtinListCollect : ExternalFunction =
        { Name = "List.collect"
          Scheme = scheme "List.collect"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ collector; VList values ] ->
                    values
                    |> List.collect (fun value ->
                        match ctx.Apply collector value with
                        | VList result -> result
                        | _ -> fail "List.collect collector must return list")
                    |> VList
                | _ -> fail "List.collect expects (collector, values)") }

    let private builtinListExists : ExternalFunction =
        { Name = "List.exists"
          Scheme = scheme "List.exists"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ predicate; VList values ] ->
                    values
                    |> List.exists (fun value ->
                        match ctx.Apply predicate value with
                        | VBool result -> result
                        | _ -> fail "List.exists predicate must return bool")
                    |> VBool
                | _ -> fail "List.exists expects (predicate, values)") }

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
        { Name = "List.fold"
          Scheme = scheme "List.fold"
          Arity = 3
          Impl =
            (fun ctx args ->
                match args with
                | [ folder; state; VList values ] ->
                    values
                    |> List.fold (fun acc value -> ctx.Apply (ctx.Apply folder acc) value) state
                | _ -> fail "List.fold expects (folder, state, values)") }

    let private builtinListFilter : ExternalFunction =
        { Name = "List.filter"
          Scheme = scheme "List.filter"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ predicate; VList values ] ->
                    values
                    |> List.filter (fun value ->
                        match ctx.Apply predicate value with
                        | VBool result -> result
                        | _ -> fail "List.filter predicate must return bool")
                    |> VList
                | _ -> fail "List.filter expects (predicate, values)") }

    let private builtinListLength : ExternalFunction =
        { Name = "List.length"
          Scheme = scheme "List.length"
          Arity = 1
          Impl = (fun _ args -> expectList "List.length" args |> List.length |> int64 |> VInt) }

    let private builtinListTryFind : ExternalFunction =
        { Name = "List.tryFind"
          Scheme = scheme "List.tryFind"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ predicate; VList values ] ->
                    values
                    |> List.tryFind (fun value ->
                        match ctx.Apply predicate value with
                        | VBool result -> result
                        | _ -> fail "List.tryFind predicate must return bool")
                    |> VOption
                | _ -> fail "List.tryFind expects (predicate, values)") }

    let private builtinListTryGet : ExternalFunction =
        { Name = "List.tryGet"
          Scheme = scheme "List.tryGet"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ predicate; VList values ] ->
                    values
                    |> List.tryFindIndex (fun value ->
                        match ctx.Apply predicate value with
                        | VBool result -> result
                        | _ -> fail "List.tryGet predicate must return bool")
                    |> Option.map (int64 >> VInt)
                    |> VOption
                | _ -> fail "List.tryGet expects (predicate, values)") }

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
        { Name = "Option.defaultWith"
          Scheme = scheme "Option.defaultWith"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ fallback; VOption (Some current) ] -> current
                | [ fallback; VOption None ] -> ctx.Apply fallback VUnit
                | _ -> fail "Option.defaultWith expects (fallback, value)") }

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
        { Name = "Option.map"
          Scheme = scheme "Option.map"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ mapper; VOption (Some current) ] -> ctx.Apply mapper current |> Some |> VOption
                | [ _; VOption None ] -> VOption None
                | _ -> fail "Option.map expects (mapper, value)") }

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
                | [ VString key; VMap values ] -> values.TryFind(MKString key) |> VOption
                | _ -> fail "Map.tryGet expects (key, values)") }

    let private builtinMapContainsKey : ExternalFunction =
        { Name = "Map.containsKey"
          Scheme = scheme "Map.containsKey"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString key; VMap values ] -> VBool (values.ContainsKey(MKString key))
                | _ -> fail "Map.containsKey expects (key, values)") }

    let private builtinMapAdd : ExternalFunction =
        { Name = "Map.add"
          Scheme = scheme "Map.add"
          Arity = 3
          Impl =
            (fun _ args ->
                match args with
                | [ VString key; value; VMap values ] -> values.Add(MKString key, value) |> VMap
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
                        | VTuple [ VString key; value ] -> state |> Map.add (MKString key) value
                        | _ -> fail "Map.ofList expects [(string * value)]"
                    pairs |> List.fold folder Map.empty |> VMap
                | _ -> fail "Map.ofList expects (pairs)") }

    let private builtinMapFold : ExternalFunction =
        { Name = "Map.fold"
          Scheme = scheme "Map.fold"
          Arity = 3
          Impl =
            (fun ctx args ->
                match args with
                | [ folder; state; VMap values ] ->
                    values
                    |> Map.fold (fun acc key value ->
                        ctx.Apply (ctx.Apply (ctx.Apply folder acc) (mapKeyToValue key)) value) state
                | _ -> fail "Map.fold expects (folder, state, values)") }

    let private builtinMapCount : ExternalFunction =
        { Name = "Map.count"
          Scheme = scheme "Map.count"
          Arity = 1
          Impl =
            (fun _ args ->
                let values = expectMap "Map.count" args
                values.Count |> int64 |> VInt) }

    let private builtinMapFilter : ExternalFunction =
        { Name = "Map.filter"
          Scheme = scheme "Map.filter"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ predicate; VMap values ] ->
                    values
                    |> Map.fold (fun acc key value ->
                        match ctx.Apply (ctx.Apply predicate (mapKeyToValue key)) value with
                        | VBool true -> acc |> Map.add key value
                        | VBool false -> acc
                        | _ -> fail "Map.filter predicate must return bool") Map.empty
                    |> VMap
                | _ -> fail "Map.filter expects (predicate, values)") }

    let private builtinMapChoose : ExternalFunction =
        { Name = "Map.choose"
          Scheme = scheme "Map.choose"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ chooser; VMap values ] ->
                    values
                    |> Map.fold (fun acc key value ->
                        match ctx.Apply (ctx.Apply chooser (mapKeyToValue key)) value with
                        | VOption (Some chosen) -> acc |> Map.add key chosen
                        | VOption None -> acc
                        | _ -> fail "Map.choose chooser must return option") Map.empty
                    |> VMap
                | _ -> fail "Map.choose expects (chooser, values)") }

    let private builtinMapMap : ExternalFunction =
        { Name = "Map.map"
          Scheme = scheme "Map.map"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ mapper; VMap values ] ->
                    values
                    |> Map.fold (fun acc key value -> acc |> Map.add key (ctx.Apply mapper value)) Map.empty
                    |> VMap
                | _ -> fail "Map.map expects (mapper, values)") }

    let private builtinMapIter : ExternalFunction =
        { Name = "Map.iter"
          Scheme = scheme "Map.iter"
          Arity = 2
          Impl =
            (fun ctx args ->
                match args with
                | [ iterator; VMap values ] ->
                    values
                    |> Map.iter (fun key value -> ctx.Apply (ctx.Apply iterator (mapKeyToValue key)) value |> ignore)
                    VUnit
                | _ -> fail "Map.iter expects (iterator, values)") }

    let private builtinMapRemove : ExternalFunction =
        { Name = "Map.remove"
          Scheme = scheme "Map.remove"
          Arity = 2
          Impl =
            (fun _ args ->
                match args with
                | [ VString key; VMap values ] -> values.Remove(MKString key) |> VMap
                | _ -> fail "Map.remove expects (key, values)") }

    let builtinExterns : ExternalFunction list =
        [ builtinIgnore
          builtinPrint
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
