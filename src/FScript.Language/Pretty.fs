namespace FScript.Language

module Pretty =
    open Eval

    let rec valueToString v =
        match v with
        | VUnit -> "()"
        | VInt i -> string i
        | VFloat f -> string f
        | VBool b -> if b then "true" else "false"
        | VString s -> sprintf "\"%s\"" s
        | VList xs ->
            xs |> List.map valueToString |> String.concat ";" |> sprintf "[%s]"
        | VTuple xs ->
            xs |> List.map valueToString |> String.concat ", " |> sprintf "(%s)"
        | VRecord fields ->
            fields
            |> Map.toList
            |> List.map (fun (name, value) -> sprintf "%s = %s" name (valueToString value))
            |> String.concat "; "
            |> sprintf "{ %s }"
        | VStringMap fields ->
            fields
            |> Map.toList
            |> List.map (fun (name, value) -> sprintf "\"%s\" => %s" name (valueToString value))
            |> String.concat "; "
            |> sprintf "map { %s }"
        | VOption None -> "None"
        | VOption (Some v) -> sprintf "Some %s" (valueToString v)
        | VUnionCase (_, caseName, None) -> caseName
        | VUnionCase (_, caseName, Some v) -> sprintf "%s %s" caseName (valueToString v)
        | VTypeToken t -> sprintf "<type %s>" (Types.typeToString t)
        | VClosure _ -> "<fun>"
        | VUnionCtor (_, caseName) -> sprintf "<ctor %s>" caseName
        | VExternal _ -> "<extern>"
