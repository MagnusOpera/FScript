namespace FScript.Core

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
        | VOption None -> "None"
        | VOption (Some v) -> sprintf "Some %s" (valueToString v)
        | VClosure _ -> "<fun>"
        | VExternal _ -> "<extern>"
