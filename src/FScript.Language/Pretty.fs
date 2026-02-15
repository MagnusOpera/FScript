namespace FScript.Language

module Pretty =
    open Eval

    let private closureParameters (firstArg: string) (body: Expr) =
        let rec loop (acc: string list) (expr: Expr) =
            match expr with
            | ELambda (param, next, _) -> loop (acc @ [ param.Name ]) next
            | _ -> acc
        loop [ firstArg ] body

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
        | VMap fields ->
            let mapKeyToString key =
                match key with
                | MKString s -> $"\"{s}\""
                | MKInt i -> string i
            fields
            |> Map.toList
            |> List.map (fun (key, value) -> sprintf "%s => %s" (mapKeyToString key) (valueToString value))
            |> String.concat "; "
            |> sprintf "map { %s }"
        | VOption None -> "None"
        | VOption (Some v) -> sprintf "Some %s" (valueToString v)
        | VUnionCase (_, caseName, None) -> caseName
        | VUnionCase (_, caseName, Some v) -> sprintf "%s %s" caseName (valueToString v)
        | VTypeToken t -> sprintf "<type %s>" (Types.typeToString t)
        | VClosure (argName, body, _) ->
            let args = closureParameters argName body
            sprintf "<fun %s>" (String.concat " " args)
        | VUnionCtor (_, caseName) -> sprintf "<ctor %s>" caseName
        | VExternal (ext, args) ->
            if args.IsEmpty then
                sprintf "<extern %s>" ext.Name
            else
                sprintf "<extern %s (%d/%d)>" ext.Name args.Length ext.Arity
