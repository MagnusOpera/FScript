namespace FScript.Language

open Execution

/// Callback-based helpers use the evaluator trampoline, including sorting and grouping.
module internal SupportCollections =
    let private keyValue = function
        | MKString x -> VString x | MKInt x -> VInt x | MKBool x -> VBool x
        | MKUnionCase(t,c) -> VUnionCase(t,c,None)
    let private mapKey = function
        | VString x -> MKString x | VInt x -> MKInt x | VBool x -> MKBool x
        | VUnionCase(t,c,None) -> MKUnionCase(t,c)
        | _ -> invalidArg "key" "Unsupported map key."
    let private boolValue = function VBool x -> x | _ -> invalidArg "predicate" "Expected bool."
    let private count value =
        if value < 0L || value > int64 System.Int32.MaxValue then invalidArg "count" "Count must be between 0 and 2147483647."
        int value
    let private compareKey left right =
        match left,right with
        | VInt a,VInt b -> compare a b
        | VFloat a,VFloat b -> compare a b
        | VString a,VString b -> System.Math.Sign(System.String.CompareOrdinal(a,b))
        | VBool a,VBool b -> compare a b
        | VUnit,VUnit -> 0
        | _ -> invalidArg "projection" "List.sortBy requires scalar keys; use List.sortWith for composite keys."

    let invoke equal apply name arguments = execution {
        let apply2 callback a b = execution { let! next = apply callback a in return! apply next b }
        let apply3 callback a b c = execution { let! next = apply2 callback a b in return! apply next c }
        let predicate callback value = execution { let! result = apply callback value in return boolValue result }
        let rec every callback wanted = function
            | [] -> Execution.result wanted
            | head::tail -> execution {
                let! result = predicate callback head
                if result <> wanted then return result else return! every callback wanted tail
              }
        let rec span callback acc = function
            | [] -> Execution.result (List.rev acc,[])
            | head::tail as remaining -> execution {
                let! keep = predicate callback head
                if keep then return! span callback (head::acc) tail else return List.rev acc,remaining
              }
        // Bottom-up stable merge sort avoids recursion proportional to the input length.
        let sort comparer values =
            let rec merge left right acc = execution {
                match left,right with
                | [],rest | rest,[] -> return List.rev acc @ rest
                | a::xs,b::ys ->
                    let! order = comparer a b
                    if order <= 0 then return! merge xs right (a::acc)
                    else return! merge left ys (b::acc)
            }
            let rec passes runs = execution {
                match runs with
                | [] -> return []
                | [run] -> return run
                | _ ->
                    let rec pairs acc = function
                        | a::b::tail -> execution { let! merged = merge a b [] in return! pairs (merged::acc) tail }
                        | remaining -> Execution.result (List.rev acc @ remaining)
                    let! merged = pairs [] runs
                    return! passes merged
            }
            passes (List.map List.singleton values)
        match name,arguments with
        | "List.isEmpty",[VList values] -> return VBool values.IsEmpty
        | "List.forall",[callback;VList values] -> let! result = every callback true values in return VBool result
        | "List.mapi",[callback;VList values] ->
            let! result = values |> List.mapi (fun i value -> i,value) |> Execution.map (fun (i,value) -> apply2 callback (VInt(int64 i)) value)
            return VList result
        | "List.indexed",[VList values] -> return VList(List.mapi (fun i value -> VTuple[VInt(int64 i);value]) values)
        | "List.init",[VInt n;callback] ->
            let! result = [0 .. count n - 1] |> Execution.map (fun i -> apply callback (VInt(int64 i)))
            return VList result
        | "List.take",[VInt n;VList values] -> 
            let n=count n
            if n>values.Length then invalidArg "count" "Count exceeds list length."
            return VList(List.take n values)
        | "List.skip",[VInt n;VList values] -> 
            let n=count n
            if n>values.Length then invalidArg "count" "Count exceeds list length."
            return VList(List.skip n values)
        | "List.takeWhile",[callback;VList values] -> let! (left,_) = span callback [] values in return VList left
        | "List.skipWhile",[callback;VList values] -> let! (_,right) = span callback [] values in return VList right
        | "List.partition",[callback;VList values] ->
            let! (yes,no) =
                values |> Execution.fold (fun (yes,no) value -> execution {
                    let! keep = predicate callback value
                    return if keep then value::yes,no else yes,value::no
                }) ([],[])
            return VTuple[VList(List.rev yes);VList(List.rev no)]
        | "List.zip",[VList left;VList right] -> return VList(List.zip left right |> List.map (fun (a,b) -> VTuple[a;b]))
        | "List.unzip",[VList values] ->
            let left,right = values |> List.map (function VTuple[a;b] -> a,b | _ -> invalidArg "pairs" "Expected pairs.") |> List.unzip
            return VTuple[VList left;VList right]
        | "List.chunkBySize",[VInt n;VList values] -> return VList(List.chunkBySize (count n) values |> List.map VList)
        | "List.scan",[callback;initial;VList values] ->
            let! (_,states) =
                values |> Execution.fold (fun (state,states) value -> execution {
                    let! next = apply2 callback state value
                    return next,next::states
                }) (initial,[initial])
            return VList(List.rev states)
        | "List.sortWith",[callback;VList values] ->
            let comparer a b = execution {
                let! result = apply2 callback a b
                return match result with VInt n -> compare n 0L | _ -> invalidArg "comparer" "Expected int."
            }
            let! result = sort comparer values
            return VList result
        | "List.sortBy",[callback;VList values] ->
            let! decorated = values |> Execution.map (fun value -> execution {
                let! key = apply callback value
                compareKey key key |> ignore
                return VTuple[key;value]
            })
            let comparer left right =
                match left,right with VTuple[a;_],VTuple[b;_] -> Execution.result(compareKey a b) | _ -> invalidOp "Expected decorated value."
            let! result = sort comparer decorated
            return VList(result |> List.map (function VTuple[_;value] -> value | _ -> invalidOp "Expected decorated value."))
        | ("List.distinctBy" | "List.groupBy" | "List.countBy"),[callback;VList values] ->
            let! groups =
                values |> Execution.fold (fun groups value -> execution {
                    let! key = apply callback value
                    let rec insert = function
                        | [] -> [key,[value]]
                        | (existing,items)::tail when equal key existing -> (existing,value::items)::tail
                        | head::tail -> head::insert tail
                    return insert groups
                }) []
            return VList(groups |> List.map (fun (key,items) ->
                if name="List.countBy" then VTuple[key;VInt(int64 items.Length)]
                elif name="List.groupBy" then VTuple[key;VList(List.rev items)]
                else List.last items))
        | "Map.keys",[VMap values] -> return VList(values |> Map.toList |> List.map (fst >> keyValue))
        | "Map.values",[VMap values] -> return VList(values |> Map.toList |> List.map snd)
        | "Map.toList",[VMap values] -> return VList(values |> Map.toList |> List.map (fun (key,value) -> VTuple[keyValue key;value]))
        | ("Map.exists" | "Map.forall"),[callback;VMap values] ->
            let wanted = name="Map.forall"
            let rec loop = function
                | [] -> Execution.result wanted
                | (key,value)::tail -> execution {
                    let! result = apply2 callback (keyValue key) value
                    if boolValue result <> wanted then return boolValue result else return! loop tail
                }
            let! result = loop (Map.toList values)
            return VBool result
        | "Map.update",[key;callback;VMap values] ->
            let key = mapKey key
            let! next = apply callback (VOption(Map.tryFind key values))
            return match next with
                   | VOption None -> VMap(Map.remove key values)
                   | VOption(Some value) -> VMap(Map.add key value values)
                   | _ -> invalidArg "updater" "Expected option."
        | "Map.mergeWith",[callback;VMap left;VMap right] ->
            let! result =
                Map.toList right |> Execution.fold (fun state (key,value) -> execution {
                    match Map.tryFind key state with
                    | None -> return Map.add key value state
                    | Some previous -> let! merged = apply3 callback (keyValue key) previous value in return Map.add key merged state
                }) left
            return VMap result
        | "Option.bind",[callback;VOption(Some value)] -> return! apply callback value
        | "Option.bind",[_;VOption None] -> return VOption None
        | "Option.flatten",[VOption(Some value)] -> return value
        | "Option.flatten",[VOption None] -> return VOption None
        | "Option.filter",[callback;VOption(Some value)] -> let! keep = predicate callback value in return VOption(if keep then Some value else None)
        | "Option.filter",[_;VOption None] -> return VOption None
        | "Option.orElse",[fallback;VOption None] -> return fallback
        | "Option.orElseWith",[callback;VOption None] -> return! apply callback VUnit
        | ("Option.orElse" | "Option.orElseWith"),[_;value] -> return value
        | "Option.map2",[callback;VOption(Some left);VOption(Some right)] -> let! result = apply2 callback left right in return VOption(Some result)
        | "Option.map2",[_;_;_] -> return VOption None
        | "Option.toList",[VOption value] -> return VList(Option.toList value)
        | _ -> return SupportLibrary.invokeScalar name arguments
    }
