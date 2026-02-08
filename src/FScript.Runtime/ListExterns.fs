namespace FScript.Runtime

open FScript.Language

module ListExterns =
    let empty : ExternalFunction =
        { Name = "List.empty"
          Scheme = Forall([ 0 ], TList (TVar 0))
          Arity = 0
          Impl = fun _ -> function
              | [] -> VList []
              | _ -> raise (HostCommon.evalError "List.empty expects no arguments") }

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
        | VStringMap xf, VStringMap yf ->
            xf.Count = yf.Count
            && (Set.ofSeq xf.Keys = Set.ofSeq yf.Keys)
            && (xf |> Map.forall (fun k xv -> valueEquals xv yf.[k]))
        | VOption None, VOption None -> true
        | VOption (Some x), VOption (Some y) -> valueEquals x y
        | VUnionCase (tx, cx, px), VUnionCase (ty, cy, py) ->
            tx = ty
            && cx = cy
            &&
            match px, py with
            | None, None -> true
            | Some xv, Some yv -> valueEquals xv yv
            | _ -> false
        | VTypeToken tx, VTypeToken ty -> tx = ty
        | _ -> false

    let map : ExternalFunction =
        { Name = "List.map"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TVar 1), TFun(TList (TVar 0), TList (TVar 1))))
          Arity = 2
          Impl = fun ctx -> function
              | [ mapper; VList items ] ->
                  items |> List.map (fun item -> ctx.Apply mapper item) |> VList
              | _ -> raise (HostCommon.evalError "List.map expects (function, list)") }

    let iter : ExternalFunction =
        { Name = "List.iter"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TUnit), TFun(TList (TVar 0), TUnit)))
          Arity = 2
          Impl = fun ctx -> function
              | [ iterator; VList items ] ->
                  for item in items do
                      ctx.Apply iterator item |> ignore
                  VUnit
              | _ -> raise (HostCommon.evalError "List.iter expects (function, list)") }

    let choose : ExternalFunction =
        { Name = "List.choose"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TOption (TVar 1)), TFun(TList (TVar 0), TList (TVar 1))))
          Arity = 2
          Impl = fun ctx -> function
              | [ chooser; VList items ] ->
                  let chosen =
                      items
                      |> List.fold (fun acc item ->
                          match ctx.Apply chooser item with
                          | VOption (Some value) -> value :: acc
                          | VOption None -> acc
                          | _ -> raise (HostCommon.evalError "List.choose chooser must return option")) []
                      |> List.rev
                  VList chosen
              | _ -> raise (HostCommon.evalError "List.choose expects (function, list)") }

    let collect : ExternalFunction =
        { Name = "List.collect"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TList (TVar 1)), TFun(TList (TVar 0), TList (TVar 1))))
          Arity = 2
          Impl = fun ctx -> function
              | [ collector; VList items ] ->
                  let collected =
                      items
                      |> List.fold (fun acc item ->
                          match ctx.Apply collector item with
                          | VList values -> acc @ values
                          | _ -> raise (HostCommon.evalError "List.collect collector must return list")) []
                  VList collected
              | _ -> raise (HostCommon.evalError "List.collect expects (function, list)") }

    let contains : ExternalFunction =
        { Name = "List.contains"
          Scheme = Forall([ 0 ], TFun(TVar 0, TFun(TList (TVar 0), TBool)))
          Arity = 2
          Impl = fun _ -> function
              | [ needle; VList items ] -> VBool (items |> List.exists (fun item -> valueEquals item needle))
              | _ -> raise (HostCommon.evalError "List.contains expects (value, list)") }

    let distinct : ExternalFunction =
        { Name = "List.distinct"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TList (TVar 0)))
          Arity = 1
          Impl = fun _ -> function
              | [ VList items ] ->
                  let distinctItems =
                      items
                      |> List.fold (fun acc item ->
                          if acc |> List.exists (fun existing -> valueEquals existing item) then acc
                          else acc @ [ item ]) []
                  VList distinctItems
              | _ -> raise (HostCommon.evalError "List.distinct expects (list)") }

    let exists : ExternalFunction =
        { Name = "List.exists"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TBool)))
          Arity = 2
          Impl = fun ctx -> function
              | [ predicate; VList items ] ->
                  let rec loop xs =
                      match xs with
                      | [] -> false
                      | x :: rest ->
                          match ctx.Apply predicate x with
                          | VBool true -> true
                          | VBool false -> loop rest
                          | _ -> raise (HostCommon.evalError "List.exists predicate must return bool")
                  VBool (loop items)
              | _ -> raise (HostCommon.evalError "List.exists expects (function, list)") }

    let fold : ExternalFunction =
        { Name = "List.fold"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TFun(TVar 1, TVar 0)), TFun(TVar 0, TFun(TList (TVar 1), TVar 0))))
          Arity = 3
          Impl = fun ctx -> function
              | [ folder; state; VList items ] ->
                  let finalState =
                      items
                      |> List.fold (fun acc item ->
                          let step = ctx.Apply folder acc
                          ctx.Apply step item) state
                  finalState
              | _ -> raise (HostCommon.evalError "List.fold expects (function, state, list)") }

    let rev : ExternalFunction =
        { Name = "List.rev"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TList (TVar 0)))
          Arity = 1
          Impl = fun _ -> function
              | [ VList items ] -> VList (List.rev items)
              | _ -> raise (HostCommon.evalError "List.rev expects (list)") }

    let length : ExternalFunction =
        { Name = "List.length"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TInt))
          Arity = 1
          Impl = fun _ -> function
              | [ VList items ] -> VInt (int64 items.Length)
              | _ -> raise (HostCommon.evalError "List.length expects (list)") }

    let tryFind : ExternalFunction =
        { Name = "List.tryFind"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = fun ctx -> function
              | [ predicate; VList items ] ->
                  let rec loop xs =
                      match xs with
                      | [] -> VOption None
                      | x :: rest ->
                          match ctx.Apply predicate x with
                          | VBool true -> VOption (Some x)
                          | VBool false -> loop rest
                          | _ -> raise (HostCommon.evalError "List.tryFind predicate must return bool")
                  loop items
              | _ -> raise (HostCommon.evalError "List.tryFind expects (function, list)") }

    let tryGet : ExternalFunction =
        { Name = "List.tryGet"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TOption TInt)))
          Arity = 2
          Impl = fun ctx -> function
              | [ predicate; VList items ] ->
                  let rec loop index xs =
                      match xs with
                      | [] -> VOption None
                      | x :: rest ->
                          match ctx.Apply predicate x with
                          | VBool true -> VOption (Some (VInt index))
                          | VBool false -> loop (index + 1L) rest
                          | _ -> raise (HostCommon.evalError "List.tryGet predicate must return bool")
                  loop 0L items
              | _ -> raise (HostCommon.evalError "List.tryGet expects (function, list)") }

    let filter : ExternalFunction =
        { Name = "List.filter"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TList (TVar 0))))
          Arity = 2
          Impl = fun ctx -> function
              | [ predicate; VList items ] ->
                  let filtered =
                      items
                      |> List.filter (fun item ->
                          match ctx.Apply predicate item with
                          | VBool b -> b
                          | _ -> raise (HostCommon.evalError "List.filter predicate must return bool"))
                  VList filtered
              | _ -> raise (HostCommon.evalError "List.filter expects (function, list)") }

    let tryHead : ExternalFunction =
        { Name = "List.tryHead"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TOption (TVar 0)))
          Arity = 1
          Impl = fun _ -> function
              | [ VList (h :: _) ] -> HostCommon.some h
              | [ VList [] ] -> HostCommon.none
              | _ -> raise (HostCommon.evalError "List.tryHead expects (list)") }

    let tail : ExternalFunction =
        { Name = "List.tail"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TList (TVar 0)))
          Arity = 1
          Impl = fun _ -> function
              | [ VList (_ :: t) ] -> VList t
              | [ VList [] ] -> raise (HostCommon.evalError "List.tail expects a non-empty list")
              | _ -> raise (HostCommon.evalError "List.tail expects (list)") }

    let append : ExternalFunction =
        { Name = "List.append"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TFun(TList (TVar 0), TList (TVar 0))))
          Arity = 2
          Impl = fun _ -> function
              | [ VList left; VList right ] -> VList (left @ right)
              | _ -> raise (HostCommon.evalError "List.append expects (list, list)") }
