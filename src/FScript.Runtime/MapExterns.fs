namespace FScript.Runtime

open FScript.Language

module MapExterns =
    let private asStringMap value =
        match value with
        | VStringMap m -> m
        | VRecord m -> m
        | _ -> raise (HostCommon.evalError "Map function expects a map value")

    let empty : ExternalFunction =
        { Name = "Map.empty"
          Scheme = Forall([ 0 ], TStringMap (TVar 0))
          Arity = 0
          Impl = fun _ -> function
              | [] -> VStringMap Map.empty
              | _ -> raise (HostCommon.evalError "Map.empty expects no arguments") }

    let add : ExternalFunction =
        { Name = "Map.add"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TVar 0, TFun(TStringMap (TVar 0), TStringMap (TVar 0)))))
          Arity = 3
          Impl = fun _ -> function
              | [ VString key; value; mapValue ] ->
                  let m = asStringMap mapValue
                  VStringMap (m.Add(key, value))
              | _ -> raise (HostCommon.evalError "Map.add expects (string, value, map)") }

    let ofList : ExternalFunction =
        { Name = "Map.ofList"
          Scheme = Forall([ 0 ], TFun(TList (TTuple [ TString; TVar 0 ]), TStringMap (TVar 0)))
          Arity = 1
          Impl = fun _ -> function
              | [ VList items ] ->
                  let folder (state: Map<string, Value>) (item: Value) =
                      match item with
                      | VTuple [ VString key; value ] -> state.Add(key, value)
                      | _ -> raise (HostCommon.evalError "Map.ofList expects a list of (string * value) tuples")
                  VStringMap (List.fold folder Map.empty items)
              | _ -> raise (HostCommon.evalError "Map.ofList expects ((string * value) list)") }

    let tryGet : ExternalFunction =
        { Name = "Map.tryGet"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = fun _ -> function
              | [ VString key; mapValue ] ->
                  let m = asStringMap mapValue
                  m.TryFind(key) |> VOption
              | _ -> raise (HostCommon.evalError "Map.tryGet expects (string, map)") }

    let count : ExternalFunction =
        { Name = "Map.count"
          Scheme = Forall([ 0 ], TFun(TStringMap (TVar 0), TInt))
          Arity = 1
          Impl = fun _ -> function
              | [ mapValue ] ->
                  let m = asStringMap mapValue
                  VInt (int64 m.Count)
              | _ -> raise (HostCommon.evalError "Map.count expects (map)") }

    let filter : ExternalFunction =
        { Name = "Map.filter"
          Scheme = Forall([ 0 ], TFun(TFun(TString, TFun(TVar 0, TBool)), TFun(TStringMap (TVar 0), TStringMap (TVar 0))))
          Arity = 2
          Impl = fun ctx -> function
              | [ predicate; mapValue ] ->
                  let m = asStringMap mapValue
                  let filtered =
                      m
                      |> Map.toList
                      |> List.choose (fun (key, value) ->
                          let predicateWithKey = ctx.Apply predicate (VString key)
                          match ctx.Apply predicateWithKey value with
                          | VBool true -> Some (key, value)
                          | VBool false -> None
                          | _ -> raise (HostCommon.evalError "Map.filter predicate must return bool"))
                      |> Map.ofList
                  VStringMap filtered
              | _ -> raise (HostCommon.evalError "Map.filter expects (function, map)") }

    let fold : ExternalFunction =
        { Name = "Map.fold"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TFun(TString, TFun(TVar 1, TVar 0))), TFun(TVar 0, TFun(TStringMap (TVar 1), TVar 0))))
          Arity = 3
          Impl = fun ctx -> function
              | [ folder; state; mapValue ] ->
                  let m = asStringMap mapValue
                  m
                  |> Map.fold (fun acc key value ->
                      let step = ctx.Apply folder acc
                      let stepWithKey = ctx.Apply step (VString key)
                      ctx.Apply stepWithKey value) state
              | _ -> raise (HostCommon.evalError "Map.fold expects (function, state, map)") }

    let choose : ExternalFunction =
        { Name = "Map.choose"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TString, TFun(TVar 0, TOption (TVar 1))), TFun(TStringMap (TVar 0), TStringMap (TVar 1))))
          Arity = 2
          Impl = fun ctx -> function
              | [ chooser; mapValue ] ->
                  let m = asStringMap mapValue
                  let chosen =
                      m
                      |> Map.fold (fun acc key value ->
                          let chooserWithKey = ctx.Apply chooser (VString key)
                          match ctx.Apply chooserWithKey value with
                          | VOption (Some chosenValue) -> Map.add key chosenValue acc
                          | VOption None -> acc
                          | _ -> raise (HostCommon.evalError "Map.choose chooser must return option")) Map.empty
                  VStringMap chosen
              | _ -> raise (HostCommon.evalError "Map.choose expects (function, map)") }

    let containsKey : ExternalFunction =
        { Name = "Map.containsKey"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TBool)))
          Arity = 2
          Impl = fun _ -> function
              | [ VString key; mapValue ] ->
                  let m = asStringMap mapValue
                  VBool (m.ContainsKey key)
              | _ -> raise (HostCommon.evalError "Map.containsKey expects (string, map)") }

    let remove : ExternalFunction =
        { Name = "Map.remove"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TStringMap (TVar 0))))
          Arity = 2
          Impl = fun _ -> function
              | [ VString key; mapValue ] ->
                  let m = asStringMap mapValue
                  VStringMap (m.Remove key)
              | _ -> raise (HostCommon.evalError "Map.remove expects (string, map)") }
