namespace FScript.Runtime

open FScript.Language

module MapExterns =
    let empty : ExternalFunction =
        { Name = "Map.empty"
          Scheme = Forall([ 0 ], TFun(TVar 0, TStringMap (TVar 0)))
          Arity = 1
          Impl = function
              | [ _ ] -> VStringMap Map.empty
              | _ -> raise (HostCommon.evalError "Map.empty expects one argument") }

    let add : ExternalFunction =
        { Name = "Map.add"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TVar 0, TFun(TStringMap (TVar 0), TStringMap (TVar 0)))))
          Arity = 3
          Impl = function
              | [ VString key; value; VStringMap m ] -> VStringMap (m.Add(key, value))
              | _ -> raise (HostCommon.evalError "Map.add expects (string, value, map)") }

    let ofList : ExternalFunction =
        { Name = "Map.ofList"
          Scheme = Forall([ 0 ], TFun(TList (TTuple [ TString; TVar 0 ]), TStringMap (TVar 0)))
          Arity = 1
          Impl = function
              | [ VList items ] ->
                  let folder (state: Map<string, Value>) (item: Value) =
                      match item with
                      | VTuple [ VString key; value ] -> state.Add(key, value)
                      | _ -> raise (HostCommon.evalError "Map.ofList expects a list of (string * value) tuples")
                  VStringMap (List.fold folder Map.empty items)
              | _ -> raise (HostCommon.evalError "Map.ofList expects ((string * value) list)") }

    let tryFind : ExternalFunction =
        { Name = "Map.tryFind"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> m.TryFind(key) |> VOption
              | _ -> raise (HostCommon.evalError "Map.tryFind expects (string, map)") }

    let tryGet : ExternalFunction =
        { Name = "Map.try"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> m.TryFind(key) |> VOption
              | _ -> raise (HostCommon.evalError "Map.try expects (string, map)") }

    let containsKey : ExternalFunction =
        { Name = "Map.containsKey"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TBool)))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> VBool (m.ContainsKey key)
              | _ -> raise (HostCommon.evalError "Map.containsKey expects (string, map)") }

    let remove : ExternalFunction =
        { Name = "Map.remove"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TStringMap (TVar 0))))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> VStringMap (m.Remove key)
              | _ -> raise (HostCommon.evalError "Map.remove expects (string, map)") }
