namespace FScript.Host

open FScript.Core

module MapExterns =
    let empty : ExternalFunction =
        { Name = "map_empty"
          Scheme = Forall([ 0 ], TFun(TVar 0, TStringMap (TVar 0)))
          Arity = 1
          Impl = function
              | [ _ ] -> VStringMap Map.empty
              | _ -> raise (HostCommon.evalError "map_empty expects one argument") }

    let add : ExternalFunction =
        { Name = "map_add"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TVar 0, TFun(TStringMap (TVar 0), TStringMap (TVar 0)))))
          Arity = 3
          Impl = function
              | [ VString key; value; VStringMap m ] -> VStringMap (m.Add(key, value))
              | _ -> raise (HostCommon.evalError "map_add expects (string, value, map)") }

    let tryFind : ExternalFunction =
        { Name = "map_tryFind"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> m.TryFind(key) |> VOption
              | _ -> raise (HostCommon.evalError "map_tryFind expects (string, map)") }

    let containsKey : ExternalFunction =
        { Name = "map_containsKey"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TBool)))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> VBool (m.ContainsKey key)
              | _ -> raise (HostCommon.evalError "map_containsKey expects (string, map)") }

    let remove : ExternalFunction =
        { Name = "map_remove"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TStringMap (TVar 0))))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> VStringMap (m.Remove key)
              | _ -> raise (HostCommon.evalError "map_remove expects (string, map)") }
