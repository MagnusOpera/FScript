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

    let tryGet : ExternalFunction =
        { Name = "Map.tryGet"
          Scheme = Forall([ 0 ], TFun(TString, TFun(TStringMap (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = function
              | [ VString key; VStringMap m ] -> m.TryFind(key) |> VOption
              | _ -> raise (HostCommon.evalError "Map.tryGet expects (string, map)") }

    let count : ExternalFunction =
        { Name = "Map.count"
          Scheme = Forall([ 0 ], TFun(TStringMap (TVar 0), TInt))
          Arity = 1
          Impl = function
              | [ VStringMap m ] -> VInt (int64 m.Count)
              | _ -> raise (HostCommon.evalError "Map.count expects (map)") }

    let filter : ExternalFunction =
        { Name = "Map.filter"
          Scheme = Forall([ 0 ], TFun(TFun(TString, TFun(TVar 0, TBool)), TFun(TStringMap (TVar 0), TStringMap (TVar 0))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "Map.filter is handled by the evaluator runtime") }

    let fold : ExternalFunction =
        { Name = "Map.fold"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TFun(TString, TFun(TVar 1, TVar 0))), TFun(TVar 0, TFun(TStringMap (TVar 1), TVar 0))))
          Arity = 3
          Impl = fun _ -> raise (HostCommon.evalError "Map.fold is handled by the evaluator runtime") }

    let choose : ExternalFunction =
        { Name = "Map.choose"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TString, TFun(TVar 0, TOption (TVar 1))), TFun(TStringMap (TVar 0), TStringMap (TVar 1))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "Map.choose is handled by the evaluator runtime") }

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
