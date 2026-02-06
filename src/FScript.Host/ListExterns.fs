namespace FScript.Host

open FScript.Core

module ListExterns =
    let map : ExternalFunction =
        { Name = "List.map"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TVar 1), TFun(TList (TVar 0), TList (TVar 1))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.map is handled by the evaluator runtime") }

    let iter : ExternalFunction =
        { Name = "List.iter"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TUnit), TFun(TList (TVar 0), TUnit)))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.iter is handled by the evaluator runtime") }

    let tryHead : ExternalFunction =
        { Name = "List.tryHead"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TOption (TVar 0)))
          Arity = 1
          Impl = function
              | [ VList (h :: _) ] -> HostCommon.some h
              | [ VList [] ] -> HostCommon.none
              | _ -> raise (HostCommon.evalError "List.tryHead expects (list)") }

    let tail : ExternalFunction =
        { Name = "List.tail"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TList (TVar 0)))
          Arity = 1
          Impl = function
              | [ VList (_ :: t) ] -> VList t
              | [ VList [] ] -> raise (HostCommon.evalError "List.tail expects a non-empty list")
              | _ -> raise (HostCommon.evalError "List.tail expects (list)") }

    let append : ExternalFunction =
        { Name = "List.append"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TFun(TList (TVar 0), TList (TVar 0))))
          Arity = 2
          Impl = function
              | [ VList left; VList right ] -> VList (left @ right)
              | _ -> raise (HostCommon.evalError "List.append expects (list, list)") }
