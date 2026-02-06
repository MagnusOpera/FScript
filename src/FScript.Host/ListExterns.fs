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

    let choose : ExternalFunction =
        { Name = "List.choose"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TOption (TVar 1)), TFun(TList (TVar 0), TList (TVar 1))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.choose is handled by the evaluator runtime") }

    let collect : ExternalFunction =
        { Name = "List.collect"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TList (TVar 1)), TFun(TList (TVar 0), TList (TVar 1))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.collect is handled by the evaluator runtime") }

    let contains : ExternalFunction =
        { Name = "List.contains"
          Scheme = Forall([ 0 ], TFun(TVar 0, TFun(TList (TVar 0), TBool)))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.contains is handled by the evaluator runtime") }

    let distinct : ExternalFunction =
        { Name = "List.distinct"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TList (TVar 0)))
          Arity = 1
          Impl = fun _ -> raise (HostCommon.evalError "List.distinct is handled by the evaluator runtime") }

    let exists : ExternalFunction =
        { Name = "List.exists"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TBool)))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.exists is handled by the evaluator runtime") }

    let fold : ExternalFunction =
        { Name = "List.fold"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TFun(TVar 1, TVar 0)), TFun(TVar 0, TFun(TList (TVar 1), TVar 0))))
          Arity = 3
          Impl = fun _ -> raise (HostCommon.evalError "List.fold is handled by the evaluator runtime") }

    let rev : ExternalFunction =
        { Name = "List.rev"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TList (TVar 0)))
          Arity = 1
          Impl = function
              | [ VList items ] -> VList (List.rev items)
              | _ -> raise (HostCommon.evalError "List.rev expects (list)") }

    let length : ExternalFunction =
        { Name = "List.length"
          Scheme = Forall([ 0 ], TFun(TList (TVar 0), TInt))
          Arity = 1
          Impl = function
              | [ VList items ] -> VInt (int64 items.Length)
              | _ -> raise (HostCommon.evalError "List.length expects (list)") }

    let tryFind : ExternalFunction =
        { Name = "List.tryFind"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TOption (TVar 0))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.tryFind is handled by the evaluator runtime") }

    let tryFindIndex : ExternalFunction =
        { Name = "List.tryFindIndex"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TOption TInt)))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.tryFindIndex is handled by the evaluator runtime") }

    let filter : ExternalFunction =
        { Name = "List.filter"
          Scheme = Forall([ 0 ], TFun(TFun(TVar 0, TBool), TFun(TList (TVar 0), TList (TVar 0))))
          Arity = 2
          Impl = fun _ -> raise (HostCommon.evalError "List.filter is handled by the evaluator runtime") }

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
