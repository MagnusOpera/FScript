namespace FScript.Runtime

open FScript.Language

module OptionExterns =
    let defaultValue : ExternalFunction =
        { Name = "Option.defaultValue"
          Scheme = Forall([ 0 ], TFun(TVar 0, TFun(TOption (TVar 0), TVar 0)))
          Arity = 2
          Impl = fun _ -> function
              | [ fallback; VOption (Some value) ] -> value
              | [ fallback; VOption None ] -> fallback
              | _ -> raise (HostCommon.evalError "Option.defaultValue expects (value, option)") }

    let defaultWith : ExternalFunction =
        { Name = "Option.defaultWith"
          Scheme = Forall([ 0 ], TFun(TFun(TUnit, TVar 0), TFun(TOption (TVar 0), TVar 0)))
          Arity = 2
          Impl = fun ctx -> function
              | [ _; VOption (Some value) ] -> value
              | [ fallback; VOption None ] -> ctx.Apply fallback VUnit
              | _ -> raise (HostCommon.evalError "Option.defaultWith expects (function, option)") }

    let isNone : ExternalFunction =
        { Name = "Option.isNone"
          Scheme = Forall([ 0 ], TFun(TOption (TVar 0), TBool))
          Arity = 1
          Impl = fun _ -> function
              | [ VOption None ] -> VBool true
              | [ VOption (Some _) ] -> VBool false
              | _ -> raise (HostCommon.evalError "Option.isNone expects (option)") }

    let isSome : ExternalFunction =
        { Name = "Option.isSome"
          Scheme = Forall([ 0 ], TFun(TOption (TVar 0), TBool))
          Arity = 1
          Impl = fun _ -> function
              | [ VOption None ] -> VBool false
              | [ VOption (Some _) ] -> VBool true
              | _ -> raise (HostCommon.evalError "Option.isSome expects (option)") }

    let map : ExternalFunction =
        { Name = "Option.map"
          Scheme = Forall([ 0; 1 ], TFun(TFun(TVar 0, TVar 1), TFun(TOption (TVar 0), TOption (TVar 1))))
          Arity = 2
          Impl = fun ctx -> function
              | [ mapper; VOption (Some value) ] -> VOption (Some (ctx.Apply mapper value))
              | [ _; VOption None ] -> VOption None
              | _ -> raise (HostCommon.evalError "Option.map expects (function, option)") }
