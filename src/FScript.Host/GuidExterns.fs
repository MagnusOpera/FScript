namespace FScript.Host

open System
open FScript.Core

module GuidExterns =
    let new_guid : ExternalFunction =
        { Name = "Guid.new"
          Scheme = Forall([ 0 ], TFun(TVar 0, TOption TString))
          Arity = 1
          Impl = function
              | [ _ ] -> HostCommon.some (VString (Guid.NewGuid().ToString("D")))
              | _ -> raise (HostCommon.evalError "Guid.new expects one argument") }
