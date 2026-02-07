namespace FScript.Runtime

open System
open FScript.Language

module PrintExterns =
    let print : ExternalFunction =
        { Name = "print"
          Scheme = Forall([], TFun(TString, TUnit))
          Arity = 1
          Impl = function
              | [ VString text ] ->
                  Console.WriteLine(text)
                  VUnit
              | _ -> raise (HostCommon.evalError "print expects (string)") }
