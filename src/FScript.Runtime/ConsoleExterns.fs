namespace FScript.Runtime

open System
open FScript.Language

module ConsoleExterns =
    let write_line : ExternalFunction =
        { Name = "Console.writeLine"
          Scheme = Forall([], TFun(TString, TUnit))
          Arity = 1
          Impl = fun _ -> function
              | [ VString text ] ->
                  Console.WriteLine(text)
                  VUnit
              | _ -> raise (HostCommon.evalError "Console.writeLine expects (string)") }

    let read_line : ExternalFunction =
        { Name = "Console.readLine"
          Scheme = Forall([], TFun(TUnit, TOption TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VUnit ] ->
                  match Console.ReadLine() with
                  | null -> HostCommon.none
                  | text -> HostCommon.some (VString text)
              | _ -> raise (HostCommon.evalError "Console.readLine expects (unit)") }
