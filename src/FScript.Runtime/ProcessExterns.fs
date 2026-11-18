namespace FScript.Runtime

open System.Collections.Generic
open System.Threading
open FScript.Language

module ProcessExterns =
    let defaults : ExternalFunction =
        { Name = "Process.defaults"; Scheme = ProcessSignatures.defaultsScheme; Arity = 0
          RequiredPermissions = HostPermissions.None
          Impl = fun _ _ -> VRecord(Map.ofList [ "workingDirectory", VOption None; "environment", VMap Map.empty;
                                                "stdin", VOption None; "timeoutMilliseconds", VOption None;
                                                "maxOutputBytes", VInt 1048576L ]) }

    let private text = function VString value -> value | _ -> raise (HostCommon.evalError "Expected a string.")
    let private optional project = function VOption value -> Option.map project value | _ -> raise (HostCommon.evalError "Expected an option.")
    let private integer = function VInt value -> value | _ -> raise (HostCommon.evalError "Expected an integer.")
    let private encode (outcome: ProcessOutcome) =
        let fields = ["stdout", VString outcome.Stdout; "stderr", VString outcome.Stderr]
        let case, fields =
            if isNull outcome.Message then "Exited", ("exitCode", VInt outcome.ExitCode)::fields
            else "Failed", ("message", VString (nonNull outcome.Message))::fields
        VUnionCase("ProcessResult", case, Some(VRecord(Map.ofList fields)))

    let exec (host: HostContext) : ExternalFunction =
        let required = HostPermissions.Native ||| HostPermissions.Threading
        { Name = "Process.exec"; Scheme = ProcessSignatures.execScheme; Arity = 3; RequiredPermissions = required
          Impl = fun context arguments ->
              context.CheckPermissions required
              match arguments with
              | [VString executable; VList arguments; VRecord options] ->
                  let directory = optional text options.["workingDirectory"] |> Option.toObj
                  let stdin = optional text options.["stdin"] |> Option.toObj
                  let timeout = optional integer options.["timeoutMilliseconds"] |> Option.toNullable
                  let limit = integer options.["maxOutputBytes"]
                  let environment =
                      match options.["environment"] with
                      | VMap entries -> entries |> Map.toArray |> Array.map (function MKString key, value -> KeyValuePair(key,text value) | _ -> raise (HostCommon.evalError "Expected string environment keys."))
                      | _ -> raise (HostCommon.evalError "Expected an environment map.")
                  let args = arguments |> List.map text |> List.toArray
                  let worker : ExternalFunction =
                      { Name = "<process worker>"; Scheme = Forall([], TFun(TUnit,TNamed "ProcessResult")); Arity = 1
                        RequiredPermissions = required
                        Impl = fun worker _ ->
                            use cancellation = new CancellationTokenSource()
                            let running = ProcessRunner.Run(host.RootDirectory, executable, args, directory, environment, stdin, timeout, limit, cancellation.Token)
                            try
                                while not running.IsCompleted do
                                    worker.CheckRuntime()
                                    Thread.Sleep(25)
                                running.GetAwaiter().GetResult() |> encode
                            with _ ->
                                cancellation.Cancel()
                                try running.GetAwaiter().GetResult() |> ignore with _ -> ()
                                reraise () }
                  context.SpawnTask(VExternal(worker,[]))
              | _ -> raise (HostCommon.evalError "Process.exec expects executable, arguments, and options.") }
