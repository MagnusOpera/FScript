namespace FScript.Runtime

open System.Collections.Generic
open FScript.Extensibility

module internal ProcessSdk =
    let defaults : ExternalFunction =
        { Name = "Process.defaults"; Scheme = ProcessSignatures.defaultsScheme; Arity = 0
          Impl = fun _ _ -> VRecord(Map.ofList [ "workingDirectory", VOption None; "environment", VMap Map.empty;
                                                "stdin", VOption None; "timeoutMilliseconds", VOption None;
                                                "maxOutputBytes", VInt 1048576L ]) }

    let private text = function VString value -> value | _ -> raise (ExtensionException "Expected a string.")
    let private optional project = function VOption value -> Option.map project value | _ -> raise (ExtensionException "Expected an option.")
    let private integer = function VInt value -> value | _ -> raise (ExtensionException "Expected an integer.")
    let private encode (outcome: ProcessOutcome) =
        let fields = ["stdout", VString outcome.Stdout; "stderr", VString outcome.Stderr]
        let case, fields =
            if isNull outcome.Message then "Exited", ("exitCode", VInt outcome.ExitCode)::fields
            else "Failed", ("message", VString (nonNull outcome.Message))::fields
        VUnionCase("ProcessResult", case, Some(VRecord(Map.ofList fields)))

    let exec (host: FScript.Extensibility.HostContext) : ExternalFunction =
        { Name = "Process.exec"; Scheme = ProcessSignatures.execScheme; Arity = 3
          Impl = fun context arguments ->
              match arguments with
              | [VString executable; VList arguments; VRecord options] ->
                  let directory = optional text options.["workingDirectory"] |> Option.toObj
                  let stdin = optional text options.["stdin"] |> Option.toObj
                  let timeout = optional integer options.["timeoutMilliseconds"] |> Option.toNullable
                  let limit = integer options.["maxOutputBytes"]
                  let environment =
                      match options.["environment"] with
                      | VMap entries -> entries |> Map.toArray |> Array.map (function MKString key, value -> KeyValuePair(key,text value) | _ -> raise (ExtensionException "Expected string environment keys."))
                      | _ -> raise (ExtensionException "Expected an environment map.")
                  let args = arguments |> List.map text |> List.toArray
                  context.ExposeAsync(fun cancellation -> task {
                      let! outcome = ProcessRunner.Run(host.RootDirectory, executable, args, directory, environment, stdin, timeout, limit, cancellation)
                      return encode outcome
                  })
              | _ -> raise (ExtensionException "Process.exec expects executable, arguments, and options.") }

/// Runtime process entry points; async I/O is adapted through the SDK 3 boundary.
module ProcessExterns =
    open FScript.Language

    let private adapt fn =
        ExtensionBridge.toLanguageRegistration {FScript.Extensibility.HostInterop.empty with Functions=[fn]}
        |> fun registration -> registration.Functions.Head

    /// Immutable default options are available without native execution privileges.
    let defaults : ExternalFunction =
        {adapt ProcessSdk.defaults with RequiredPermissions=HostPermissions.None}

    let exec (host: FScript.Runtime.HostContext) : ExternalFunction =
        let sdkHost = FScript.Extensibility.HostContext(host.RootDirectory,host.DeniedPathGlobs,[])
        let fn = adapt (ProcessSdk.exec sdkHost)
        {fn with
            RequiredPermissions=HostPermissions.Native
            Impl=fun context arguments ->
                context.CheckPermissions HostPermissions.Native
                fn.Impl context arguments}

    let internal registration host =
        let metadata : FScript.Extensibility.ExtensionRegistration =
            {FScript.Extensibility.HostInterop.empty with
                Types=[ProcessSignatures.optionsDescriptor]
                Unions=[ProcessSignatures.resultDescriptor]}
        let registration = ExtensionBridge.toLanguageRegistration metadata
        {registration with Functions=[defaults;exec host]}
