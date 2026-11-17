namespace FScript.Runtime

open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open FScript.Extensibility

/// Ordinary CLR FFI companions, with SDK values at the data boundary.
[<AbstractClass; Sealed>]
type CompiledProcess private () =
    static member Defaults() =
        VRecord(Map.ofList ["workingDirectory", VOption None; "environment", VMap Map.empty;
                           "stdin", VOption None; "timeoutMilliseconds", VOption None; "maxOutputBytes", VInt 1048576L])

    static member Exec(executable: string, arguments: string array, options: ScriptValue) = task {
        let text = function VString value -> value | _ -> raise (ExtensionException "Expected a string.")
        let integer = function VInt value -> value | _ -> raise (ExtensionException "Expected an integer.")
        let optional project = function VOption value -> Option.map project value | _ -> raise (ExtensionException "Expected an option.")
        let fields = match options with VRecord fields -> fields | _ -> raise (ExtensionException "Expected process options.")
        let environment =
            match fields.["environment"] with
            | VMap entries -> entries |> Map.toArray |> Array.map (function MKString key,value -> KeyValuePair(key,text value) | _ -> raise (ExtensionException "Expected string environment keys."))
            | _ -> raise (ExtensionException "Expected an environment map.")
        let! outcome = ProcessRunner.Run(Directory.GetCurrentDirectory(), executable, arguments,
                                        optional text fields.["workingDirectory"] |> Option.toObj, environment,
                                        optional text fields.["stdin"] |> Option.toObj,
                                        optional integer fields.["timeoutMilliseconds"] |> Option.toNullable,
                                        integer fields.["maxOutputBytes"], CancellationToken.None)
        let payload = ["stdout", VString outcome.Stdout; "stderr", VString outcome.Stderr]
        let case,payload =
            match outcome.Message with
            | null -> "Exited", ("exitCode", VInt outcome.ExitCode)::payload
            | message -> "Failed", ("message", VString message)::payload
        return VUnionCase("ProcessResult", case, Some(VRecord(Map.ofList payload)))
    }

    static member FScriptCompiledFunctions() : IEnumerable<KeyValuePair<string,MethodInfo>> =
        [KeyValuePair("Process.defaults", typeof<CompiledProcess>.GetMethod("Defaults") |> nonNull)
         KeyValuePair("Process.exec", typeof<CompiledProcess>.GetMethod("Exec") |> nonNull)]
