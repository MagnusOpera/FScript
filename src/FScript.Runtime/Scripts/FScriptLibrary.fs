namespace FScript.Runtime

open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open FScript.Language

type private SV = FScript.Extensibility.ScriptValue

module internal ScriptData =
    let rec fromSdk = function
        | SV.VString value -> VString value
        | SV.VInt value -> VInt value
        | SV.VOption value -> VOption(Option.map fromSdk value)
        | SV.VList values -> VList(List.map fromSdk values)
        | SV.VRecord values -> VRecord(Map.map (fun _ -> fromSdk) values)
        | _ -> raise (HostCommon.evalError "Expected script execution options.")
    let rec toSdk = function
        | VString value -> SV.VString value
        | VInt value -> SV.VInt value
        | VOption value -> SV.VOption(Option.map toSdk value)
        | VList values -> SV.VList(List.map toSdk values)
        | VRecord values -> SV.VRecord(Map.map (fun _ -> toSdk) values)
        | VUnionCase(name,case,value) -> SV.VUnionCase(name,case,Option.map toSdk value)
        | _ -> raise (HostCommon.evalError "Expected script execution result data.")

module internal FScriptLibrary =
    let defaults : ExternalFunction =
        {Name="FScript.defaults"; Scheme=Forall([],FScriptSignatures.optionsType); Arity=0; RequiredPermissions=HostPermissions.None
         Impl=fun _ _ -> FScriptSignatures.defaultValue}

    let rec registration context policy scope =
        let standard = StandardLibrary.registration context policy
        {standard with
            Types=FScriptSignatures.optionsDescriptor::standard.Types
            Unions=FScriptSignatures.resultDescriptor::standard.Unions
            Functions=defaults::((exec context policy scope)::standard.Functions |> List.filter (fun fn -> HostInterop.allows context.Permissions fn.RequiredPermissions))}

    and exec context policy scope : ExternalFunction =
        {Name="FScript.exec"; Scheme=FScriptSignatures.execScheme; Arity=3; RequiredPermissions=HostPermissions.Threading
         Impl=fun caller values ->
            caller.CheckPermissions HostPermissions.Threading
            let permissions = HostInterop.currentPermissions caller
            match values with
            | [VString path; VList args; options] ->
                let args = args |> List.map (function VString value -> value | _ -> raise (HostCommon.evalError "Expected string arguments."))
                HostInterop.startTask caller (fun cancellation -> task {
                    use linked = CancellationTokenSource.CreateLinkedTokenSource(scope.Cancellation,cancellation)
                    return! ScriptRunner.run registration context policy {scope with Cancellation=linked.Token} permissions path args options
                })
            | _ -> raise (HostCommon.evalError "FScript.exec expects a file path, string arguments, and options.")}

/// Standard library functions that can also be registered individually by a host.
module FScriptExterns =
    let defaults = FScriptLibrary.defaults
    let exec context = FScriptLibrary.exec context (FileSystemPolicy.create context) ScriptRunner.initialScope

/// CLR FFI entry points interpret children through the Runtime library.
[<AbstractClass; Sealed>]
type CompiledFScript private () =
    static member Defaults() = ScriptData.toSdk FScriptSignatures.defaultValue
    static member Exec(path: string, arguments: string array, options: FScript.Extensibility.ScriptValue) = task {
        let context = {HostContext.create (Directory.GetCurrentDirectory()) with Permissions=HostPermissions.All}
        let! result = ScriptRunner.run FScriptLibrary.registration context (FileSystemPolicy.create context) ScriptRunner.initialScope
                                      HostPermissions.All path (Array.toList arguments) (ScriptData.fromSdk options)
        return ScriptData.toSdk result
    }
    static member FScriptCompiledFunctions() : IEnumerable<KeyValuePair<string,MethodInfo>> =
        [KeyValuePair("FScript.defaults",typeof<CompiledFScript>.GetMethod("Defaults") |> nonNull)
         KeyValuePair("FScript.exec",typeof<CompiledFScript>.GetMethod("Exec") |> nonNull)]
