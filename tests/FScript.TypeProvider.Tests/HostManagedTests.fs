namespace FScript.TypeProvider.Tests

open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.TypeProvider

type HostManagedExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration =
        let descriptor = { Name = "Node"; Fields = Map.ofList ["Value", TInt]; HostManaged = true }
        let types = { HostInterop.empty with Types = [descriptor] }
        { types with
            Functions =
                [ { Name = "Host.node"; Scheme = Forall([], TNamed "Node"); Arity = 0
                    Impl = fun _ _ -> HostInterop.create types "Node" (Map.ofList ["Value", VInt 42L]) ("secret" :> obj) } ] }

[<TestFixture>]
type HostManagedTests() =
    [<Test>]
    member _.``Type provider resolution retains registered types for internal script use`` () =
        let providerName =
            match typeof<HostManagedExternProvider>.AssemblyQualifiedName with
            | null -> failwith "Missing provider name"
            | name -> name
        let registration = ScriptRuntime.resolveRegistration (Directory.GetCurrentDirectory()) [providerName]
        let loaded = ScriptHost.loadSourceWithRegistration registration "[<export>] let read () = Host.node.Value"
        match ScriptHost.invoke loaded "read" [VUnit] with
        | VInt 42L -> ()
        | _ -> Assert.Fail("Expected visible field from registered type")

    [<Test>]
    member _.``Host-managed and constrained record CLR exports fail explicitly`` () =
        for t in [THostManaged("Node", Map.ofList ["Value", TInt]); TRecordShape(0, Map.ofList ["Value", TInt], false); TNamed "Node"] do
            match Contract.ofFScriptType "export" t with
            | Error message -> Assert.That(message, Does.Contain("not supported in exported signatures"))
            | Ok _ -> Assert.Fail("Host-managed CLR projection must remain unsupported")
