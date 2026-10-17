namespace FScript.Cli.Tests.Fixtures.Valid

open FScript.Language
open FScript.Runtime

type ValidExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : ExternalFunction list =
        [ { Name = "Ext.answer"
            Scheme = Forall([], TInt)
            Arity = 0
            Impl = (fun _ _ -> VInt 42L) } ]

type ValidHostContextProvider =
    [<FScriptExternProvider>]
    static member Provide(ctx: HostContext) : ExternalFunction list =
        let marker = System.IO.Path.Combine(ctx.RootDirectory, "provider-registration.requested")
        if System.IO.File.Exists(marker) then
            System.IO.File.WriteAllText(System.IO.Path.Combine(ctx.RootDirectory, "provider-registration.executed"), "registered")
            System.Console.WriteLine("fixture provider registration")
        [ { Name = "Ext.rootDirectory"
            Scheme = Forall([], TString)
            Arity = 0
            Impl = (fun _ _ -> VString ctx.RootDirectory) } ]


[<FScriptHostType("Node")>]
type ProviderNode =
    { [<FScriptVisible>] Value: int64
      Context: HostContext }

[<FScriptHostType("Point", ScriptConstructible = true)>]
type ProviderPoint = { [<FScriptVisible>] X: int64 }

type HostManagedProvider =
    [<FScriptExternProvider>]
    static member Provide(ctx: HostContext) : ExtensionRegistration =
        let descriptor = HostTypes.describe<ProviderNode>()
        let registration = { HostInterop.empty with Types = [descriptor; HostTypes.describe<ProviderPoint>()] }
        { registration with
            Functions =
                [ HostFunctions.create registration "Ext.node" (fun (n: int64) -> { Value = n; Context = ctx })
                  HostFunctions.create registration "Ext.acceptNode" (fun (node: ProviderNode) ->
                      if node.Context = ctx then node.Value
                      else failwith "Host state was lost")
                  HostFunctions.create registration "Ext.acceptPoint" (fun (point: ProviderPoint) -> point.X) ] }
