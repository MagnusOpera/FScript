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
                [ { Name = "Ext.node"
                    Scheme = Forall([], TFun(TInt, TNamed "Node"))
                    Arity = 1
                    Impl = fun _ args ->
                        match args with
                        | [VInt n] -> HostTypes.wrap registration { Value = n; Context = ctx }
                        | _ -> failwith "Expected integer" }
                  { Name = "Ext.acceptNode"
                    Scheme = Forall([], TFun(TNamed "Node", TInt))
                    Arity = 1
                    Impl = fun _ args ->
                        let node = HostTypes.unwrap<ProviderNode> args.Head
                        if node.Context = ctx then VInt node.Value
                        else failwith "Host state was lost" }
                  { Name = "Ext.acceptPoint"
                    Scheme = Forall([], TFun(TNamed "Point", TInt))
                    Arity = 1
                    Impl = fun _ args -> VInt((HostTypes.unwrap<ProviderPoint> args.Head).X) } ] }
