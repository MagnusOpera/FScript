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
        [ { Name = "Ext.rootDirectory"
            Scheme = Forall([], TString)
            Arity = 0
            Impl = (fun _ _ -> VString ctx.RootDirectory) } ]


type HostManagedProvider =
    [<FScriptExternProvider>]
    static member Provide(ctx: HostContext) : ExtensionRegistration =
        let descriptor = { Name = "Node"; Fields = Map.ofList ["Value", TInt]; HostManaged = true }
        let registration = { HostInterop.empty with Types = [descriptor] }
        { registration with
            Functions =
                [ { Name = "Ext.node"
                    Scheme = Forall([], TFun(TInt, TNamed "Node"))
                    Arity = 1
                    Impl = fun _ args ->
                        match args with
                        | [VInt n] -> HostInterop.create registration "Node" (Map.ofList ["Value", VInt n]) ctx
                        | _ -> failwith "Expected integer" }
                  { Name = "Ext.acceptNode"
                    Scheme = Forall([], TFun(TNamed "Node", TInt))
                    Arity = 1
                    Impl = fun _ args ->
                        let host = HostInterop.unwrap "Node" args.Head
                        match host.Payload with
                        | :? HostContext as original when original = ctx -> host.Fields.["Value"]
                        | _ -> failwith "Host state was lost" } ] }
