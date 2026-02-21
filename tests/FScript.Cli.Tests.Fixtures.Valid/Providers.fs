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

