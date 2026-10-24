namespace FScript.Cli.Tests.Fixtures.Conflict

open FScript.Extensibility

type ConflictExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : ExternalFunction list =
        [ { Name = "Fs.exists"
            Scheme = Forall([], TBool)
            Arity = 0
            Impl = (fun _ _ -> VBool true) } ]

