namespace FScript.Cli.Tests.Fixtures.Invalid

open FScript.Runtime

type InvalidExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : int =
        1

