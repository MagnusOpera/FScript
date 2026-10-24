namespace FScript.Cli.Tests.Fixtures.Invalid

open FScript.Extensibility

type InvalidExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : int =
        1

