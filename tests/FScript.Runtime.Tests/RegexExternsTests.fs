namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type RegexExternsTests () =
    [<Test>]
    member _.``regex_match_groups returns None on invalid regex`` () =
        match invoke RegexExterns.match_groups [ VString "(["; VString "x" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")
