namespace FScript.Host.Tests

open NUnit.Framework
open FScript.Core
open FScript.Host
open FScript.Host.Tests.HostTestHelpers

[<TestFixture>]
type RegexExternsTests () =
    [<Test>]
    member _.``regex_match_groups returns None on invalid regex`` () =
        match invoke RegexExterns.match_groups [ VString "(["; VString "x" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")
