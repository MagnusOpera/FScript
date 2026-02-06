namespace FScript.Host.Tests

open NUnit.Framework
open FScript.Core
open FScript.Host
open FScript.Host.Tests.HostTestHelpers

[<TestFixture>]
type JsonExternsTests () =
    [<Test>]
    member _.``json_deserialize returns None for type mismatch`` () =
        let target = TRecord (Map.ofList [ "Age", TInt ])
        match invoke JsonExterns.deserialize [ VTypeToken target; VString "{\"Age\":\"x\"}" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")
