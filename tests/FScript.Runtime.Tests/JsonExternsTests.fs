namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type JsonExternsTests () =
    [<Test>]
    member _.``json_deserialize returns None for type mismatch`` () =
        let target = TRecord (Map.ofList [ "Age", TInt ])
        match invoke JsonExterns.deserialize [ VTypeToken target; VString "{\"Age\":\"x\"}" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")
