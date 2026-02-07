namespace FScript.Runtime.Tests

open System
open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type GuidExternsTests () =
    [<Test>]
    member _.``guid_new returns parseable guid`` () =
        match invoke GuidExterns.new_guid [ VUnit ] with
        | VOption (Some (VString g)) ->
            let ok, _ = Guid.TryParse(g)
            ok |> should equal true
        | _ -> Assert.Fail("Expected guid string")
