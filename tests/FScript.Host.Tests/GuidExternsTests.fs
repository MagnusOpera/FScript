namespace FScript.Host.Tests

open System
open NUnit.Framework
open FsUnit
open FScript.Core
open FScript.Host
open FScript.Host.Tests.HostTestHelpers

[<TestFixture>]
type GuidExternsTests () =
    [<Test>]
    member _.``guid_new returns parseable guid`` () =
        match invoke GuidExterns.new_guid [ VUnit ] with
        | VOption (Some (VString g)) ->
            let ok, _ = Guid.TryParse(g)
            ok |> should equal true
        | _ -> Assert.Fail("Expected guid string")
