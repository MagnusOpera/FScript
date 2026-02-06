namespace FScript.Host.Tests

open NUnit.Framework
open FsUnit
open FScript.Core
open FScript.Host
open FScript.Host.Tests.HostTestHelpers

[<TestFixture>]
type HashExternsTests () =
    [<Test>]
    member _.``hash_md5 computes known digest`` () =
        match invoke HashExterns.md5 [ VString "abc" ] with
        | VOption (Some (VString digest)) -> digest |> should equal "900150983cd24fb0d6963f7d28e17f72"
        | _ -> Assert.Fail("Expected digest")
