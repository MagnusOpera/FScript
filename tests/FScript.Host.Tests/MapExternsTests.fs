namespace FScript.Host.Tests

open NUnit.Framework
open FScript.Core
open FScript.Host
open FScript.Host.Tests.HostTestHelpers

[<TestFixture>]
type MapExternsTests () =
    [<Test>]
    member _.``map externs perform CRUD`` () =
        let m0 = invoke MapExterns.empty [ VUnit ]
        let m1 = invoke MapExterns.add [ VString "a"; VInt 1L; m0 ]
        let found = invoke MapExterns.tryFind [ VString "a"; m1 ]
        let exists = invoke MapExterns.containsKey [ VString "a"; m1 ]
        let m2 = invoke MapExterns.remove [ VString "a"; m1 ]

        match found with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

        match exists with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected containsKey true")

        match invoke MapExterns.tryFind [ VString "a"; m2 ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected removed key")
