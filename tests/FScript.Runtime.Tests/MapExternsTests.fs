namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

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

    [<Test>]
    member _.``map_of_list builds map and keeps last duplicate`` () =
        let source =
            VList
                [ VTuple [ VString "a"; VInt 1L ]
                  VTuple [ VString "b"; VInt 2L ]
                  VTuple [ VString "a"; VInt 3L ] ]
        let m = invoke MapExterns.ofList [ source ]

        match invoke MapExterns.tryFind [ VString "a"; m ] with
        | VOption (Some (VInt 3L)) -> ()
        | _ -> Assert.Fail("Expected duplicate key to keep last value")

    [<Test>]
    member _.``map_of_list rejects invalid tuple list`` () =
        let act () = invoke MapExterns.ofList [ VList [ VInt 1L ] ] |> ignore
        Assert.Throws<EvalException>(TestDelegate act) |> ignore
