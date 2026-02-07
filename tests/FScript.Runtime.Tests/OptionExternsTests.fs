namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type OptionExternsTests () =
    [<Test>]
    member _.``option externs basic behavior`` () =
        let someValue = VOption (Some (VInt 7L))
        let noneValue = VOption None

        match invoke OptionExterns.get [ someValue ] with
        | VInt 7L -> ()
        | _ -> Assert.Fail("Expected Option.get Some 7")

        match invoke OptionExterns.defaultValue [ VInt 9L; noneValue ] with
        | VInt 9L -> ()
        | _ -> Assert.Fail("Expected default value on None")

        match invoke OptionExterns.defaultValue [ VInt 9L; someValue ] with
        | VInt 7L -> ()
        | _ -> Assert.Fail("Expected inner value on Some")

        match invoke OptionExterns.isNone [ noneValue ] with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected isNone true for None")

        match invoke OptionExterns.isSome [ someValue ] with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected isSome true for Some")

    [<Test>]
    member _.``Option.get errors on None`` () =
        let act () = invoke OptionExterns.get [ VOption None ] |> ignore
        Assert.Throws<EvalException>(TestDelegate act) |> ignore
