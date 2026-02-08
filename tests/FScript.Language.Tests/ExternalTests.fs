namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language

[<TestFixture>]
type ExternalTests () =
    let externs : ExternalFunction list =
        [ { Name = "toUpper"
            Scheme = Forall([], TFun(TString, TString))
            Arity = 1
            Impl = fun _ -> function
                | [ VString s ] -> VString (s.ToUpperInvariant())
                | _ -> failwith "bad args" }
          { Name = "add"
            Scheme = Forall([], TFun(TInt, TFun(TInt, TInt)))
            Arity = 2
            Impl = fun _ -> function
                | [ VInt a; VInt b ] -> VInt (a + b)
                | _ -> failwith "bad args" } ]

    [<Test>]
    member _.``Type inference sees external functions`` () =
        let typed = Helpers.inferWithExterns externs "toUpper \"a\""
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Evaluation calls external functions`` () =
        let v = Helpers.evalWithExterns externs "toUpper \"a\""
        match v with
        | VString s -> s |> should equal "A"
        | _ -> Assert.Fail("Expected string")

    [<Test>]
    member _.``External functions support currying`` () =
        let v = Helpers.evalWithExterns externs "(let inc = add 1\n    inc 2\n)"
        match v with
        | VInt i -> i |> should equal 3L
        | _ -> Assert.Fail("Expected int")

    [<Test>]
    member _.``Type error when external used with wrong type`` () =
        let act () = Helpers.inferWithExterns externs "toUpper 1" |> ignore
        act |> should throw typeof<TypeException>
