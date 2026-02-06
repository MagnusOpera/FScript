namespace FScript.Core.Tests

open NUnit.Framework
open FsUnit
open FScript.Core

[<TestFixture>]
type TypeInferenceTests () =
    [<Test>]
    member _.``Infers primitive literal types`` () =
        let check src expected =
            match Helpers.infer src |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal expected
            | _ -> Assert.Fail("Expected expression")
        check "1" TInt
        check "1.0" TFloat
        check "true" TBool
        check "\"a\"" TString

    [<Test>]
    member _.``Infers polymorphic identity function`` () =
        let typed = Helpers.infer "let id x = x"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, t, _) ->
            match t with
            | TFun (TVar a, TVar b) when a = b -> ()
            | _ -> Assert.Fail("Expected a -> a")
        | _ -> Assert.Fail("Expected let")

    [<Test>]
    member _.``Infers let-polymorphism in nested let expressions`` () =
        let typed = Helpers.infer "(let id = fun x -> x\n    let a = id 1\n    id true\n)"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TBool
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers list types for literals and cons`` () =
        let typed = Helpers.infer "1 :: []"
        match typed |> List.last with
        | TypeInfer.TSExpr texpr ->
            match texpr.Type with
            | TList TInt -> ()
            | _ -> Assert.Fail("Expected int list")
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers tuple type`` () =
        let typed = Helpers.infer "(1, true, \"x\")"
        match typed |> List.last with
        | TypeInfer.TSExpr te ->
            te.Type |> should equal (TTuple [ TInt; TBool; TString ])
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers record type and field projection`` () =
        let typed = Helpers.infer "let p = { Name = \"a\"; Age = 1 }\np.Age"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers record copy-update`` () =
        let typed = Helpers.infer "let p = { Name = \"a\"; Age = 1 }\nlet p2 = { p with Age = 2 }\np2.Age"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers append list type`` () =
        let typed = Helpers.infer "[1] @ [2]"
        match typed |> List.last with
        | TypeInfer.TSExpr te ->
            match te.Type with
            | TList TInt -> ()
            | _ -> Assert.Fail("Expected int list")
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers range as int list`` () =
        let t1 = Helpers.infer "[1..5]" |> List.last
        match t1 with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TList TInt)
        | _ -> Assert.Fail("Expected expression")

        let t2 = Helpers.infer "[5..1]" |> List.last
        match t2 with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TList TInt)
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Formats inferred list and option types with F# postfix syntax`` () =
        let listType =
            match Helpers.infer "[1]" |> List.last with
            | TypeInfer.TSExpr te -> te.Type
            | _ -> failwith "Expected expression"
        let optionType =
            match Helpers.infer "Some 1" |> List.last with
            | TypeInfer.TSExpr te -> te.Type
            | _ -> failwith "Expected expression"
        Types.typeToString listType |> should equal "int list"
        Types.typeToString optionType |> should equal "int option"

    [<Test>]
    member _.``Infers pipeline operator`` () =
        let typed = Helpers.infer "let inc x = x + 1\n1 |> inc"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers if expression type`` () =
        let typed = Helpers.infer "if true then 1 else 2"
        match typed |> List.last with
        | TypeInfer.TSExpr texpr -> texpr.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on option`` () =
        let typed = Helpers.infer "match Some 1 with | Some x -> x | None -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on list`` () =
        let typed = Helpers.infer "match [1;2] with | x::xs -> x | [] -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers comparison and logical operators`` () =
        let t1 = Helpers.infer "1 < 2" |> List.last
        match t1 with
        | TypeInfer.TSExpr te -> te.Type |> should equal TBool
        | _ -> Assert.Fail("Expected expression")
        let t2 = Helpers.infer "true && false" |> List.last
        match t2 with
        | TypeInfer.TSExpr te -> te.Type |> should equal TBool
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for non-bool if condition`` () =
        let act () = Helpers.infer "if 1 then 2 else 3" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for list append with non-list`` () =
        let act () = Helpers.infer "[1] @ 2" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for unknown field in record update`` () =
        let act () = Helpers.infer "let p = { Name = \"a\"; Age = 1 }\n{ p with Missing = 2 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type errors for non-int ranges`` () =
        let act1 () = Helpers.infer "[1.0..5.0]" |> ignore
        act1 |> should throw typeof<TypeException>

        let act2 () = Helpers.infer "[\"a\"..\"z\"]" |> ignore
        act2 |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error when non-unit value is discarded in block`` () =
        let act () = Helpers.infer "let a =\n  42\n  666" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Allows explicit ignore for discarded value`` () =
        let typed = Helpers.infer "let a =\n  42 |> ignore\n  666\na"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports occurs check failure for self application`` () =
        let act () = Helpers.infer "fun x -> x x" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers typeof as type token`` () =
        let typed = Helpers.infer "type Package = { Name: string }\ntypeof Package"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TTypeToken
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers string keyed map type suffix`` () =
        let typed = Helpers.infer "type Package = { Deps: int map }\n0"
        match typed.Head with
        | TypeInfer.TSType t ->
            let depType = t.Fields |> List.find (fun (n, _) -> n = "Deps") |> snd
            depType |> should equal (TRPostfix(TRName "int", "map"))
        | _ -> Assert.Fail("Expected type declaration")
