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
        | TypeInfer.TSLet (_, _, t, _, _) ->
            match t with
            | TFun (TVar a, TVar b) when a = b -> ()
            | _ -> Assert.Fail("Expected a -> a")
        | _ -> Assert.Fail("Expected let")

    [<Test>]
    member _.``Infers recursive function binding`` () =
        let typed = Helpers.infer "let rec sum n =\n    if n = 0 then 0 else n + sum (n - 1)\nsum 5"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

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
    member _.``Infers if expression type with elif`` () =
        let typed = Helpers.infer "if false then 1 elif true then 2 else 3"
        match typed |> List.last with
        | TypeInfer.TSExpr texpr -> texpr.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers raise as polymorphic in branch context`` () =
        let typed = Helpers.infer "if true then raise \"boom\" else 1"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers for loop as unit`` () =
        let typed = Helpers.infer "for x in [1;2] do x |> ignore"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TUnit
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on option`` () =
        let typed = Helpers.infer "match Some 1 with\n    | Some x -> x\n    | None -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on list`` () =
        let typed = Helpers.infer "match [1;2] with\n    | x::xs -> x\n    | [] -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on tuple`` () =
        let typed = Helpers.infer "match (1, true) with\n    | (x, true) -> x\n    | _ -> 0"
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
    member _.``Reports type error for raise with non-string`` () =
        let act () = Helpers.infer "raise 1" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for for loop over non-list`` () =
        let act () = Helpers.infer "for x in 1 do x |> ignore" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for for loop non-unit body`` () =
        let act () = Helpers.infer "for x in [1] do x" |> ignore
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
    member _.``Infers explicit recursive type declarations`` () =
        let typed =
            Helpers.infer
                "type rec Node = { Value: int; Next: Node option; Children: Node list; Index: Node map; Pair: (int * Node option) }\n0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers multiline explicit recursive type declaration`` () =
        let typed =
            Helpers.infer
                "type rec Node =\n    { Value: int\n      Next: Node option\n      Children: Node list }\n0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Requires rec keyword for recursive type declaration`` () =
        let act () =
            Helpers.infer
                "type Node = { Value: int; Next: Node option }\n0"
            |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Rejects mutual recursive type declarations`` () =
        let act () =
            Helpers.infer
                "type rec A = { B: B option }\ntype rec B = { A: A option }\n0"
            |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers string keyed map type suffix`` () =
        let typed = Helpers.infer "type Package = { Deps: int map }\n0"
        match typed.Head with
        | TypeInfer.TSType t ->
            let depType = t.Fields |> List.find (fun (n, _) -> n = "Deps") |> snd
            depType |> should equal (TRPostfix(TRName "int", "map"))
        | _ -> Assert.Fail("Expected type declaration")

    [<Test>]
    member _.``Infers interpolated string as string`` () =
        let typed = Helpers.infer "let name = \"world\"\n$\"hello {name}\""
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Allows non-string interpolation placeholder`` () =
        let typed = Helpers.infer "$\"value={1}\""
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")
