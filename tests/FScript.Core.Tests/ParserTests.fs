namespace FScript.Core.Tests

open NUnit.Framework
open FsUnit
open FScript.Core

[<TestFixture>]
type ParserTests () =
    [<Test>]
    member _.``Parses top-level let binding`` () =
        let program = Helpers.parse "let x = 1"
        program.Length |> should equal 1
        match program.Head with
        | SLet (name, args, _, _, _) ->
            name |> should equal "x"
            args.Length |> should equal 0
        | _ -> Assert.Fail("Expected top-level let")

    [<Test>]
    member _.``Parses top-level function binding with arguments`` () =
        let program = Helpers.parse "let id x = x"
        match program.Head with
        | SLet ("id", args, _, _, _) -> args.Length |> should equal 1
        | _ -> Assert.Fail("Expected function let")

    [<Test>]
    member _.``Parses list literals`` () =
        let p1 = Helpers.parse "[1; 2]"
        match p1.[0] with
        | SExpr (EList (items, _)) -> items.Length |> should equal 2
        | _ -> Assert.Fail("Expected list literal")

    [<Test>]
    member _.``Parses range expressions`` () =
        let p1 = Helpers.parse "[1..5]"
        match p1.[0] with
        | SExpr (ERange (ELiteral (LInt 1L, _), ELiteral (LInt 5L, _), _)) -> ()
        | _ -> Assert.Fail("Expected range expression")

        let p2 = Helpers.parse "[5..1]"
        match p2.[0] with
        | SExpr (ERange (ELiteral (LInt 5L, _), ELiteral (LInt 1L, _), _)) -> ()
        | _ -> Assert.Fail("Expected descending range expression")

    [<Test>]
    member _.``Parses tuple literal`` () =
        let p = Helpers.parse "(1, true, \"x\")"
        match p.[0] with
        | SExpr (ETuple (items, _)) -> items.Length |> should equal 3
        | _ -> Assert.Fail("Expected tuple literal")

    [<Test>]
    member _.``Parses record literal and field access`` () =
        let p = Helpers.parse "{ Name = \"a\"; Age = 1 }.Age"
        match p.[0] with
        | SExpr (EFieldGet (ERecord (fields, _), "Age", _)) ->
            fields.Length |> should equal 2
        | _ -> Assert.Fail("Expected record field access")

    [<Test>]
    member _.``Parses record copy-update expression`` () =
        let p = Helpers.parse "{ p with Age = 2 }"
        match p.[0] with
        | SExpr (ERecordUpdate (EVar ("p", _), updates, _)) ->
            updates.Length |> should equal 1
        | _ -> Assert.Fail("Expected record update expression")

    [<Test>]
    member _.``Parses let expression without in`` () =
        let p = Helpers.parse "let x = (let y = 1\n    y + 1\n)"
        match p.[0] with
        | SLet (_, _, ELet ("y", _, _, _, _), _, _) -> ()
        | _ -> Assert.Fail("Expected nested let expression")

    [<Test>]
    member _.``Parses top-level recursive function binding`` () =
        let program = Helpers.parse "let rec fib n = if n < 2 then n else fib (n - 1)"
        match program.Head with
        | SLet ("fib", args, _, true, _) ->
            args.Length |> should equal 1
        | _ -> Assert.Fail("Expected recursive function let")

    [<Test>]
    member _.``Parses lambda expression`` () =
        let p = Helpers.parse "fun x -> x"
        match p.[0] with
        | SExpr (ELambda ("x", _, _)) -> ()
        | _ -> Assert.Fail("Expected lambda")

    [<Test>]
    member _.``Parses if then else`` () =
        let p = Helpers.parse "if true then 1 else 2"
        match p.[0] with
        | SExpr (EIf _) -> ()
        | _ -> Assert.Fail("Expected if expression")

    [<Test>]
    member _.``Parses if with elif`` () =
        let p = Helpers.parse "if false then 1 elif true then 2 else 3"
        match p.[0] with
        | SExpr (EIf (_, _, EIf (_, _, _, _), _)) -> ()
        | _ -> Assert.Fail("Expected nested if from elif")

    [<Test>]
    member _.``Parses raise expression`` () =
        let p = Helpers.parse "raise \"boom\""
        match p.[0] with
        | SExpr (ERaise (ELiteral (LString "boom", _), _)) -> ()
        | _ -> Assert.Fail("Expected raise expression")

    [<Test>]
    member _.``Parses for loop with inline body`` () =
        let p = Helpers.parse "for x in [1;2] do x |> ignore"
        match p.[0] with
        | SExpr (EFor ("x", EList (items, _), _, _)) ->
            items.Length |> should equal 2
        | _ -> Assert.Fail("Expected for loop expression")

    [<Test>]
    member _.``Parses for loop with block body`` () =
        let p = Helpers.parse "for x in [1;2] do\n    x |> ignore"
        match p.[0] with
        | SExpr (EFor ("x", EList (items, _), EBinOp ("|>", EVar ("x", _), EVar ("ignore", _), _), _)) ->
            items.Length |> should equal 2
        | _ -> Assert.Fail("Expected for loop expression with block body")

    [<Test>]
    member _.``Parses match with list patterns`` () =
        let src = "match [1;2] with\n| x::xs -> x\n| [] -> 0"
        let program = Helpers.parse src
        match program.[0] with
        | SExpr (EMatch (_, cases, _)) -> cases.Length |> should equal 2
        | _ -> Assert.Fail("Expected match")

    [<Test>]
    member _.``Parses match with option patterns`` () =
        let src = "match Some 1 with\n    | Some x -> x\n    | None -> 0"
        let program = Helpers.parse src
        match program.[0] with
        | SExpr (EMatch (_, cases, _)) -> cases.Length |> should equal 2
        | _ -> Assert.Fail("Expected match")

    [<Test>]
    member _.``Parses match with tuple patterns`` () =
        let src = "match (1, true) with\n    | (x, true) -> x\n    | _ -> 0"
        let program = Helpers.parse src
        match program.[0] with
        | SExpr (EMatch (_, (PTuple (_, _), _, _) :: _, _)) -> ()
        | _ -> Assert.Fail("Expected tuple pattern in match")

    [<Test>]
    member _.``Rejects misaligned multiline match cases`` () =
        let act () = Helpers.parse "match [1;2] with\n| x::xs -> x\n    | [] -> 0" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses operator precedence`` () =
        let p = Helpers.parse "1 + 2 * 3"
        match p.[0] with
        | SExpr (EBinOp ("+", _, EBinOp ("*", _, _, _), _)) -> ()
        | _ -> Assert.Fail("Expected precedence 1 + (2*3)")

    [<Test>]
    member _.``Parses pipeline operator`` () =
        let p = Helpers.parse "1 |> inc"
        match p.[0] with
        | SExpr (EBinOp ("|>", ELiteral (LInt 1L, _), EVar ("inc", _), _)) -> ()
        | _ -> Assert.Fail("Expected pipeline expression")

    [<Test>]
    member _.``Parses indented block in let binding`` () =
        let src = "let f x =\n    let y = x + 1\n    y"
        let p = Helpers.parse src
        match p.[0] with
        | SLet ("f", [_], ELet _, false, _) -> ()
        | _ -> Assert.Fail("Expected block-desugared let")

    [<Test>]
    member _.``Parses interpolated string`` () =
        let p = Helpers.parse "$\"hello {name}\""
        match p.[0] with
        | SExpr (EInterpolatedString ([ IPText "hello "; IPExpr (EVar ("name", _)) ], _)) -> ()
        | _ -> Assert.Fail("Expected interpolated string")

    [<Test>]
    member _.``Rejects in keyword in let expression`` () =
        let act () = Helpers.parse "(let x = 1 in x)" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects list generic-angle type syntax`` () =
        let act () = Helpers.parse "list<string>" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects option generic-angle type syntax`` () =
        let act () = Helpers.parse "option<string>" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects malformed range expressions`` () =
        let act1 () = Helpers.parse "[1..]" |> ignore
        act1 |> should throw typeof<ParseException>

        let act2 () = Helpers.parse "[..2]" |> ignore
        act2 |> should throw typeof<ParseException>

        let act3 () = Helpers.parse "[1..2;3]" |> ignore
        act3 |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects empty interpolation placeholder`` () =
        let act () = Helpers.parse "$\"a {} b\"" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects malformed for loop missing in`` () =
        let act () = Helpers.parse "for x [1] do x |> ignore" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects raise without argument`` () =
        let act () = Helpers.parse "raise" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects let rec without function argument`` () =
        let act () = Helpers.parse "let rec x = 1" |> ignore
        act |> should throw typeof<ParseException>
