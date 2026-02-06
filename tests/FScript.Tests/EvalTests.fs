namespace FScript.Tests

open NUnit.Framework
open FsUnit
open FScript.Core

[<TestFixture>]
type EvalTests () =
    let assertInt expected actual =
        match actual with
        | VInt v -> v |> should equal expected
        | _ -> Assert.Fail("Expected int value")

    let assertFloat expected actual =
        match actual with
        | VFloat v -> v |> should equal expected
        | _ -> Assert.Fail("Expected float value")

    let assertBool expected actual =
        match actual with
        | VBool v -> v |> should equal expected
        | _ -> Assert.Fail("Expected bool value")

    let assertListInt expected actual =
        match actual with
        | VList values ->
            let ints =
                values
                |> List.map (function | VInt i -> i | _ -> -1L)
            ints |> should equal expected
        | _ -> Assert.Fail("Expected int list value")

    [<Test>]
    member _.``Evaluates arithmetic precedence`` () =
        Helpers.eval "1 + 2 * 3" |> assertInt 7L

    [<Test>]
    member _.``Evaluates numeric operators`` () =
        Helpers.eval "7 - 2" |> assertInt 5L
        Helpers.eval "2 * 3" |> assertInt 6L
        Helpers.eval "8 / 2" |> assertInt 4L
        Helpers.eval "7 % 4" |> assertInt 3L

    [<Test>]
    member _.``Evaluates float arithmetic`` () =
        Helpers.eval "1.5 + 2.5" |> assertFloat 4.0

    [<Test>]
    member _.``Evaluates lists and append`` () =
        Helpers.eval "[1;2] @ [3]" |> assertListInt [ 1L; 2L; 3L ]
        Helpers.eval "1 :: [2;3]" |> assertListInt [ 1L; 2L; 3L ]

    [<Test>]
    member _.``Evaluates integer ranges`` () =
        Helpers.eval "[1..5]" |> assertListInt [ 1L; 2L; 3L; 4L; 5L ]
        Helpers.eval "[5..1]" |> assertListInt [ 5L; 4L; 3L; 2L; 1L ]
        Helpers.eval "[3..3]" |> assertListInt [ 3L ]

    [<Test>]
    member _.``Evaluates tuples and records`` () =
        match Helpers.eval "(1, true)" with
        | VTuple [ VInt 1L; VBool true ] -> ()
        | _ -> Assert.Fail("Expected tuple value")
        Helpers.eval "{ Name = \"a\"; Age = 42 }.Age" |> assertInt 42L

    [<Test>]
    member _.``Evaluates record copy-update immutably`` () =
        Helpers.eval "let p = { Name = \"a\"; Age = 1 }\nlet p2 = { p with Age = 2 }\np.Age" |> assertInt 1L
        Helpers.eval "let p = { Name = \"a\"; Age = 1 }\nlet p2 = { p with Age = 2 }\np2.Age" |> assertInt 2L

    [<Test>]
    member _.``Evaluates match on list`` () =
        let src = "match [1;2] with\n| x::xs -> x\n| [] -> 0"
        Helpers.eval src |> assertInt 1L

    [<Test>]
    member _.``Evaluates option values`` () =
        match Helpers.eval "Some 4" with
        | VOption (Some (VInt 4L)) -> ()
        | _ -> Assert.Fail("Expected Some 4")
        match Helpers.eval "None" with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")

    [<Test>]
    member _.``Evaluates match on option`` () =
        let src = "match Some 3 with | Some x -> x + 1 | None -> 0"
        Helpers.eval src |> assertInt 4L

    [<Test>]
    member _.``Evaluates if expressions`` () =
        Helpers.eval "if true then 1 else 2" |> assertInt 1L
        Helpers.eval "if false then 1 else 2" |> assertInt 2L

    [<Test>]
    member _.``Evaluates comparison and logical operators`` () =
        Helpers.eval "1 < 2" |> assertBool true
        Helpers.eval "true && false" |> assertBool false

    [<Test>]
    member _.``Evaluates pipeline operator`` () =
        Helpers.eval "let inc x = x + 1\n1 |> inc" |> assertInt 2L
        Helpers.eval "1 |> (fun x -> x + 2)" |> assertInt 3L

    [<Test>]
    member _.``Evaluates lexical closures`` () =
        Helpers.eval "let x = 2\nlet addx y = x + y\naddx 3" |> assertInt 5L

    [<Test>]
    member _.``Evaluates function application`` () =
        Helpers.eval "(fun x -> fun y -> x + y) 2 3" |> assertInt 5L

    [<Test>]
    member _.``Evaluates top-level function value as unit result`` () =
        let v = Helpers.eval "let f x = x"
        match v with
        | VUnit -> ()
        | _ -> Assert.Fail("Expected unit")

    [<Test>]
    member _.``Reports type error for applying non-function`` () =
        let act () = Helpers.eval "1 2" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports runtime error for non-exhaustive match`` () =
        let act () = Helpers.eval "match [1] with | [] -> 0" |> ignore
        act |> should throw typeof<EvalException>

    [<Test>]
    member _.``Reports type error for unknown field in record update`` () =
        let act () = Helpers.eval "let p = { Name = \"a\"; Age = 1 }\n{ p with Missing = 2 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error when non-unit value is discarded`` () =
        let act () = Helpers.eval "let a =\n  42\n  666" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Evaluates block with explicit ignore`` () =
        Helpers.eval "let a =\n  42 |> ignore\n  666\na" |> assertInt 666L
