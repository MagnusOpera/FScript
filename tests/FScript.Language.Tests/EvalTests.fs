namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language

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
    member _.``Evaluates map literals`` () =
        match Helpers.eval "{ [\"a\"] = 1; [\"b\"] = 2 }" with
        | VStringMap m ->
            m.Count |> should equal 2
            match m.["a"] with
            | VInt 1L -> ()
            | _ -> Assert.Fail("Expected key a to map to 1")
            match m.["b"] with
            | VInt 2L -> ()
            | _ -> Assert.Fail("Expected key b to map to 2")
        | _ -> Assert.Fail("Expected map value")

    [<Test>]
    member _.``Evaluates map literals with duplicate keys as last-wins`` () =
        match Helpers.eval "{ [\"a\"] = 1; [\"a\"] = 2 }" with
        | VStringMap m ->
            m.Count |> should equal 1
            match m.["a"] with
            | VInt 2L -> ()
            | _ -> Assert.Fail("Expected key a to map to 2")
        | _ -> Assert.Fail("Expected map value")

    [<Test>]
    member _.``Evaluates empty map literal`` () =
        match Helpers.eval "{}" with
        | VStringMap m -> m.Count |> should equal 0
        | _ -> Assert.Fail("Expected empty map value")

    [<Test>]
    member _.``Evaluates map literal spread with left precedence`` () =
        match Helpers.eval "let tail = { [\"a\"] = 2; [\"b\"] = 3 }\n{ [\"a\"] = 1; ..tail }" with
        | VStringMap m ->
            m.Count |> should equal 2
            match m.["a"] with
            | VInt 1L -> ()
            | _ -> Assert.Fail("Expected key a to map to 1")
            match m.["b"] with
            | VInt 3L -> ()
            | _ -> Assert.Fail("Expected key b to map to 3")
        | _ -> Assert.Fail("Expected map value")

    [<Test>]
    member _.``Evaluates map indexer lookup as option`` () =
        match Helpers.eval "let m = { [\"a\"] = 1 }\nm[\"a\"]" with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1 from map indexer")

        match Helpers.eval "let m = { [\"a\"] = 1 }\nm[\"b\"]" with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None for missing map key")

    [<Test>]
    member _.``Rejects map append via append operator`` () =
        let act () = Helpers.eval "{ [\"a\"] = 1 } @ { [\"b\"] = 2 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Evaluates record copy-update immutably`` () =
        Helpers.eval "let p = { Name = \"a\"; Age = 1 }\nlet p2 = { p with Age = 2 }\np.Age" |> assertInt 1L
        Helpers.eval "let p = { Name = \"a\"; Age = 1 }\nlet p2 = { p with Age = 2 }\np2.Age" |> assertInt 2L

    [<Test>]
    member _.``Evaluates match on list`` () =
        let src = "match [1;2] with\n    | x::xs -> x\n    | [] -> 0"
        Helpers.eval src |> assertInt 1L

    [<Test>]
    member _.``Evaluates match on non-empty list literal pattern`` () =
        let src = "match [1] with\n    | [x] -> x\n    | _ -> 0"
        Helpers.eval src |> assertInt 1L

    [<Test>]
    member _.``Evaluates match on Some with non-empty list literal pattern`` () =
        let src = "match Some [1] with\n    | Some [x] -> x\n    | _ -> 0"
        Helpers.eval src |> assertInt 1L

    [<Test>]
    member _.``Evaluates multiline map fold with match and cons`` () =
        let src =
            "let apply f x = f x\n" +
            "let f value = apply (fun item ->\n" +
            "    match item with\n" +
            "    | \"workspace:*\" -> value :: []\n" +
            "    | _ -> []) value\n" +
            "f \"workspace:*\""
        match Helpers.eval src with
        | VList [ VString "workspace:*" ] -> ()
        | _ -> Assert.Fail("Expected [\"workspace:*\"]")

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
        let src = "match Some 3 with\n    | Some x -> x + 1\n    | None -> 0"
        Helpers.eval src |> assertInt 4L

    [<Test>]
    member _.``Evaluates match on tuple`` () =
        let src = "match (1, true) with\n    | (x, true) -> x\n    | _ -> 0"
        Helpers.eval src |> assertInt 1L

    [<Test>]
    member _.``Evaluates match on record subset pattern`` () =
        let src = "let n = { Value = 1; Next = None }\nmatch n with\n    | { Value = v } -> v\n    | _ -> 0"
        Helpers.eval src |> assertInt 1L

    [<Test>]
    member _.``Evaluates match on empty map pattern`` () =
        let src = "match {} with\n    | {} -> 0\n    | _ -> 1"
        Helpers.eval src |> assertInt 0L

    [<Test>]
    member _.``Evaluates match on map cons pattern`` () =
        let src = "let m = { [\"b\"] = 2; [\"a\"] = 1 }\nmatch m with\n    | { [k] = v; ..tail } -> (k, v, tail)\n    | {} -> (\"\", 0, {})"
        match Helpers.eval src with
        | VTuple [ VString "a"; VInt 1L; VStringMap tail ] ->
            tail.Count |> should equal 1
            tail.ContainsKey "b" |> should equal true
        | _ -> Assert.Fail("Expected map cons pattern to expose head and tail")

    [<Test>]
    member _.``Evaluates match with map guard for removal`` () =
        let src =
            "let remove k m =\n" +
            "    match m with\n" +
            "    | { [key] = _; ..rest } when key = k -> rest\n" +
            "    | _ -> m\n" +
            "let m = { [\"a\"] = 1; [\"b\"] = 2 }\n" +
            "remove \"z\" m |> Map.count"
        Helpers.eval src |> assertInt 2L

    [<Test>]
    member _.``Skips guarded case when guard is false`` () =
        let src = "match [1] with\n    | x::xs when x < 0 -> x\n    | _ -> 42"
        Helpers.eval src |> assertInt 42L

    [<Test>]
    member _.``Evaluates match on discriminated union`` () =
        let src = "type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Point -> 0\n    | Circle r -> r\nradius (Circle 3)"
        Helpers.eval src |> assertInt 3L

    [<Test>]
    member _.``Evaluates qualified match on discriminated union`` () =
        let src = "type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Shape.Point -> 0\n    | Shape.Circle r -> r\nradius (Shape.Circle 3)"
        Helpers.eval src |> assertInt 3L

    [<Test>]
    member _.``Evaluates if expressions`` () =
        Helpers.eval "if true then 1 else 2" |> assertInt 1L
        Helpers.eval "if false then 1 else 2" |> assertInt 2L

    [<Test>]
    member _.``Evaluates if with elif`` () =
        Helpers.eval "if false then 1 elif true then 2 else 3" |> assertInt 2L

    [<Test>]
    member _.``Evaluates raise by aborting execution`` () =
        let act () = Helpers.eval "raise \"boom\"" |> ignore
        act |> should throw typeof<EvalException>

    [<Test>]
    member _.``Evaluates for loop as unit`` () =
        let result = Helpers.eval "for x in [1;2;3] do x |> ignore"
        match result with
        | VUnit -> ()
        | _ -> Assert.Fail("Expected unit value")

    [<Test>]
    member _.``Evaluates unit literal`` () =
        match Helpers.eval "()" with
        | VUnit -> ()
        | _ -> Assert.Fail("Expected unit value")

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
    member _.``Evaluates recursive function binding`` () =
        let src = "let rec sum n =\n    if n = 0 then 0 else n + sum (n - 1)\nsum 5"
        Helpers.eval src |> assertInt 15L

    [<Test>]
    member _.``Evaluates mutually recursive function bindings`` () =
        let src = "let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 5"
        match Helpers.eval src with
        | VBool false -> ()
        | _ -> Assert.Fail("Expected bool result from mutual recursion")

    [<Test>]
    member _.``Evaluates function application`` () =
        Helpers.eval "(fun x -> fun y -> x + y) 2 3" |> assertInt 5L

    [<Test>]
    member _.``Evaluates multi-parameter lambda expression`` () =
        Helpers.eval "(fun x y -> x + y) 2 3" |> assertInt 5L

    [<Test>]
    member _.``Evaluates function application with indented next-line arguments`` () =
        Helpers.eval "let add x y = x + y\nadd\n    1\n    2" |> assertInt 3L

    [<Test>]
    member _.``Evaluates annotated function parameters`` () =
        match Helpers.eval "type rec Node = { Value: int; Next: Node option }\nlet display_node (node: Node) = $\"{node.Value}\"\ndisplay_node { Value = 42; Next = None }" with
        | VString "42" -> ()
        | _ -> Assert.Fail("Expected annotated parameter evaluation")

    [<Test>]
    member _.``Evaluates recursive record argument with Node annotation`` () =
        match Helpers.eval "type rec Node = { Value: int; Left: Node option; Right: Node option }\nlet leaf = { Value = 1; Left = None; Right = None }\nlet root = { Value = 0; Left = Some leaf; Right = None }\nlet display_node (node: Node) = $\"{node.Value}\"\ndisplay_node root" with
        | VString "0" -> ()
        | _ -> Assert.Fail("Expected recursive Node annotation evaluation")

    [<Test>]
    member _.``Evaluates inline structural record annotation`` () =
        match Helpers.eval "let format_address (address: { City: string; Zip: int }) = $\"{address.City} ({address.Zip})\"\nformat_address { City = \"Paris\"; Zip = 75000 }" with
        | VString "Paris (75000)" -> ()
        | _ -> Assert.Fail("Expected inline structural record annotation evaluation")

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
    member _.``Reports type error for for loop with non-unit body`` () =
        let act () = Helpers.eval "for x in [1;2] do x" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports runtime error for non-exhaustive match`` () =
        let act () = Helpers.eval "match [1] with\n    | [] -> 0" |> ignore
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

    [<Test>]
    member _.``Evaluates interpolated string`` () =
        match Helpers.eval "let name = \"world\"\n$\"hello {name}\"" with
        | VString "hello world" -> ()
        | _ -> Assert.Fail("Expected interpolated string result")

    [<Test>]
    member _.``Evaluates interpolation with int placeholder`` () =
        match Helpers.eval "$\"value={1}\"" with
        | VString "value=1" -> ()
        | _ -> Assert.Fail("Expected interpolated string with int placeholder")

    [<Test>]
    member _.``Evaluates interpolation placeholder containing string literal`` () =
        match Helpers.eval "$\"{if true then \"missing\" else \"none\"}\"" with
        | VString "missing" -> ()
        | _ -> Assert.Fail("Expected interpolation with string-literal placeholder expression")

    [<Test>]
    member _.``Evaluates typeof for explicit recursive type`` () =
        match Helpers.eval "type rec Node = { Value: int; Next: Node option }\ntypeof Node" with
        | VTypeToken _ -> ()
        | _ -> Assert.Fail("Expected type token")

    [<Test>]
    member _.``Evaluates nameof for bound identifiers`` () =
        match Helpers.eval "let x = 1\nnameof x" with
        | VString "x" -> ()
        | _ -> Assert.Fail("Expected identifier name")

        match Helpers.eval "let display x = x\nnameof display" with
        | VString "display" -> ()
        | _ -> Assert.Fail("Expected function name")
