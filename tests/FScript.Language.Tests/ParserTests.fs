namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language

[<TestFixture>]
type ParserTests () =
    [<Test>]
    member _.``Parses top-level let binding`` () =
        let program = Helpers.parse "let x = 1"
        program.Length |> should equal 1
        match program.Head with
        | SLet (name, args, _, _, _, _) ->
            name |> should equal "x"
            args.Length |> should equal 0
        | _ -> Assert.Fail("Expected top-level let")

    [<Test>]
    member _.``Parses top-level function binding with arguments`` () =
        let program = Helpers.parse "let id x = x"
        match program.Head with
        | SLet ("id", args, _, _, _, _) -> args.Length |> should equal 1
        | _ -> Assert.Fail("Expected function let")

    [<Test>]
    member _.``Parses recursive type declaration`` () =
        let program = Helpers.parse "type rec Node = { Value: int; Left: Node option; Right: Node option }"
        match program.Head with
        | SType def ->
            def.Name |> should equal "Node"
            def.IsRecursive |> should equal true
            def.Fields.Length |> should equal 3
        | _ -> Assert.Fail("Expected recursive type declaration")

    [<Test>]
    member _.``Parses multiline type declaration`` () =
        let src = "type rec Node =\n    { Value: int\n      Left: Node option\n      Right: Node option }"
        let program = Helpers.parse src
        match program.Head with
        | SType def ->
            def.Name |> should equal "Node"
            def.IsRecursive |> should equal true
            def.Fields.Length |> should equal 3
        | _ -> Assert.Fail("Expected multiline type declaration")

    [<Test>]
    member _.``Parses discriminated union declaration`` () =
        let src = "type Shape =\n    | Point\n    | Circle of int"
        let program = Helpers.parse src
        match program.Head with
        | SType def ->
            def.Name |> should equal "Shape"
            def.Cases.Length |> should equal 2
            def.Cases.[0] |> should equal ("Point", None)
        | _ -> Assert.Fail("Expected union type declaration")

    [<Test>]
    member _.``Rejects misaligned multiline type declaration fields`` () =
        let act () =
            Helpers.parse "type Person =\n    { Name: string\n       Age: int }" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses list literals`` () =
        let p1 = Helpers.parse "[1; 2]"
        match p1.[0] with
        | SExpr (EList (items, _)) -> items.Length |> should equal 2
        | _ -> Assert.Fail("Expected list literal")

    [<Test>]
    member _.``Parses compact multiline list literal`` () =
        let p = Helpers.parse "[1\n 2\n 3]"
        match p.[0] with
        | SExpr (EList (items, _)) -> items.Length |> should equal 3
        | _ -> Assert.Fail("Expected compact multiline list literal")

    [<Test>]
    member _.``Parses block multiline list literal`` () =
        let p = Helpers.parse "[\n 1\n 2\n 3\n]"
        match p.[0] with
        | SExpr (EList (items, _)) -> items.Length |> should equal 3
        | _ -> Assert.Fail("Expected block multiline list literal")

    [<Test>]
    member _.``Rejects multiline list literal with '[' kept on assignment line`` () =
        let act () = Helpers.parse "let x = [\n 1\n 2\n]" |> ignore
        act |> should throw typeof<ParseException>

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
    member _.``Parses unit literal`` () =
        let p = Helpers.parse "()"
        match p.[0] with
        | SExpr (EUnit _) -> ()
        | _ -> Assert.Fail("Expected unit literal")

    [<Test>]
    member _.``Parses record literal and field access`` () =
        let p = Helpers.parse "{ Name = \"a\"; Age = 1 }.Age"
        match p.[0] with
        | SExpr (EFieldGet (ERecord (fields, _), "Age", _)) ->
            fields.Length |> should equal 2
        | _ -> Assert.Fail("Expected record field access")

    [<Test>]
    member _.``Parses multiline record literal compact braces`` () =
        let p = Helpers.parse "{ Name = \"a\"\n  Age = 1 }"
        match p.[0] with
        | SExpr (ERecord (fields, _)) -> fields.Length |> should equal 2
        | _ -> Assert.Fail("Expected multiline record literal")

    [<Test>]
    member _.``Parses multiline record literal block braces`` () =
        let p = Helpers.parse "{\n  Name = \"a\"\n  Age = 1\n}"
        match p.[0] with
        | SExpr (ERecord (fields, _)) -> fields.Length |> should equal 2
        | _ -> Assert.Fail("Expected multiline block record literal")

    [<Test>]
    member _.``Rejects multiline record literal with '{' kept on assignment line`` () =
        let act () = Helpers.parse "let x = {\n  Name = \"a\"\n  Age = 1\n}" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses record copy-update expression`` () =
        let p = Helpers.parse "{ p with Age = 2 }"
        match p.[0] with
        | SExpr (ERecordUpdate (EVar ("p", _), updates, _)) ->
            updates.Length |> should equal 1
        | _ -> Assert.Fail("Expected record update expression")

    [<Test>]
    member _.``Parses multiline record copy-update expression`` () =
        let p = Helpers.parse "{ p with Age = 2\n  Name = \"n\" }"
        match p.[0] with
        | SExpr (ERecordUpdate (EVar ("p", _), updates, _)) -> updates.Length |> should equal 2
        | _ -> Assert.Fail("Expected multiline record update expression")

    [<Test>]
    member _.``Parses map literal`` () =
        let p = Helpers.parse "{ [\"a\"] = 1; [\"b\"] = 2 }"
        match p.[0] with
        | SExpr (EMap (entries, _)) ->
            entries.Length |> should equal 2
        | _ -> Assert.Fail("Expected map literal")

    [<Test>]
    member _.``Parses multiline map literal`` () =
        let src = "{\n    [\"a\"] = 1\n    [\"b\"] = 2\n}"
        let p = Helpers.parse src
        match p.[0] with
        | SExpr (EMap (entries, _)) ->
            entries.Length |> should equal 2
        | _ -> Assert.Fail("Expected multiline map literal")

    [<Test>]
    member _.``Rejects multiline map literal with '{' kept on assignment line`` () =
        let act () = Helpers.parse "let x = {\n  [\"a\"] = 1\n  [\"b\"] = 2\n}" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses compact multiline map literal`` () =
        let src = "{ [\"a\"] = 1\n  [\"b\"] = 2 }"
        let p = Helpers.parse src
        match p.[0] with
        | SExpr (EMap (entries, _)) -> entries.Length |> should equal 2
        | _ -> Assert.Fail("Expected compact multiline map literal")

    [<Test>]
    member _.``Parses empty map literal`` () =
        let p = Helpers.parse "{}"
        match p.[0] with
        | SExpr (EMap (entries, _)) -> entries.Length |> should equal 0
        | _ -> Assert.Fail("Expected empty map literal")

    [<Test>]
    member _.``Parses map literal with key expression`` () =
        let p = Helpers.parse "let a = \"x\"\n{ [a] = 1 }"
        match p.[1] with
        | SExpr (EMap (entries, _)) -> entries.Length |> should equal 1
        | _ -> Assert.Fail("Expected map literal with expression key")

    [<Test>]
    member _.``Parses let expression without in`` () =
        let p = Helpers.parse "let x = (let y = 1\n    y + 1\n)"
        match p.[0] with
        | SLet (_, _, EParen (ELet ("y", _, _, _, _), _), _, _, _) -> ()
        | _ -> Assert.Fail("Expected nested let expression")

    [<Test>]
    member _.``Parses top-level recursive function binding`` () =
        let program = Helpers.parse "let rec fib n = if n < 2 then n else fib (n - 1)"
        match program.Head with
        | SLet ("fib", args, _, true, _, _) ->
            args.Length |> should equal 1
        | _ -> Assert.Fail("Expected recursive function let")

    [<Test>]
    member _.``Parses top-level mutually recursive function bindings`` () =
        let src = "let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)"
        let program = Helpers.parse src
        match program.Head with
        | SLetRecGroup (bindings, _, _) -> bindings.Length |> should equal 2
        | _ -> Assert.Fail("Expected recursive let group")

    [<Test>]
    member _.``Parses lambda expression`` () =
        let p = Helpers.parse "fun x -> x"
        match p.[0] with
        | SExpr (ELambda ({ Name = "x"; Annotation = None }, _, _)) -> ()
        | _ -> Assert.Fail("Expected lambda")

    [<Test>]
    member _.``Parses multi-parameter lambda expression`` () =
        let p = Helpers.parse "fun x y -> x + y"
        match p.[0] with
        | SExpr (ELambda ({ Name = "x"; Annotation = None }, ELambda ({ Name = "y"; Annotation = None }, _, _), _)) -> ()
        | _ -> Assert.Fail("Expected curried lambda")

    [<Test>]
    member _.``Parses function application with indented next-line argument`` () =
        let p = Helpers.parse "Map.ofList\n    [(\"a\", 1)]"
        match p.[0] with
        | SExpr (EApply (EFieldGet (EVar ("Map", _), "ofList", _), EList _, _)) -> ()
        | _ -> Assert.Fail("Expected multiline function application")

    [<Test>]
    member _.``Parses annotated let parameter`` () =
        let p = Helpers.parse "let show (node: Node) = node"
        match p.[0] with
        | SLet ("show", [ { Name = "node"; Annotation = Some (TRName "Node") } ], _, _, _, _) -> ()
        | _ -> Assert.Fail("Expected annotated let parameter")

    [<Test>]
    member _.``Parses multiline let parameters with aligned columns and inline body`` () =
        let src = "let format_address (address: { City: string; Zip: int })\n                   (name: string) = $\"{address.City} ({name})\""
        let p = Helpers.parse src
        match p.[0] with
        | SLet ("format_address", args, _, _, _, _) -> args.Length |> should equal 2
        | _ -> Assert.Fail("Expected multiline let parameters")

    [<Test>]
    member _.``Parses multiline let parameters with aligned columns and block body`` () =
        let src = "let format_address (address: { City: string; Zip: int })\n                   (name: string) =\n    $\"{address.City} ({name})\""
        let p = Helpers.parse src
        match p.[0] with
        | SLet ("format_address", args, _, _, _, _) -> args.Length |> should equal 2
        | _ -> Assert.Fail("Expected multiline let parameters with block body")

    [<Test>]
    member _.``Rejects multiline let parameters when columns do not align`` () =
        let act () =
            Helpers.parse "let format_address (address: { City: string; Zip: int })\n                  (name: string) = $\"{address.City} ({name})\"" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses annotated lambda parameter with function type`` () =
        let p = Helpers.parse "fun (f: int -> string) -> f 1"
        match p.[0] with
        | SExpr (ELambda ({ Name = "f"; Annotation = Some (TRFun (TRName "int", TRName "string")) }, _, _)) -> ()
        | _ -> Assert.Fail("Expected annotated lambda parameter with function type")

    [<Test>]
    member _.``Parses annotated parameter with inline record type`` () =
        let p = Helpers.parse "let format_address (address: { City: string; Zip: int }) = address.City"
        match p.[0] with
        | SLet ("format_address", [ { Name = "address"; Annotation = Some (TRRecord [ ("City", TRName "string"); ("Zip", TRName "int") ]) } ], _, _, _, _) -> ()
        | _ -> Assert.Fail("Expected annotated let parameter with inline record type")

    [<Test>]
    member _.``Rejects multiline inline record type annotation`` () =
        let act () = Helpers.parse "let format_address (address: {\n    City: string\n    Zip: int\n}) = address.City" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses nameof expression`` () =
        let p = Helpers.parse "let x = 1\nnameof x"
        match p.[1] with
        | SExpr (ENameOf ("x", _)) -> ()
        | _ -> Assert.Fail("Expected nameof expression")

    [<Test>]
    member _.``Rejects malformed inline record type annotation`` () =
        let act () = Helpers.parse "let format_address (address: { City string; Zip: int }) = address.City" |> ignore
        act |> should throw typeof<ParseException>

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
    member _.``Parses match with non-empty list literal pattern`` () =
        let src = "match [1] with\n| [x] -> x\n| _ -> 0"
        let program = Helpers.parse src
        match program.[0] with
        | SExpr (EMatch (_, (PCons (_, PNil _, _), _, _) :: _, _)) -> ()
        | _ -> Assert.Fail("Expected non-empty list pattern")

    [<Test>]
    member _.``Parses Some with non-empty list literal pattern`` () =
        let src = "match Some [1] with\n| Some [x] -> x\n| _ -> 0"
        let program = Helpers.parse src
        match program.[0] with
        | SExpr (EMatch (_, (PSome (PCons (_, PNil _, _), _), _, _) :: _, _)) -> ()
        | _ -> Assert.Fail("Expected Some with non-empty list pattern")

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
    member _.``Rejects tuple match case without parentheses`` () =
        let act () = Helpers.parse "match (1, true) with\n    | x, true -> x\n    | _ -> 0" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Parses match with record patterns`` () =
        let src = "match { Value = 1; Next = None } with\n    | { Value = v } -> v\n    | _ -> 0"
        let program = Helpers.parse src
        match program.[0] with
        | SExpr (EMatch (_, (PRecord (_, _), _, _) :: _, _)) -> ()
        | _ -> Assert.Fail("Expected record pattern in match")

    [<Test>]
    member _.``Parses match with union patterns`` () =
        let src = "type Result = | Ok of int | Error\nmatch Ok 1 with\n    | Ok x -> x\n    | Error -> 0"
        let program = Helpers.parse src
        match program.[1] with
        | SExpr (EMatch (_, (PUnionCase ("Ok", Some (PVar ("x", _)), _), _, _) :: _, _)) -> ()
        | _ -> Assert.Fail("Expected union case pattern in match")

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
        | SLet ("f", [_], ELet _, false, _, _) -> ()
        | _ -> Assert.Fail("Expected block-desugared let")

    [<Test>]
    member _.``Parses multiline lambda argument closed by parenthesis on same line`` () =
        let src = "Map.fold (fun acc key value ->\n    match value with\n    | \"workspace:*\" -> key :: acc\n    | _ -> acc) [] { [\"a\"] = \"workspace:*\" }"
        let p = Helpers.parse src
        match p.[0] with
        | SExpr (EApply (EApply (EApply (EFieldGet (EVar ("Map", _), "fold", _), _, _), _, _), _, _)) -> ()
        | _ -> Assert.Fail("Expected multiline lambda argument application")

    [<Test>]
    member _.``Parses pipeline continuation after multiline lambda argument`` () =
        let src = "let main =\n    [0..9]\n    |> List.map (fun i ->\n        i)\n    |> List.iter print"
        let p = Helpers.parse src
        match p.[0] with
        | SLet ("main", [], EBinOp ("|>", EBinOp ("|>", _, _, _), _, _), false, _, _) -> ()
        | _ -> Assert.Fail("Expected pipeline continuation after multiline lambda argument")

    [<Test>]
    member _.``Parses pipeline continuation after multiline lambda with nested lambda`` () =
        let src = "let main =\n    [0..9]\n    |> List.map (fun i ->\n        i |> fib |> fun x -> $\"{x}\")\n    |> List.iter print"
        let p = Helpers.parse src
        match p.[0] with
        | SLet ("main", [], EBinOp ("|>", EBinOp ("|>", _, _, _), _, _), false, _, _) -> ()
        | _ -> Assert.Fail("Expected pipeline continuation after multiline nested lambda")

    [<Test>]
    member _.``Reports indentation error for misindented multiline lambda pipeline`` () =
        let src = "let main =\n    [0..9]\n    |> List.map (fun i ->\n        i\n         |> fib |> fun x -> $\"{x}\")\n    |> List.iter print"
        try
            Helpers.parse src |> ignore
            Assert.Fail("Expected parse exception")
        with
        | ParseException err ->
            Assert.That(err.Message, Does.StartWith("Indentation error"))

    [<Test>]
    member _.``Parses exported top-level let binding with attribute`` () =
        let p = Helpers.parse "[<export>] let cosine x = x"
        match p.[0] with
        | SLet ("cosine", [_], _, false, true, _) -> ()
        | _ -> Assert.Fail("Expected exported top-level let binding")

    [<Test>]
    member _.``Parses exported top-level recursive let group with attribute`` () =
        let p = Helpers.parse "[<export>] let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)"
        match p.[0] with
        | SLetRecGroup (bindings, true, _) -> bindings.Length |> should equal 2
        | _ -> Assert.Fail("Expected exported recursive let group")

    [<Test>]
    member _.``Parses interpolated string`` () =
        let p = Helpers.parse "$\"hello {name}\""
        match p.[0] with
        | SExpr (EInterpolatedString ([ IPText "hello "; IPExpr (EVar ("name", _)) ], _)) -> ()
        | _ -> Assert.Fail("Expected interpolated string")

    [<Test>]
    member _.``Parses interpolated placeholder containing string literal`` () =
        let p = Helpers.parse "$\"{if true then \"missing\" else \"none\"}\""
        match p.[0] with
        | SExpr (EInterpolatedString ([ IPExpr _ ], _)) -> ()
        | _ -> Assert.Fail("Expected interpolated placeholder expression")

    [<Test>]
    member _.``Parses mutually recursive let expression`` () =
        let src = "(let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 4\n)"
        let p = Helpers.parse src
        match p.[0] with
        | SExpr (EParen (ELetRecGroup (bindings, _, _), _)) -> bindings.Length |> should equal 2
        | _ -> Assert.Fail("Expected recursive let-expression group")

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

    [<Test>]
    member _.``Rejects and binding without function argument`` () =
        let act () = Helpers.parse "let rec f x = x\nand g = 1" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects bare parameter annotation syntax`` () =
        let act () = Helpers.parse "let f x: int = x" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects duplicate fields in record pattern`` () =
        let act () = Helpers.parse "match { Value = 1 } with | { Value = a; Value = b } -> a | _ -> 0" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects export let syntax`` () =
        let act () = Helpers.parse "export let y = 1" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects wrong attribute casing`` () =
        let act () = Helpers.parse "[<Export>] let y = 1" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Rejects export attribute in nested expression block`` () =
        let act () = Helpers.parse "(let x =\n    [<export>] let y = 1\n    y\n)" |> ignore
        act |> should throw typeof<ParseException>
