namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language

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
    member _.``Infers print as built-in function`` () =
        let typed = Helpers.infer "print \"hello\""
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TUnit
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers polymorphic identity function`` () =
        let typed = Helpers.infer "let id x = x"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, t, _, _, _) ->
            match t with
            | TFun (TVar a, TVar b) when a = b -> ()
            | _ -> Assert.Fail("Expected a -> a")
        | _ -> Assert.Fail("Expected let")

    [<Test>]
    member _.``Uses annotated record parameter for field access`` () =
        let typed =
            Helpers.infer
                "type rec Node = { Value: int; Next: Node option }\nlet display_node (node: Node) = $\"{node.Value}\""
        match typed.[1] with
        | TypeInfer.TSLet (_, _, t, _, _, _) ->
            match t with
            | TFun (TNamed "Node", TString) ->
                ()
            | _ -> Assert.Fail("Expected Node -> string function type")
        | _ -> Assert.Fail("Expected annotated function let")

    [<Test>]
    member _.``Allows structural recursive record value where Node is expected`` () =
        let typed =
            Helpers.infer
                "type rec Node = { Value: int; Left: Node option; Right: Node option }\nlet leaf = { Value = 1; Left = None; Right = None }\nlet root = { Value = 0; Left = Some leaf; Right = None }\nlet display_node (node: Node) = $\"{node.Value}\"\ndisplay_node root"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers annotated function parameter types`` () =
        let typed = Helpers.infer "let apply (f: int -> int) x = f x\napply (fun n -> n + 1) 2"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers structural inline record parameter annotation`` () =
        let typed =
            Helpers.infer
                "let format_address (address: {| City: string; Zip: int |}) = $\"{address.City} ({address.Zip})\"\nformat_address { City = \"Paris\"; Zip = 75000; Country = \"FR\" }"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers structural record literal at call site`` () =
        let typed =
            Helpers.infer
                "type OfficeAddress = { City: string; Zip: int }\nlet make_office_address (address: OfficeAddress) = address\nmake_office_address {| City = \"London\"; Zip = 12345 |}"
        match typed |> List.last with
        | TypeInfer.TSExpr te ->
            match te.Type with
            | TNamed "OfficeAddress" -> ()
            | _ -> Assert.Fail($"Expected OfficeAddress, got {te.Type}")
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Resolves declared type from inline nominal record annotation`` () =
        let typed =
            Helpers.infer
                "type Person = { Name: string }\nlet sayHello (person: { Name: string }) = person.Name\nsayHello { Name = \"Ada\" }"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for inline record annotation mismatch`` () =
        let act () =
            Helpers.infer
                "let format_address (address: { City: string; Zip: int }) = address.City\nformat_address { City = \"Paris\"; Zip = \"75000\" }"
            |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers nameof result as string`` () =
        let typed = Helpers.infer "let x = 1\nnameof x"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports error for nameof unbound identifier`` () =
        let act () = Helpers.infer "nameof missing" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers recursive function binding`` () =
        let typed = Helpers.infer "let rec sum n =\n    if n = 0 then 0 else n + sum (n - 1)\nsum 5"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers mutually recursive function bindings`` () =
        let src = "let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 4"
        let typed = Helpers.infer src
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TBool
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
    member _.``Infers unit literal`` () =
        let typed = Helpers.infer "()"
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
    member _.``Infers match on non-empty list literal pattern`` () =
        let typed = Helpers.infer "match [1] with\n    | [x] -> x\n    | _ -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on Some with non-empty list literal pattern`` () =
        let typed = Helpers.infer "match Some [1] with\n    | Some [x] -> x\n    | _ -> 0"
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
    member _.``Infers match on record subset pattern`` () =
        let typed =
            Helpers.infer
                "type rec Node = { Value: int; Next: Node option }\nlet n = { Value = 1; Next = None }\nmatch n with\n    | { Value = v } -> v\n    | _ -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on empty map pattern`` () =
        let typed =
            Helpers.infer
                "match {} with\n    | {} -> 0\n    | _ -> 1"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on map cons pattern`` () =
        let typed =
            Helpers.infer
                "let m = { [\"a\"] = 1 }\nmatch m with\n    | { [k] = v; ..tail } -> (k, v)\n    | {} -> (\"\", 0)"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TString; TInt ])
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match on map literal-key pattern without tail`` () =
        let typed =
            Helpers.infer
                "let m = { [\"a\"] = 1; [\"b\"] = 2 }\nmatch m with\n    | { [\"a\"] = x; [\"b\"] = y } -> x + y\n    | _ -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers match case guard`` () =
        let typed =
            Helpers.infer
                "let m = { [\"a\"] = 1 }\nmatch m with\n    | { [k] = v; ..tail } when k = \"a\" -> v\n    | _ -> 0"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for non-bool guard`` () =
        let act () =
            Helpers.infer
                "match [1] with\n    | x::xs when x -> x\n    | _ -> 0"
            |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for unknown record field in pattern`` () =
        let act () =
            Helpers.infer
                "type rec Node = { Value: int; Next: Node option }\nlet n = { Value = 1; Next = None }\nmatch n with\n    | { Missing = v } -> v\n    | _ -> 0"
            |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for record pattern on non-record scrutinee`` () =
        let act () = Helpers.infer "match 1 with | { Value = v } -> v | _ -> 0" |> ignore
        act |> should throw typeof<TypeException>

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
    member _.``Reports type error for annotation mismatch`` () =
        let act () = Helpers.infer "let f (x: int) = x + true" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers multiline map fold with match and cons`` () =
        let typed =
            Helpers.infer
                "let apply f x = f x\nlet f value = apply (fun item ->\n    match item with\n    | \"workspace:*\" -> value :: []\n    | _ -> []) value\nf"
        match typed |> List.last with
        | TypeInfer.TSExpr te ->
            match te.Type with
            | TFun (TString, TList TString) -> ()
            | _ -> Assert.Fail($"Expected string -> string list, got {Types.typeToString te.Type}")
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers annotated lambda field access inside Option.map`` () =
        let typed =
            Helpers.infer
                "type Package = { name: string }\nlet get_name = fun (value: Package) -> value.name\nget_name"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TFun (TNamed "Package", TString))
        | _ -> Assert.Fail("Expected expression")

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
    member _.``Infers match on discriminated union`` () =
        let src = "type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Point -> 0\n    | Circle r -> r\nradius (Circle 3)"
        let typed = Helpers.infer src
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers qualified discriminated union constructor and match`` () =
        let src = "type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Shape.Point -> 0\n    | Shape.Circle r -> r\nradius (Shape.Circle 3)"
        let typed = Helpers.infer src
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for union payload type mismatch`` () =
        let act () =
            Helpers.infer "type Shape = | Point | Circle of int\nCircle true" |> ignore
        act |> should throw typeof<TypeException>

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
    member _.``Infers map literal type`` () =
        let typed = Helpers.infer "{ [\"a\"] = 1; [\"b\"] = 2 }"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap (TString, TInt))
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers map literal type with int keys`` () =
        let typed = Helpers.infer "{ [1] = 2; [3] = 4 }"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap (TInt, TInt))
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for unsupported map key type`` () =
        let act () = Helpers.infer "{ [true] = 2 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers map literal with string key expression`` () =
        let typed = Helpers.infer "let key = \"a\"\n{ [key] = 1 }"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap (TString, TInt))
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers map literal with spread`` () =
        let typed = Helpers.infer "let tail = { [\"b\"] = 2 }\n{ [\"a\"] = 1; ..tail }"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap (TString, TInt))
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers empty map through int-keyed usage`` () =
        let typed = Helpers.infer "let empty = {}\nlet f x m = Map.add 1 x m\nf 42 empty"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap (TInt, TInt))
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for mixed map literal values`` () =
        let act () = Helpers.infer "{ [\"a\"] = 1; [\"b\"] = \"x\" }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for map append with append operator`` () =
        let act () = Helpers.infer "{ [\"a\"] = 1 } @ { [\"b\"] = 2 }" |> ignore
        act |> should throw typeof<TypeException>

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
