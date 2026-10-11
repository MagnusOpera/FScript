namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type TypeInferenceTests () =
    [<Test>]
    member _.``Allocates unique type variables for parallel callers`` () =
        let variables =
            Array.Parallel.init 100_000 (fun _ ->
                match Types.freshVar () with
                | TVar id -> id
                | _ -> failwith "Expected a type variable")

        variables |> Array.distinct |> Array.length |> should equal variables.Length

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
    member _.``Infers Console.writeLine as injected extern`` () =
        let host = { RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let externs = Registry.all host
        let typed = Helpers.inferWithExterns externs "Console.writeLine \"hello\""
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TUnit
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers String builtins`` () =
        let assertType src expected =
            match Helpers.infer src |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal expected
            | _ -> Assert.Fail("Expected expression")

        assertType "String.replace \"a\" \"a\" \"b\"" TString
        assertType "String.indexOf \"b\" \"abc\"" (TOption TInt)
        assertType "String.toLower \"ABC\"" TString
        assertType "String.toUpper \"abc\"" TString
        assertType "String.substring 1 2 \"abcdef\"" (TOption TString)
        assertType "String.concat \",\" [\"a\";\"b\"]" TString
        assertType "String.split \",\" \"a,b\"" (TList TString)
        assertType "String.endsWith \".b\" \"a.b\"" TBool

    [<Test>]
    member _.``Reports type error for invalid String.concat argument type`` () =
        let act () = Helpers.infer "String.concat \",\" [1;2]" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers polymorphic identity function`` () =
        let typed = Helpers.infer "let id x = x"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, t, _, _, _) ->
            match t with
            | TFun (TVar a, TVar b) when a = b -> ()
            | _ -> Assert.Fail("Expected a -> a")
        | _ -> Assert.Fail("Expected let")

    [<TestCase("+")>]
    [<TestCase("-")>]
    [<TestCase("*")>]
    [<TestCase("/")>]
    member _.``Arithmetic functions preserve a shared numeric operand and result type`` (op: string) =
        let typed = Helpers.infer $"let calculate x y = x {op} y\n(calculate 1 2, calculate 1.5 2.6)"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, TFun (TNumeric a, TFun (TNumeric b, TNumeric result)), _, _, _) ->
            a |> should equal b
            a |> should equal result
        | _ -> Assert.Fail("Expected numeric -> numeric -> numeric")
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TInt; TFloat ])
        | _ -> Assert.Fail("Expected tuple expression")

    [<Test>]
    member _.``Numeric constraints reject unsupported polymorphic applications`` () =
        for argument in [ "true"; "\"text\""; "[1]"; "Some 1"; "{ Value = 1 }"; "fun x -> x" ] do
            let source = $"let add x y = x + y\nadd ({argument}) ({argument})"
            (fun () -> Helpers.infer source |> ignore) |> should throw typeof<TypeException>

    [<Test>]
    member _.``Numeric constraints survive aliases higher order functions and nested lets`` () =
        for binding in [ "let sum = add"; "let sum = (fun f -> f) add"; "let sum x y =\n    let inner a b = a + b\n    inner x y" ] do
            let prefix = $"let add x y = x + y\n{binding}\n"
            match Helpers.infer (prefix + "(sum 1 2, sum 1.5 2.6)") |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TInt; TFloat ])
            | _ -> Assert.Fail("Expected tuple expression")
            (fun () -> Helpers.infer (prefix + "sum true false") |> ignore) |> should throw typeof<TypeException>

    [<Test>]
    member _.``Numeric variables remain independent unless unified`` () =
        let typed = Helpers.infer "let calculate x y = (x + x, y + y)\ncalculate 1 2.6"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TInt; TFloat ])
        | _ -> Assert.Fail("Expected tuple expression")

    [<Test>]
    member _.``Numeric constraints on captured parameters are not generalized`` () =
        let source = "let calculate x =\n    let addY y = x + y\n    (addY 1, addY 1.5)"
        (fun () -> Helpers.infer source |> ignore) |> should throw typeof<TypeException>

    [<Test>]
    member _.``Recursive functions preserve numeric constraints`` () =
        let prefix = "let rec sum stop x y = if stop then x + y else sum true x y\n"
        match Helpers.infer (prefix + "(sum true 1 2, sum false 1.5 2.6)") |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TInt; TFloat ])
        | _ -> Assert.Fail("Expected tuple expression")
        (fun () -> Helpers.infer (prefix + "sum true true false") |> ignore) |> should throw typeof<TypeException>

    [<Test>]
    member _.``Logical and numeric constraints cannot share an operand`` () =
        (fun () -> Helpers.infer "let incompatible x = (x && x, x + x)" |> ignore)
        |> should throw typeof<TypeException>

    [<Test>]
    member _.``Concrete numeric annotations specialize all addition parameters`` () =
        let typed = Helpers.infer "let add x (y: int) = x + y\nadd 1 2"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, t, _, _, _) -> t |> should equal (TFun (TInt, TFun (TInt, TInt)))
        | _ -> Assert.Fail("Expected function binding")
        (fun () -> Helpers.infer "let add x (y: int) = x + y\nadd 1.5 2" |> ignore)
        |> should throw typeof<TypeException>

    [<Test>]
    member _.``Numeric annotations specialize independently on each application`` () =
        let source = "let add (x: numeric) (y: numeric): numeric = x + y\n(add 1 2, add 1.5 2.6)"
        match Helpers.infer source |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TInt; TFloat ])
        | _ -> Assert.Fail("Expected tuple expression")
        (fun () -> Helpers.infer "let id (x: numeric) = x\nid true" |> ignore)
        |> should throw typeof<TypeException>

    [<Test>]
    member _.``Numeric comparison functions reject nonnumeric applications`` () =
        let prefix = "let less x y = x < y\n"
        match Helpers.infer (prefix + "(less 1 2, less 1.5 2.6)") |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TBool; TBool ])
        | _ -> Assert.Fail("Expected tuple expression")
        (fun () -> Helpers.infer (prefix + "less true false") |> ignore) |> should throw typeof<TypeException>

    [<Test>]
    member _.``Remainder specializes to integers matching runtime support`` () =
        let typed = Helpers.infer "let remainder x y = x % y\nremainder 5 2"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, t, _, _, _) -> t |> should equal (TFun (TInt, TFun (TInt, TInt)))
        | _ -> Assert.Fail("Expected function binding")
        (fun () -> Helpers.infer "1.5 % 2.6" |> ignore) |> should throw typeof<TypeException>

    [<Test>]
    member _.``Polymorphic addition evaluates integer and float applications`` () =
        Helpers.evalToString "let add x y = x + y\n(add 1 2, add 1.5 2.6)"
        |> should equal "(3, 4.1)"

    [<Test>]
    member _.``Arithmetic with a concrete operand specializes the function`` () =
        let typed = Helpers.infer "let increment x = x + 1.5\nincrement 2.6"
        match typed.[0] with
        | TypeInfer.TSLet (_, _, t, _, _, _) -> t |> should equal (TFun (TFloat, TFloat))
        | _ -> Assert.Fail("Expected function binding")

    [<Test>]
    member _.``Arithmetic still rejects mixed operands and concrete nonnumeric operands`` () =
        for source in [ "let add x y = x + y\nadd 1 2.6"; "true + false"; "\"a\" + \"b\"" ] do
            (fun () -> Helpers.infer source |> ignore) |> should throw typeof<TypeException>

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
    member _.``Infers function with annotated return type`` () =
        let typed = Helpers.infer "let increment x : int = x + 1\nincrement 2"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers structural inline record parameter annotation`` () =
        let typed =
            Helpers.infer
                "let format_address (address: {| City: string; Zip: int |}) = $\"{address.City} ({address.Zip})\"\nformat_address {| City = \"Paris\"; Zip = 75000; Country = \"FR\" |}"
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
                "let format_address (address: { City: string; Zip: int }) = address.City\nformat_address {| City = \"Paris\"; Zip = \"75000\" |}"
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
    member _.``Infers list and map indexer types`` () =
        match Helpers.infer "[1;2;3][0]" |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TOption TInt)
        | _ -> Assert.Fail("Expected expression")

        match Helpers.infer "let m = { [\"a\"] = 1 }\nm[\"a\"]" |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TOption TInt)
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Rejects incompatible indexer key types`` () =
        let listAct () = Helpers.infer "[1;2;3][\"0\"]" |> ignore
        listAct |> should throw typeof<TypeException>

        let mapAct () = Helpers.infer "let m = { [\"a\"] = 1 }\nm[0]" |> ignore
        mapAct |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers tuple type`` () =
        let typed = Helpers.infer "(1, true, \"x\")"
        match typed |> List.last with
        | TypeInfer.TSExpr te ->
            te.Type |> should equal (TTuple [ TInt; TBool; TString ])
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers tuple let binding names`` () =
        let typed = Helpers.infer "let (a, b) = (1, true)\n(a, b)"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTuple [ TInt; TBool ])
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Reports type error for tuple let mismatch`` () =
        let act () = Helpers.infer "let (a, b) = 1\na" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers record type and field projection`` () =
        let typed = Helpers.infer "type Person = { Name: string; Age: int }\nlet p = { Name = \"a\"; Age = 1 }\np.Age"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers record copy-update`` () =
        let typed = Helpers.infer "type Person = { Name: string; Age: int }\nlet p = { Name = \"a\"; Age = 1 }\nlet p2 = { p with Age = 2 }\np2.Age"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers structural record copy-update`` () =
        let typed = Helpers.infer "let p = {| Name = \"a\"; Age = 1 |}\nlet p2 = {| p with Age = 2 |}\np2.Age"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers structural record copy-update with new fields`` () =
        let typed = Helpers.infer "let home = {| City = \"Paris\"; Zip = 75000 |}\nlet home = {| home with Name = \"toto\"\n                        Country = \"toto\" |}\nhome.Name"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
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
    member _.``Infers local variable types for map pattern bindings`` () =
        let src =
            "let scores = { [\"a\"] = 1; [\"b\"] = 2 }\n" +
            "let mapPreview =\n" +
            "    match scores with\n" +
            "    | {} ->\n" +
            "        \"empty\"\n" +
            "    | { [subject] = score; ..remaining } ->\n" +
            "        $\"{subject}:{score}:{Map.count remaining}\""

        let program = Helpers.parse src
        let _, locals = TypeInfer.inferProgramWithLocalVariableTypes program

        let typesFor name =
            locals
            |> List.filter (fun entry -> entry.Name = name && entry.Span.Start.File.IsNone)
            |> List.map (fun entry -> entry.Type)

        let subjectTypes = typesFor "subject"
        let scoreTypes = typesFor "score"
        let remainingTypes = typesFor "remaining"

        subjectTypes |> should not' (be Empty)
        scoreTypes |> should not' (be Empty)
        remainingTypes |> should not' (be Empty)

        subjectTypes |> List.iter (fun t -> t |> should equal TString)
        scoreTypes |> List.iter (fun t -> t |> should equal TInt)
        remainingTypes |> List.iter (fun t -> t |> should equal (TMap (TString, TInt)))

    [<Test>]
    member _.``Ordinary inference and local capture produce the same concrete result`` () =
        let program =
            Helpers.parse
                "let transform value =\n    let identity item = item\n    let pair = (identity value, identity value)\n    pair\ntransform 42"

        let ordinary = TypeInfer.inferProgram program
        let captured, locals = TypeInfer.inferProgramWithLocalVariableTypes program

        let lastType typed =
            match typed |> List.last with
            | TypeInfer.TSExpr expression -> expression.Type
            | _ -> failwith "Expected expression"

        lastType ordinary |> should equal (TTuple [ TInt; TInt ])
        lastType captured |> should equal (TTuple [ TInt; TInt ])
        locals |> should not' (be Empty)

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
        let act () = Helpers.infer "type Person = { Name: string; Age: int }\nlet p = { Name = \"a\"; Age = 1 }\n{ p with Missing = 2 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for undeclared plain record literal`` () =
        let act () = Helpers.infer "let home = { City = \"Paris\"; Zip = 75000; Name = \"toto\" }\nhome" |> ignore
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
    member _.``Reports type error for return type annotation mismatch`` () =
        let act () = Helpers.infer "let f x : string = x + 1" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Reports type error for recursive return annotation mismatch`` () =
        let act () =
            Helpers.infer "let rec even n : int = if n = 0 then true else odd (n - 1)\nand odd n : int = if n = 0 then false else even (n - 1)\neven 4"
            |> ignore
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
    member _.``Infers unit-parameter functions`` () =
        let typed = Helpers.infer "let ping () = 1\nping ()"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TInt
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
    member _.``Infers int bool and payload-free union map keys`` () =
        let cases =
            [ "{ [1] = 2; [3] = 4 }", TInt
              "{ [true] = 2; [false] = 4 }", TBool
              "type Color = | Red | Blue\n{ [Red] = 2; [Blue] = 4 }", TNamed "Color" ]
        for source, keyType in cases do
            match Helpers.infer source |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal (TMap(keyType, TInt))
            | _ -> Assert.Fail("Expected map expression")

    [<Test>]
    member _.``Reports type error for unsupported map key type`` () =
        let act () = Helpers.infer "{ [1.5] = 2 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Rejects payload-bearing union map keys`` () =
        let act () = Helpers.infer "type Color = | Red | Blue of int\n{ [Red] = 1 }" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Infers explicitly annotated non-string maps`` () =
        let source = "type Color = | Red | Blue\nlet get (m: int map[Color]) = m[Red]\nget { [Red] = 5 }"
        match Helpers.infer source |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TOption TInt)
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Infers unannotated map function arguments from keys`` () =
        let cases =
            [ "type Color = | Red | Blue\nlet get m = m[Red]\nget { [Red] = 5 }"
              "let get m = m[true]\nget { [true] = 5 }"
              "let get m = Map.tryGet 1 m\nget { [1] = 5 }" ]
        for source in cases do
            match Helpers.infer source |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal (TOption TInt)
            | _ -> Assert.Fail("Expected expression")

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
    member _.``Infers empty map through string-keyed usage`` () =
        let typed = Helpers.infer "let empty = {}\nlet f x m = Map.add \"a\" x m\nf 42 empty"
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap (TString, TInt))
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
    member _.``Infers task spawn and await`` () =
        let host = { RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let externs = Registry.all host

        match Helpers.inferWithExterns externs "Task.spawn (fun _ -> 1)" |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TTask TInt)
        | _ -> Assert.Fail("Expected task expression")

        match Helpers.inferWithExterns externs "Task.await (Task.spawn (fun _ -> \"x\"))" |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected awaited expression")

    [<Test>]
    member _.``Accepts task type annotation`` () =
        let host = { RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let externs = Registry.all host
        let typed = Helpers.inferWithExterns externs "let await_int (t: int task) = Task.await t"
        match typed.Head with
        | TypeInfer.TSLet (_, _, t, _, _, _) ->
            t |> should equal (TFun (TTask TInt, TInt))
        | _ -> Assert.Fail("Expected let binding")

    [<Test>]
    member _.``Rejects invalid task builtin arguments`` () =
        let host = { RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let externs = Registry.all host

        let spawnAct () = Helpers.inferWithExterns externs "Task.spawn 1" |> ignore
        spawnAct |> should throw typeof<TypeException>

        let awaitAct () = Helpers.inferWithExterns externs "Task.await 1" |> ignore
        awaitAct |> should throw typeof<TypeException>

    [<Test>]
    member _.``Allows non-string interpolation placeholder`` () =
        let typed = Helpers.infer "$\"value={1}\""
        match typed |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal TString
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Postfix map key annotations compose with containers`` () =
        let sources =
            [ "let f (m: string map[int]) = m[0]\nf { [0] = \"ok\" }", TOption TString
              "let f (m: int list map[bool]) = m[true]\nf { [true] = [1] }", TOption(TList TInt)
              "let f (m: int map[int] list) = m\nf [{ [0] = 1 }]", TList(TMap(TInt, TInt))
              "let f (m: string map[string]) = m\nf { [\"a\"] = \"ok\" }", TMap(TString, TString)
              "let f (m: string map[numeric]) = m\nf { [0] = \"ok\" }", TMap(TInt, TString) ]
        for source, expected in sources do
            match Helpers.infer source |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal expected
            | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``Rejects angle bracket map type syntax`` () =
        let act () = Helpers.parse "let f (m: map<int, string>) = m" |> ignore
        act |> should throw typeof<ParseException>

    [<Test>]
    member _.``Numeric map keys constrain function arguments to int`` () =
        let sources =
            [ "let make k = { [k + k] = \"value\" }\nmake 2", TMap(TInt, TString)
              "let has k m = Map.containsKey (k + k) m\nhas 2 { [4] = \"value\" }", TBool
              "let has k m = m |> Map.containsKey (k + k)\nhas 2 { [4] = \"value\" }", TBool ]
        for source, expected in sources do
            match Helpers.infer source |> List.last with
            | TypeInfer.TSExpr te -> te.Type |> should equal expected
            | _ -> Assert.Fail("Expected expression")
        let reject () = Helpers.infer "let make k = { [k + k] = 1 }\nmake 2.5" |> ignore
        reject |> should throw typeof<TypeException>

    [<Test>]
    member _.``Map value numeric inference still permits floats`` () =
        match Helpers.infer "let make v = { [true] = v + v }\nmake 2.5" |> List.last with
        | TypeInfer.TSExpr te -> te.Type |> should equal (TMap(TBool, TFloat))
        | _ -> Assert.Fail("Expected expression")

    [<Test>]
    member _.``String keyed maps format with legacy shorthand`` () =
        Types.typeToString (TMap(TString, TInt)) |> should equal "int map"
        Types.typeToString (TMap(TInt, TString)) |> should equal "string map[int]"
        Types.typeToString (TMap(TBool, TList TInt)) |> should equal "int list map[bool]"
