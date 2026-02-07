namespace FScript.Language.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type HostExternTests () =
    let host = { RootDirectory = System.IO.Directory.GetCurrentDirectory() }
    let externs = Registry.all host

    [<Test>]
    member _.``Map externs support CRUD`` () =
        let script = "Map.empty 0 |> Map.add \"a\" 1 |> Map.tryFind \"a\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

    [<Test>]
    member _.``Map.try aliases Map.tryFind`` () =
        let script = "Map.empty 0 |> Map.add \"a\" 1 |> Map.try \"a\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

    [<Test>]
    member _.``List.map external maps values`` () =
        let script = "[0..3] |> List.map (fun n -> n + 1)"
        match Helpers.evalWithExterns externs script with
        | VList [ VInt 1L; VInt 2L; VInt 3L; VInt 4L ] -> ()
        | _ -> Assert.Fail("Expected mapped int list")

    [<Test>]
    member _.``List.iter external applies function and returns unit`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let script = "[1;2] |> List.iter (fun n -> n |> fun x -> $\"{x}\" |> print)"
            let result = Helpers.evalWithExterns externs script
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().Replace("\r\n", "\n").TrimEnd() |> should equal "1\n2"
        finally
            Console.SetOut(original)

    [<Test>]
    member _.``List.tryHead returns option`` () =
        match Helpers.evalWithExterns externs "[1;2] |> List.tryHead" with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

        match Helpers.evalWithExterns externs "[] |> List.tryHead" with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")

    [<Test>]
    member _.``List.tail returns tail and errors on empty list`` () =
        match Helpers.evalWithExterns externs "[1;2;3] |> List.tail" with
        | VList [ VInt 2L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected [2;3]")

        let act () = Helpers.evalWithExterns externs "[] |> List.tail" |> ignore
        act |> should throw typeof<EvalException>

    [<Test>]
    member _.``List.append concatenates lists`` () =
        match Helpers.evalWithExterns externs "List.append [1;2] [3]" with
        | VList [ VInt 1L; VInt 2L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected concatenated list")

    [<Test>]
    member _.``List externs support choose collect contains distinct exists fold rev tryFind tryFindIndex and filter`` () =
        match Helpers.evalWithExterns externs "[1;2;3;4] |> List.choose (fun x -> if x % 2 = 0 then Some x else None)" with
        | VList [ VInt 2L; VInt 4L ] -> ()
        | _ -> Assert.Fail("Expected choose of even elements")

        match Helpers.evalWithExterns externs "[1;2;3] |> List.collect (fun x -> [x; x + 10])" with
        | VList [ VInt 1L; VInt 11L; VInt 2L; VInt 12L; VInt 3L; VInt 13L ] -> ()
        | _ -> Assert.Fail("Expected collected list")

        match Helpers.evalWithExterns externs "List.contains 2 [1;2;3]" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected contains true")

        match Helpers.evalWithExterns externs "[1;2;1;3;2] |> List.distinct" with
        | VList [ VInt 1L; VInt 2L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected distinct values preserving order")

        match Helpers.evalWithExterns externs "List.exists (fun x -> x > 2) [1;2;3]" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected exists true")

        match Helpers.evalWithExterns externs "List.fold (fun s -> fun x -> s + x) 0 [1;2;3]" with
        | VInt 6L -> ()
        | _ -> Assert.Fail("Expected fold sum")

        match Helpers.evalWithExterns externs "List.rev [1;2;3]" with
        | VList [ VInt 3L; VInt 2L; VInt 1L ] -> ()
        | _ -> Assert.Fail("Expected reversed list")

        match Helpers.evalWithExterns externs "List.length [1;2;3]" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected list length 3")

        match Helpers.evalWithExterns externs "List.tryFind (fun x -> x > 2) [1;2;3]" with
        | VOption (Some (VInt 3L)) -> ()
        | _ -> Assert.Fail("Expected tryFind Some 3")

        match Helpers.evalWithExterns externs "List.tryFindIndex (fun x -> x = 2) [1;2;3]" with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected tryFindIndex Some 1")

        match Helpers.evalWithExterns externs "List.filter (fun x -> x % 2 = 1) [1;2;3;4]" with
        | VList [ VInt 1L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected filtered odd list")

    [<Test>]
    member _.``Option externs support mapping and defaults`` () =
        match Helpers.evalWithExterns externs "Option.get (Some 3)" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected Option.get Some value")

        match Helpers.evalWithExterns externs "Option.defaultValue 9 None" with
        | VInt 9L -> ()
        | _ -> Assert.Fail("Expected default on None")

        match Helpers.evalWithExterns externs "Option.defaultValue 9 (Some 3)" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected Some value over default")

        match Helpers.evalWithExterns externs "Option.defaultWith (fun _ -> 11) None" with
        | VInt 11L -> ()
        | _ -> Assert.Fail("Expected computed default on None")

        match Helpers.evalWithExterns externs "Option.defaultWith (fun _ -> raise \"boom\") (Some 3)" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected Option.defaultWith to skip fallback for Some")

        match Helpers.evalWithExterns externs "Option.isNone None" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected Option.isNone true")

        match Helpers.evalWithExterns externs "Option.isSome (Some 1)" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected Option.isSome true")

        match Helpers.evalWithExterns externs "Option.map (fun x -> x + 1) (Some 1)" with
        | VOption (Some (VInt 2L)) -> ()
        | _ -> Assert.Fail("Expected mapped Some value")

        match Helpers.evalWithExterns externs "Option.map (fun x -> x + 1) None" with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None unchanged")

    [<Test>]
    member _.``Option.get throws on None`` () =
        let act () = Helpers.evalWithExterns externs "Option.get None" |> ignore
        act |> should throw typeof<EvalException>

    [<Test>]
    member _.``Json deserialize uses typeof record`` () =
        let script =
            "type Package = { Name: string; Version: string option; Deps: int map }\n" +
            "let json = \"{\\\"Name\\\":\\\"pkg\\\",\\\"Version\\\":null,\\\"Deps\\\":{\\\"a\\\":1}}\"\n" +
            "Json.deserialize (typeof Package) json"

        match Helpers.evalWithExterns externs script with
        | VOption (Some (VRecord fields)) ->
            fields.ContainsKey "Name" |> should equal true
        | _ -> Assert.Fail("Expected Some record")

    [<Test>]
    member _.``Regex groups external returns captures`` () =
        let script = "Regex.matchGroups \"^file:(.*)$\" \"file:foo\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VList [ VString "foo" ])) -> ()
        | _ -> Assert.Fail("Expected group capture")

    [<Test>]
    member _.``Old flat extern names are no longer available`` () =
        let act () = Helpers.evalWithExterns externs "regex_match_groups \"^file:(.*)$\" \"file:foo\"" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``print external writes to console`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result = Helpers.evalWithExterns externs "print \"hello-core-test\""
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().TrimEnd() |> should equal "hello-core-test"
        finally
            Console.SetOut(original)

    [<Test>]
    member _.``for loop can call print external`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result = Helpers.evalWithExterns externs "for msg in [\"a\";\"b\"] do print msg"
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().Replace("\r\n", "\n").TrimEnd() |> should equal "a\nb"
        finally
            Console.SetOut(original)
