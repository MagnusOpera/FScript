namespace FScript.Core.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit
open FScript.Core
open FScript.Host

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
