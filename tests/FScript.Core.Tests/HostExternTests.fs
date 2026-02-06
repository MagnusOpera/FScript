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
        let script = "map_empty 0 |> map_add \"a\" 1 |> map_tryFind \"a\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

    [<Test>]
    member _.``Json deserialize uses typeof record`` () =
        let script =
            "type Package = { Name: string; Version: string option; Deps: int map }\n" +
            "let json = \"{\\\"Name\\\":\\\"pkg\\\",\\\"Version\\\":null,\\\"Deps\\\":{\\\"a\\\":1}}\"\n" +
            "json_deserialize (typeof Package) json"

        match Helpers.evalWithExterns externs script with
        | VOption (Some (VRecord fields)) ->
            fields.ContainsKey "Name" |> should equal true
        | _ -> Assert.Fail("Expected Some record")

    [<Test>]
    member _.``Regex groups external returns captures`` () =
        let script = "regex_match_groups \"^file:(.*)$\" \"file:foo\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VList [ VString "foo" ])) -> ()
        | _ -> Assert.Fail("Expected group capture")

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
