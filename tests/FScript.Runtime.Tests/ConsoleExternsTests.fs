namespace FScript.Runtime.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type ConsoleExternsTests () =
    [<Test>]
    member _.``Console writeLine writes to console and returns unit`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result = invoke ConsoleExterns.write_line [ VString "hello-console" ]
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().TrimEnd() |> should equal "hello-console"
        finally
            Console.SetOut(original)

    [<Test>]
    member _.``Console readLine reads a line and wraps it in Some`` () =
        let original = Console.In
        use reader = new StringReader("hello-input\n")
        Console.SetIn(reader)
        try
            let result = invoke ConsoleExterns.read_line [ VUnit ]
            match result with
            | VOption (Some (VString "hello-input")) -> ()
            | _ -> Assert.Fail("Expected Some hello-input")
        finally
            Console.SetIn(original)

    [<Test>]
    member _.``Console readLine returns None on end of input`` () =
        let original = Console.In
        use reader = new StringReader("")
        Console.SetIn(reader)
        try
            let result = invoke ConsoleExterns.read_line [ VUnit ]
            match result with
            | VOption None -> ()
            | _ -> Assert.Fail("Expected None")
        finally
            Console.SetIn(original)
