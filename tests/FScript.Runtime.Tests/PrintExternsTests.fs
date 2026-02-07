namespace FScript.Runtime.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type PrintExternsTests () =
    [<Test>]
    member _.``print writes to console and returns unit`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result = invoke PrintExterns.print [ VString "hello-print" ]
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().TrimEnd() |> should equal "hello-print"
        finally
            Console.SetOut(original)
