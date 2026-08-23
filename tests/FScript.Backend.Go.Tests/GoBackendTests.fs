namespace FScript.Backend.Go.Tests

open System
open System.Diagnostics
open System.IO
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type GoBackendTests() =
    [<TestCase("let add x y = x + y\nadd 1 2", "3")>]
    [<TestCase("[1] @ [2; 3]", "[1;2;3]")>]
    [<TestCase("[1; 2] = [1; 2]", "true")>]
    [<TestCase("let f x = (let y = x + 1\n    y * 2\n)\nf 2", "6")>]
    [<TestCase("let f x = (let rec loop n = if n = 0 then x else loop (n - 1)\n    loop 2\n)\nf 7", "7")>]
    [<TestCase("match Some 3 with\n    | Some x -> x + 1\n    | None -> 0", "4")>]
    [<TestCase("match [1] with\n    | x::xs when x < 0 -> x\n    | _ -> 42", "42")>]
    [<TestCase("[1..3]", "[1;2;3]")>]
    [<TestCase("[1; 2][1]", "Some 2")>]
    [<TestCase("$\"value {1 + 2}\"", "value 3")>]
    [<TestCase("type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Point -> 0\n    | Circle r -> r\nradius (Circle 3)", "3")>]
    [<TestCase("let tail = { [\"a\"] = 2; [\"b\"] = 3 }\nlet values = { [\"a\"] = 1; ..tail }\nvalues[\"b\"]", "Some 3")>]
    [<TestCase("let rec even n =\n    if n = 0 then true else odd (n - 1)\nand odd n =\n    if n = 0 then false else even (n - 1)\n\neven 10", "true")>]
    [<TestCase("String.toUpper \"hello\"", "HELLO")>]
    [<TestCase("List.length [1; 2; 3]", "3")>]
    [<TestCase("Option.defaultValue 9 None", "9")>]
    [<TestCase("let sum pair = (let (left, right) = pair\n    left + right\n)\nsum (1, 2)", "3")>]
    [<TestCase("for value in [1; 2; 3] do ignore value", "()")>]
    [<TestCase("type Package = { Name: string }\ntypeof Package", "{ Name: string }")>]
    member _.``Generated projects run with Go`` (source: string, expected: string) =
        let input = { Program = Parser.parseProgram source; Externs = [] }
        let compiled =
            match Compiler.compile input with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let files =
            match GoBackend.generate { ModuleName = "fscript.generated/testprogram" } compiled.Lir with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let directory = Path.Combine(Path.GetTempPath(), $"fscript-go-{Guid.NewGuid():N}")
        try
            for file in files do
                let path = Path.Combine(directory, file.RelativePath)
                match Path.GetDirectoryName(path) with
                | null -> ()
                | parent -> Directory.CreateDirectory(parent) |> ignore
                File.WriteAllText(path, file.Content)
            let start = ProcessStartInfo("go")
            start.WorkingDirectory <- directory
            start.ArgumentList.Add("run")
            start.ArgumentList.Add(".")
            start.RedirectStandardOutput <- true
            start.RedirectStandardError <- true
            use proc =
                match Process.Start(start) with
                | null -> failwith "Failed to start Go"
                | value -> value
            let output = proc.StandardOutput.ReadToEnd()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Assert.That(proc.ExitCode, Is.EqualTo(0), error)
            Assert.That(output.Trim(), Is.EqualTo(expected))
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)
