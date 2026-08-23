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
    [<TestCase("List.map (fun value -> value + 1) [1; 2]", "[2;3]")>]
    [<TestCase("List.choose (fun value -> if value > 1 then Some value else None) [1; 2; 3]", "[2;3]")>]
    [<TestCase("List.collect (fun value -> [value; value]) [1; 2]", "[1;1;2;2]")>]
    [<TestCase("List.fold (fun state value -> state + value) 0 [1; 2; 3]", "6")>]
    [<TestCase("List.tryGet (fun value -> value = 2) [1; 2; 3]", "Some 1")>]
    [<TestCase("Option.map (fun value -> value + 1) (Some 4)", "Some 5")>]
    [<TestCase("Option.defaultWith (fun _ -> 7) None", "7")>]
    [<TestCase("Map.fold (fun state key value -> state + value) 0 { [\"a\"] = 1; [\"b\"] = 2 }", "3")>]
    [<TestCase("List.iter (fun value -> ignore value) [1; 2]", "()")>]
    [<TestCase("List.exists (fun value -> value = 2) [1; 2]", "true")>]
    [<TestCase("List.distinct [1; 1; 2]", "[1;2]")>]
    [<TestCase("List.filter (fun value -> value > 1) [1; 2; 3]", "[2;3]")>]
    [<TestCase("List.tryFind (fun value -> value > 1) [1; 2; 3]", "Some 2")>]
    [<TestCase("List.tryItem 1 [4; 5]", "Some 5")>]
    [<TestCase("Map.add \"b\" 2 (Map.ofList [(\"a\", 1)])", "{ [a] = 1; [b] = 2 }")>]
    [<TestCase("Map.map (fun value -> value + 1) (Map.filter (fun key value -> value > 1) (Map.ofList [(\"b\", 2); (\"a\", 1)]))", "{ [b] = 3 }")>]
    [<TestCase("Map.choose (fun key value -> if value > 1 then Some (value + 1) else None) { [\"a\"] = 1; [\"b\"] = 2 }", "{ [b] = 3 }")>]
    [<TestCase("Map.remove \"a\" { [\"a\"] = 1; [\"b\"] = 2 }", "{ [b] = 2 }")>]
    [<TestCase("let (left, right) = (1, 2)\nleft + right", "3")>]
    [<TestCase("(let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 10\n)", "true")>]
    [<TestCase("match { [\"b\"] = 2; [\"a\"] = 1 } with\n    | { [key] = value; ..tail } -> value\n    | {} -> 0", "1")>]
    [<TestCase("match {| Name = \"Ada\"; Age = 42 |} with\n    | {| Name: string |} -> true\n    | _ -> false", "true")>]
    [<TestCase("List.length Env.Arguments", "0")>]
    [<TestCase("Option.isSome Env.ScriptName", "true")>]
    [<TestCase("[<Import(\"strings\", \"ToUpper\")>] let upper : string -> string\nupper \"hello\"", "HELLO")>]
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
