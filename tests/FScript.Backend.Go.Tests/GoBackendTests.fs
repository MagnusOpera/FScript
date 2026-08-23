namespace FScript.Backend.Go.Tests

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type GoBackendTests() =
    let requireGo () =
        try
            use proc =
                match Process.Start(ProcessStartInfo("go", "version", RedirectStandardOutput = true)) with
                | null -> raise (InvalidOperationException("Could not start go"))
                | value -> value
            proc.WaitForExit()
            if proc.ExitCode <> 0 then raise (InvalidOperationException("go version failed"))
        with ex ->
            if String.Equals(Environment.GetEnvironmentVariable("CI"), "true", StringComparison.OrdinalIgnoreCase) then
                Assert.Fail($"Go is required in CI: {ex.Message}")
            else Assert.Ignore($"Go is not installed: {ex.Message}")

    [<Test>]
    member _.``Go specialization closes HM instantiations deterministically`` () =
        let template = TFun(TVar 0, TFun(TList(TVar 0), TList(TVar 0)))
        let concrete = TFun(TInt, TFun(TList TInt, TList TInt))
        match GoSpecialization.unifyInstantiation template concrete with
        | Error error -> Assert.Fail(error)
        | Ok substitution ->
            Assert.That(GoSpecialization.apply substitution template, Is.EqualTo(concrete))
            Assert.That(GoSpecialization.specializationKey 7 concrete, Is.EqualTo(GoSpecialization.specializationKey 7 concrete))

    [<Test>]
    member _.``Go representations reject residual open types`` () =
        match GoSpecialization.tryRepresentation [] (TList(TVar 0)) with
        | Ok _ -> Assert.Fail("Expected an open-representation error")
        | Error error -> Assert.That(error, Does.Contain("Open type variable"))

    [<Test>]
    member _.``Go backend reports reachable tasks explicitly`` () =
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram "Task.await (Task.spawn (fun _ -> 42))"; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/tasks" } compiled.Lir with
        | Ok _ -> Assert.Fail("Expected unsupported task diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSB4004"))

    [<Test>]
    member _.``Go backend validates caller-provided LIR`` () =
        let compiled = match Compiler.compile { Program = Parser.parseProgram "1"; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let entry = compiled.Lir.Entry.Value
        let invalid = { compiled.Lir with Entry = Some { entry with Node = Lir.Load(Lir.SymbolId 999999) } }
        match GoBackend.generate { ModuleName = "fscript.generated/invalid" } invalid with
        | Ok _ -> Assert.Fail("Expected invalid LIR to be rejected")
        | Error diagnostics -> Assert.That(diagnostics |> List.exists (fun diagnostic -> diagnostic.Code.StartsWith("FSL2")), Is.True)

    [<Test>]
    member _.``Go backend rejects only reachable host externs`` () =
        let external =
            { Name = "Host.value"; Scheme = Forall([], TFun(TInt, TInt)); Arity = 1
              Kind = UnsupportedHostExtern }
        let compile source =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [ external ] } with
            | Ok value -> value.Lir
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/externs" } (compile "Host.value 1") with
        | Ok _ -> Assert.Fail("Expected unsupported host-extern diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSB4006"))
        match GoBackend.generate { ModuleName = "fscript.generated/externs" } (compile "1") with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok _ -> ()

    [<Test>]
    member _.``Go backend reports default host runtime externs only when reachable`` () =
        let compile source =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value.Lir
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))

        match GoBackend.generate { ModuleName = "fscript.generated/runtimeexterns" } (compile "let unused = Fs.exists\n1") with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok _ -> ()

        match GoBackend.generate { ModuleName = "fscript.generated/runtimeexterns" } (compile "Fs.exists \"missing\"") with
        | Ok _ -> Assert.Fail("Expected unsupported host-runtime diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSB4006"))

    [<Test>]
    member _.``Go backend emits imported record boundary adapters`` () =
        let source = "[<Import(\"example.com/model\", \"Person\")>] type Person = { Name: string; Age: int }\n[<Import(\"example.com/model\", \"Birthday\")>] let birthday : Person -> Person\nbirthday { Name = \"Ada\"; Age = 42 }"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let roundTripped =
            match compiled.Lir |> IrArtifact.serialize |> IrArtifact.deserialize with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        Assert.That(roundTripped.ImportedRecords.Length, Is.EqualTo(1))
        match GoBackend.generate { ModuleName = "fscript.generated/importrecords" } roundTripped with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Contain("goimport"))
            Assert.That(program, Does.Contain(".Person{Name:"))
            Assert.That(program, Does.Contain("importedResult.Name"))

    [<Test>]
    member _.``Go backend rejects unsupported imported record shapes`` () =
        let source = "[<Import(\"example.com/model\", \"person\")>] type Person = { name: string }\n1"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/importrecords" } compiled.Lir with
        | Ok _ -> Assert.Fail("Expected imported-record diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSB4005"))

    [<Test>]
    member _.``Go backend emits concrete ordinary record types`` () =
        let source = "type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nperson.Name"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/records" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Match("type record[0-9]+ struct"))
            Assert.That(program, Does.Contain("`fscript:\"Name\"`"))

    [<Test>]
    member _.``Go backend emits tagged ordinary union types`` () =
        let source = "type Shape = | Point | Circle of int\nCircle 3"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/unions" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Match("type union[0-9]+ struct"))
            Assert.That(program, Does.Contain("Tag"))
            Assert.That(program, Does.Contain("Case0 int64"))
            Assert.That(program, Does.Contain("FScriptUnion() fsruntime.Union"))

    [<Test>]
    member _.``Go generation is deterministic targets Go 1.25 and emits no generic declarations`` () =
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram "let identity value = value\nidentity [1; 2]"; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let generate () =
            match GoBackend.generate { ModuleName = "fscript.generated/deterministic" } compiled.Lir with
            | Ok files -> files
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let first = generate ()
        let second = generate ()
        Assert.That((first = second), Is.True)
        let goMod = first |> List.find (fun file -> file.RelativePath = "go.mod") |> _.Content
        Assert.That(goMod, Does.Contain("go 1.25"))
        for file in first |> List.filter (fun file -> file.RelativePath.EndsWith(".go")) do
            Assert.That(Regex.IsMatch(file.Content, @"func\s+(?:\([^)]*\)\s*)?[A-Za-z_][A-Za-z0-9_]*\s*\["), Is.False, file.RelativePath)

    [<Test>]
    member _.``Go emitter delegates LIR pattern plans to the runtime`` () =
        let source = "match Some (1, 2) with | Some (left, right) -> left + right | None -> 0"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/patterns" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Contain("fsruntime.MatchPatternPlan(matchValue"))
            Assert.That(program, Does.Contain("fsruntime.ProjectionStep{Kind: \"option-value\"}"))
            Assert.That(program, Does.Not.Contain("fsruntime.OptionValue(matchValue"))
            Assert.That(program, Does.Not.Contain("fsruntime.AsTuple(matchValue"))

    [<TestCase("let add x y = x + y\nadd 1 2", "3")>]
    [<TestCase("[1] @ [2; 3]", "[1;2;3]")>]
    [<TestCase("[1; 2] = [1; 2]", "true")>]
    [<TestCase("(1, 2)", "(1, 2)")>]
    [<TestCase("let f x = (let y = x + 1\n    y * 2\n)\nf 2", "6")>]
    [<TestCase("let f x = (let rec loop n = if n = 0 then x else loop (n - 1)\n    loop 2\n)\nf 7", "7")>]
    [<TestCase("match Some 3 with\n    | Some x -> x + 1\n    | None -> 0", "4")>]
    [<TestCase("match [1] with\n    | x::xs when x < 0 -> x\n    | _ -> 42", "42")>]
    [<TestCase("[1..3]", "[1;2;3]")>]
    [<TestCase("[1; 2][1]", "Some 2")>]
    [<TestCase("$\"value {1 + 2}\"", "\"value 3\"")>]
    [<TestCase("type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Point -> 0\n    | Circle r -> r\nradius (Circle 3)", "3")>]
    [<TestCase("let tail = { [\"a\"] = 2; [\"b\"] = 3 }\nlet values = { [\"a\"] = 1; ..tail }\nvalues[\"b\"]", "Some 3")>]
    [<TestCase("let rec even n =\n    if n = 0 then true else odd (n - 1)\nand odd n =\n    if n = 0 then false else even (n - 1)\n\neven 10", "true")>]
    [<TestCase("String.toUpper \"hello\"", "\"HELLO\"")>]
    [<TestCase("List.length [1; 2; 3]", "3")>]
    [<TestCase("Option.defaultValue 9 None", "9")>]
    [<TestCase("let sum pair = (let (left, right) = pair\n    left + right\n)\nsum (1, 2)", "3")>]
    [<TestCase("for value in [1; 2; 3] do ignore value", "()")>]
    [<TestCase("type Package = { Name: string }\ntypeof Package", "<type { Name: string }>")>]
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
    [<TestCase("Map.add \"b\" 2 (Map.ofList [(\"a\", 1)])", "map { \"a\" => 1; \"b\" => 2 }")>]
    [<TestCase("Map.ofList [(\"b\", 2); (\"a\", 1)]", "map { \"a\" => 1; \"b\" => 2 }")>]
    [<TestCase("[\"a\"; \"b\"] |> String.concat \",\"", "\"a,b\"")>]
    [<TestCase("Map.map (fun value -> value + 1) (Map.filter (fun key value -> value > 1) (Map.ofList [(\"b\", 2); (\"a\", 1)]))", "map { \"b\" => 3 }")>]
    [<TestCase("Map.choose (fun key value -> if value > 1 then Some (value + 1) else None) { [\"a\"] = 1; [\"b\"] = 2 }", "map { \"b\" => 3 }")>]
    [<TestCase("Map.remove \"a\" { [\"a\"] = 1; [\"b\"] = 2 }", "map { \"b\" => 2 }")>]
    [<TestCase("let (left, right) = (1, 2)\nleft + right", "3")>]
    [<TestCase("(let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 10\n)", "true")>]
    [<TestCase("match { [\"b\"] = 2; [\"a\"] = 1 } with\n    | { [key] = value; ..tail } -> value\n    | {} -> 0", "1")>]
    [<TestCase("match {| Name = \"Ada\"; Age = 42 |} with\n    | {| Name: string |} -> true\n    | _ -> false", "true")>]
    [<TestCase("List.length Env.Arguments", "0")>]
    [<TestCase("Option.isSome Env.ScriptName", "true")>]
    [<TestCase("[<Import(\"strings\", \"ToUpper\")>] let upper : string -> string\nupper \"hello\"", "\"HELLO\"")>]
    [<TestCase("Console.writeLine \"hello\"", "hello\n()")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nperson.Name", "\"Ada\"")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nlet older = { person with Age = 43 }\nolder.Age", "43")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet age (person: Person) = person.Age\nage {| Name = \"Ada\"; Age = 42 |}", "42")>]
    [<TestCase("type Shape = | Point | Circle of int\nCircle 2 = Circle 2", "true")>]
    [<TestCase("type Person = { Name: string }\ntype Result = | Missing | Found of Person\nFound { Name = \"Ada\" } = Found { Name = \"Ada\" }", "true")>]
    [<TestCase("(1.0, [2.0], 1.0 / 0.0, 0.0 / 0.0)", "(1, [2], Infinity, NaN)")>]
    [<TestCase("let ordered left middle right = left < middle && middle <= right && right > left && right >= middle\nordered 1 2 3", "true")>]
    member _.``Generated projects run with Go`` (source: string, expected: string) =
        requireGo ()
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
            let formatStart = ProcessStartInfo("gofmt")
            formatStart.WorkingDirectory <- directory
            formatStart.ArgumentList.Add("-d")
            formatStart.ArgumentList.Add(directory)
            formatStart.RedirectStandardOutput <- true
            use formatProcess =
                match Process.Start(formatStart) with
                | null -> failwith "Failed to start gofmt"
                | value -> value
            let formatDiff = formatProcess.StandardOutput.ReadToEnd()
            formatProcess.WaitForExit()
            Assert.That(formatProcess.ExitCode, Is.EqualTo(0))
            Assert.That(formatDiff, Is.Empty, "Generated Go sources must already be gofmt-compatible")
            let testStart = ProcessStartInfo("go")
            testStart.WorkingDirectory <- directory
            testStart.ArgumentList.Add("test")
            testStart.ArgumentList.Add("./...")
            testStart.RedirectStandardOutput <- true
            testStart.RedirectStandardError <- true
            use testProcess =
                match Process.Start(testStart) with
                | null -> failwith "Failed to start go test"
                | value -> value
            let testOutput = testProcess.StandardOutput.ReadToEnd()
            let testError = testProcess.StandardError.ReadToEnd()
            testProcess.WaitForExit()
            Assert.That(testProcess.ExitCode, Is.EqualTo(0), testOutput + testError)
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

    [<Test>]
    member _.``Generated runtime reads standard input and text files`` () =
        requireGo ()
        let source = "(Console.readLine (), Console.readLine (), Fs.readText \"input.txt\")"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let files =
            match GoBackend.generate { ModuleName = "fscript.generated/consoleinput" } compiled.Lir with
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
            File.WriteAllText(Path.Combine(directory, "input.txt"), "loaded")
            let start = ProcessStartInfo("go")
            start.WorkingDirectory <- directory
            start.ArgumentList.Add("run")
            start.ArgumentList.Add(".")
            start.RedirectStandardInput <- true
            start.RedirectStandardOutput <- true
            start.RedirectStandardError <- true
            use proc =
                match Process.Start(start) with
                | null -> failwith "Failed to start Go"
                | value -> value
            proc.StandardInput.WriteLine("Ada")
            proc.StandardInput.Close()
            let output = proc.StandardOutput.ReadToEnd()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Assert.That(proc.ExitCode, Is.EqualTo(0), error)
            Assert.That(output.Trim(), Is.EqualTo("(Some \"Ada\", None, Some \"loaded\")"))
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)
