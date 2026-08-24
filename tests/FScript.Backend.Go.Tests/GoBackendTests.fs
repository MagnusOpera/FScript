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
    member _.``Go backend reports open reachable results`` () =
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram "let identity value = value\nidentity"; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/open" } compiled.Lir with
        | Ok _ -> Assert.Fail("Expected FSB4007")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSB4007"))

    [<Test>]
    member _.``Go backend rejects cloning effectful generalized initializers`` () =
        let source = "let generalized =\n  let _ = Console.writeLine \"effect\"\n  fun value -> value\n(generalized 1, generalized \"x\")"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/effectful" } compiled.Lir with
        | Ok _ -> Assert.Fail("Expected FSB4008")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSB4008"))

    [<Test>]
    member _.``Go monomorphization closes quicksort declarations and functions`` () =
        let source = File.ReadAllText(Path.Combine(TestContext.CurrentContext.TestDirectory, "../../../../../samples/quicksort.fss"))
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoMonomorphization.specialize compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok specialized ->
            Assert.That(specialized.Declarations |> List.exists (fun item -> item.Name = "quicksort" && item.Type = TFun(TList TInt, TList TInt)), Is.True)
            Assert.That(specialized.Declarations |> List.forall (fun item -> not (GoSpecialization.containsVariable item.Type)), Is.True)
            Assert.That(specialized.Functions |> List.forall (fun item -> not (GoSpecialization.containsVariable item.ParameterType) && not (GoSpecialization.containsVariable item.ReturnType)), Is.True)

    [<Test>]
    member _.``Go specialization uses concrete curry and bottom context`` () =
        let source = "let fail message = raise message\nlet parse label text =\n    match Int.tryParse text with\n    | Some value -> value\n    | None -> fail $\"Invalid {label}: {text}\"\nparse \"number\" \"1\""
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/context" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let generated = files |> List.map _.Content |> String.concat "\n"
            Assert.That(generated, Does.Not.Contain("any"))

    [<Test>]
    member _.``Basic sample compiles and runs as generated Go`` () =
        requireGo ()
        let samplePath = Path.GetFullPath(Path.Combine(TestContext.CurrentContext.TestDirectory, "../../../../../samples/basic/basic.fss"))
        let sampleDirectory =
            match Path.GetDirectoryName samplePath with
            | null -> failwith "The BASIC sample path has no parent directory."
            | value -> value
        let program = IncludeResolver.parseProgramFromFile sampleDirectory samplePath
        let compiled =
            match Compiler.compile { Program = program; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/basic" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let directory = Path.Combine(Path.GetTempPath(), $"fscript-basic-{Guid.NewGuid():N}")
            try
                for file in files do
                    let path = Path.Combine(directory, file.RelativePath)
                    match Path.GetDirectoryName(path) with
                    | null -> ()
                    | parent -> Directory.CreateDirectory(parent) |> ignore
                    File.WriteAllText(path, file.Content)
                let run arguments =
                    let start = ProcessStartInfo("go", WorkingDirectory = directory, RedirectStandardOutput = true, RedirectStandardError = true)
                    for argument in arguments do start.ArgumentList.Add(argument)
                    use proc =
                        match Process.Start(start) with
                        | null -> failwith "Failed to start Go."
                        | value -> value
                    let output, error = proc.StandardOutput.ReadToEnd(), proc.StandardError.ReadToEnd()
                    proc.WaitForExit()
                    Assert.That(proc.ExitCode, Is.EqualTo(0), output + error)
                    output
                run [ "test"; "./..." ] |> ignore
                let basicProgram = Path.Combine(sampleDirectory, "helloworld.bas")
                Assert.That(run [ "run"; "."; basicProgram ], Is.EqualTo("HELLO WORLD\n()\n"))
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

    [<Test>]
    member _.``Quicksort uses the zero-erasure Go path`` () =
        requireGo ()
        let source = File.ReadAllText(Path.Combine(TestContext.CurrentContext.TestDirectory, "../../../../../samples/quicksort.fss"))
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/quicksort" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let generated = files |> List.map _.Content |> String.concat "\n"
            Assert.That(generated, Does.Contain("[]int64"))
            Assert.That(Regex.IsMatch(generated, @"\bany\b|interface\s*\{"), Is.False)
            Assert.That(generated, Does.Not.Contain("fsruntime"))
            let directory = Path.Combine(Path.GetTempPath(), $"fscript-quicksort-{Guid.NewGuid():N}")
            try
                for file in files do
                    let path = Path.Combine(directory, file.RelativePath)
                    match Path.GetDirectoryName(path) with
                    | null -> ()
                    | parent -> Directory.CreateDirectory(parent) |> ignore
                    File.WriteAllText(path, file.Content)
                let run command arguments =
                    let start = ProcessStartInfo(command, WorkingDirectory = directory, RedirectStandardOutput = true, RedirectStandardError = true)
                    for argument in arguments do start.ArgumentList.Add(argument)
                    use proc =
                        match Process.Start(start) with
                        | null -> failwith $"Failed to start {command}."
                        | value -> value
                    let output, error = proc.StandardOutput.ReadToEnd(), proc.StandardError.ReadToEnd()
                    proc.WaitForExit()
                    Assert.That(proc.ExitCode, Is.EqualTo(0), output + error)
                    output
                Assert.That(run "gofmt" [ "-d"; directory ], Is.Empty)
                run "go" [ "test"; "./..." ] |> ignore
                let output = run "go" [ "run"; "." ]
                Assert.That(output, Does.Contain("quicksort output: [1;2;2;3;3;4;5;6;7;8;9]"))
            finally
                if Directory.Exists directory then Directory.Delete(directory, true)

    [<Test>]
    member _.``Fibonacci emits statement oriented control flow`` () =
        let source = File.ReadAllText(Path.Combine(TestContext.CurrentContext.TestDirectory, "../../../../../samples/fibonacci.fss"))
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/fibonacci" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Not.Contain("value83 = func() struct{}"), program)
            Assert.That(program, Does.Not.Contain("return func() int64"), program)
            Assert.That(program, Does.Contain("for value := rangeFirst"), program)
            Assert.That(program, Does.Contain("tailMatchValue := arg"), program)
            Assert.That(program, Does.Contain("raised89("), program)
            Assert.That(program, Does.Not.Contain("pipelineFunction8"), program)

    [<Test>]
    member _.``Saturated curried functions use native multi argument calls`` () =
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram "let add left right = left + right\nadd 1 2"; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/arity" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(Regex.IsMatch(program, @"func raised\d+\(arg\d+ int64, arg\d+ int64\) int64"), Is.True, program)
            Assert.That(Regex.IsMatch(program, @"raised\d+\(int64\(1\), int64\(2\)\)"), Is.True, program)

    [<Test>]
    member _.``Typed Go vertical slice emits no dynamic carriers`` () =
        let source = "let identity value = value\n(identity 42, identity \"ok\", [1; 2], Some 3)"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let specialized =
            match GoMonomorphization.specialize compiled.Lir with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        Assert.That(TypedGoEmitter.canEmit specialized, Is.True, Lir.format specialized)
        TypedGoEmitter.generate "fscript.generated/typed" specialized |> ignore
        match GoBackend.generate { ModuleName = "fscript.generated/typed" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let generated = files |> List.map _.Content |> String.concat "\n"
            Assert.That(Regex.IsMatch(generated, @"\bany\b|interface\s*\{\s*\}"), Is.False, generated)
            Assert.That(generated, Does.Contain("[]int64"))
            Assert.That(generated, Does.Contain("func("))

    [<Test>]
    member _.``Specialized Go builtins use concrete collection helpers`` () =
        let source = "(List.map (fun value -> value + 1) [1; 2], Option.defaultValue 0 (Some 3), Map.add \"b\" 2 (Map.ofList [(\"a\", 1)]))"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/typedbuiltins" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let generated = files |> List.map _.Content |> String.concat "\n"
            Assert.That(generated, Does.Contain("[]int64"))
            Assert.That(generated, Does.Contain("Entries []entry_"))
            Assert.That(Regex.IsMatch(generated, @"\bany\b|interface\s*\{"), Is.False)
            Assert.That(generated, Does.Not.Contain("reflect."))

    [<Test>]
    member _.``Go backend emits private typed task synchronization`` () =
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram "Task.await (Task.spawn (fun _ -> 42))"; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/tasks" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Contain("<-chan struct{}"))
            Assert.That(program, Does.Contain("go func()"))
            Assert.That(program, Does.Not.Contain("any"))

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
    member _.``Go backend emits default filesystem externs only when reachable`` () =
        let compile source =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value.Lir
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))

        match GoBackend.generate { ModuleName = "fscript.generated/runtimeexterns" } (compile "let unused = Fs.exists\n1") with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok _ -> ()

        match GoBackend.generate { ModuleName = "fscript.generated/runtimeexterns" } (compile "Fs.exists \"missing\"") with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files -> Assert.That(files |> List.exists (fun file -> file.Content.Contains("os.Stat")), Is.True)

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
            Assert.That(program, Does.Contain("func(goimport0.Person) goimport0.Person"))
            Assert.That(program, Does.Not.Contain("any"))

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
            Assert.That(program, Does.Contain("type named_Person struct"))
            Assert.That(program, Does.Contain("Field_Name string"))
            Assert.That(program, Does.Not.Contain("any"))

    [<Test>]
    member _.``Go backend emits tagged ordinary union types`` () =
        let source = "type Shape = | Point | Circle of int\nCircle 3"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/unions" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Contain("type named_Shape struct"))
            Assert.That(program, Does.Contain("Tag"))
            Assert.That(program, Does.Contain("Case_Circle int64"))
            Assert.That(program, Does.Not.Contain("interface{}"))

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
    member _.``Go emitter lowers LIR pattern plans to typed checks`` () =
        let source = "match Some (1, 2) with | Some (left, right) -> left + right | None -> 0"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/patterns" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Contain("matchValue := option_"))
            Assert.That(program, Does.Contain("(matchValue).HasValue"))
            Assert.That(program, Does.Contain("((matchValue).Value).Item0"))
            Assert.That(program, Does.Not.Contain("MatchPatternPlan"))
            Assert.That(program, Does.Not.Contain("any"))

    [<Test>]
    member _.``Go emitter lowers direct self tail recursion to a native loop`` () =
        let source = "let rec loop total n = if n <= 0 then total else loop (total + n) (n - 1)\nloop 0 10"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match GoBackend.generate { ModuleName = "fscript.generated/tailrec" } compiled.Lir with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok files ->
            let program = files |> List.find (fun file -> file.RelativePath.EndsWith("program.go")) |> _.Content
            Assert.That(program, Does.Contain("for {"))
            Assert.That(program, Does.Contain("continue"))
            Assert.That(program, Does.Contain("tailNext"))
            Assert.That(Regex.IsMatch(program, @"arg\d+, arg\d+ = tailNext\d+, tailNext\d+"), Is.True)

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
    [<TestCase("Regex.matchGroups \"^file:((?:[^:]+)):(.*)$\" \"file:name:value\"", "Some [\"name\";\"value\"]")>]
    [<TestCase("Task.await (Task.spawn (fun _ -> 40 + 2))", "42")>]
    [<TestCase("let first = Task.spawn (fun _ -> 40 + 1)\nlet second = Task.spawn (fun _ -> 40 + 2)\n(Task.await first, Task.await second)", "(41, 42)")>]
    [<TestCase("(Fs.combinePath \"a\" \"b.txt\", Fs.parentDirectory \"a/b.txt\", Fs.extension \"a/b.txt\", Fs.fileNameWithoutExtension \"a/b.txt\")", "(\"a/b.txt\", Some \"a\", Some \".txt\", \"b\")")>]
    [<TestCase("let created = Fs.createDirectory \"tmp/nested\"\nlet written = Fs.writeText \"tmp/nested/data.txt\" \"loaded\"\n(created, written, Fs.exists \"tmp/nested/data.txt\", Fs.kind \"tmp/nested/data.txt\", Fs.glob \"tmp/**/*.txt\", Fs.enumerateFiles \"tmp\" \"**/*.txt\")", "(true, true, true, File \"tmp/nested/data.txt\", Some [\"tmp/nested/data.txt\"], Some [\"tmp/nested/data.txt\"])")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nperson.Name", "\"Ada\"")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nlet older = { person with Age = 43 }\nolder.Age", "43")>]
    [<TestCase("let home = {| City = \"Paris\"; Zip = 75000 |}\nlet wider = {| home with Name = \"Robert\" |}\n(wider.Name, wider.City, wider.Zip)", "(\"Robert\", \"Paris\", 75000)")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet age (person: Person) = person.Age\nage {| Name = \"Ada\"; Age = 42 |}", "42")>]
    [<TestCase("type Shape = | Point | Circle of int\nCircle 2 = Circle 2", "true")>]
    [<TestCase("type Person = { Name: string }\ntype Result = | Missing | Found of Person\nFound { Name = \"Ada\" } = Found { Name = \"Ada\" }", "true")>]
    [<TestCase("(1.0, [2.0], 1.0 / 0.0, 0.0 / 0.0)", "(1, [2], Infinity, NaN)")>]
    [<TestCase("let ordered left middle right = left < middle && middle <= right && right > left && right >= middle\nordered 1 2 3", "true")>]
    [<TestCase("let rec loop total n = if n <= 0 then total else loop (total + n) (n - 1)\nloop 0 1000000", "500000500000")>]
    [<TestCase("let tap label value = let _ = Console.writeLine label\n    value\nlet rec loop total n = if n <= 0 then total else (tap \"last\" (n - 1)) |> loop (tap \"first\" (total + n))\nloop 0 1", "last\nfirst\n1")>]
    [<TestCase("let run start = (let rec loop n = if n <= 0 then n else loop (n - 1)\n    loop start\n)\nrun 1000000", "0")>]
    [<TestCase("let rec length total values =\n    match values with\n    | [] -> total\n    | _::tail -> length (total + 1) tail\nlength 0 [1..100000]", "100000")>]
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
        for file in files |> List.filter (fun file -> file.RelativePath.EndsWith(".go", StringComparison.Ordinal)) do
            Assert.That(Regex.IsMatch(file.Content, @"\bany\b|interface\s*\{"), Is.False, file.RelativePath)
            Assert.That(file.Content, Does.Not.Contain("reflect."), file.RelativePath)
            Assert.That(file.Content, Does.Not.Contain("IntrinsicClosure"), file.RelativePath)
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
