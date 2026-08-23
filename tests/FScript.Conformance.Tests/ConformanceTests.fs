namespace FScript.Conformance.Tests

open System
open System.Diagnostics
open System.IO
open FScript.Compiler
open FScript.Language
open NUnit.Framework

module private Harness =
    type Outcome = { Value: string; Stdout: string list }

    let private quoteString (value: string) =
        "\"" + value.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t") + "\""

    let rec private formatLirValue = function
        | LirValue.VUnit -> "()"
        | LirValue.VInt value -> string value
        | LirValue.VFloat value -> value.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | LirValue.VBool value -> string value |> _.ToLowerInvariant()
        | LirValue.VString value -> quoteString value
        | LirValue.VOption None -> "None"
        | LirValue.VOption(Some value) -> "Some " + formatLirValue value
        | LirValue.VList values -> "[" + (values |> List.map formatLirValue |> String.concat ";") + "]"
        | LirValue.VTuple values -> "(" + (values |> List.map formatLirValue |> String.concat ", ") + ")"
        | LirValue.VRecord fields -> "{ " + (fields |> Map.toList |> List.map (fun (name, value) -> name + " = " + formatLirValue value) |> String.concat "; ") + " }"
        | LirValue.VUnion(_, caseName, None) -> caseName
        | LirValue.VUnion(_, caseName, Some value) -> caseName + " " + formatLirValue value
        | LirValue.VMap entries -> "map { " + (entries |> List.map (fun (key, value) -> formatLirValue key + " => " + formatLirValue value) |> String.concat "; ") + " }"
        | LirValue.VTypeToken value -> "<type " + Types.typeToString value + ">"
        | LirValue.VClosure _ | LirValue.VIntrinsic _ | LirValue.VExternal _ | LirValue.VTargetImport _ -> "<function>"
        | LirValue.VRuntimeContext -> "<runtime-context>"
    let formatLir value = formatLirValue value

    let compile source =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Ok value -> value
        | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))

    let legacy source =
        let output = ResizeArray<string>()
        let console : ExternalFunction =
            { Name = "Console.writeLine"; Scheme = Forall([], TFun(TString, TUnit)); Arity = 1
              Impl = fun _ values -> match values with [ VString value ] -> output.Add(value); VUnit | _ -> VUnit }
        let program = Parser.parseProgram source
        let value = program |> TypeInfer.inferProgramWithExterns [ console ] |> Eval.evalProgramWithExterns [ console ] |> Pretty.valueToString
        { Value = value; Stdout = List.ofSeq output }

    let lir source =
        let artifact = compile source |> _.Lir |> IrArtifact.serialize
        let loaded = match IrArtifact.deserialize artifact with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match LirInterpreter.execute { ScriptName = None; Arguments = [] } loaded with
        | Ok result -> Ok { Value = formatLir result.Value; Stdout = result.Stdout }
        | Error diagnostics -> Error diagnostics.Head.Code

    let go source =
        let files = match GoBackend.generate { ModuleName = "fscript.generated/conformance" } (compile source).Lir with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let directory = Path.Combine(Path.GetTempPath(), $"fscript-conformance-{Guid.NewGuid():N}")
        try
            for file in files do
                let path = Path.Combine(directory, file.RelativePath)
                match Path.GetDirectoryName(path) with
                | null -> ()
                | parent -> Directory.CreateDirectory(parent) |> ignore
                File.WriteAllText(path, file.Content)
            let start = ProcessStartInfo("go")
            start.WorkingDirectory <- directory; start.ArgumentList.Add("run"); start.ArgumentList.Add(".")
            start.RedirectStandardOutput <- true; start.RedirectStandardError <- true
            use proc =
                match Process.Start(start) with
                | null -> failwith "Could not start the Go toolchain"
                | started -> started
            let output = proc.StandardOutput.ReadToEnd().Trim()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            if proc.ExitCode = 0 then
                let lines = output.Replace("\r\n", "\n").Split('\n') |> Array.toList
                match List.rev lines with
                | value :: reversedOutput -> Ok { Value = value; Stdout = List.rev reversedOutput }
                | [] -> Ok { Value = ""; Stdout = [] }
            else Error error
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)

[<TestFixture>]
type ConformanceTests() =
    [<OneTimeSetUp>]
    member _.``Require Go toolchain`` () =
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

    [<TestCase("[Some 1; None] = [Some 1; None]")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet age (person: Person) = person.Age\nage {| Name = \"Ada\"; Age = 42 |}")>]
    [<TestCase("type Shape = | Point | Circle of int\nmatch Circle 3 with | Point -> 0 | Circle radius -> radius")>]
    [<TestCase("let identity value = value\nidentity 42")>]
    [<TestCase("(1, 2)")>]
    [<TestCase("List.fold (fun state value -> state + value) 0 [1; 2; 3]")>]
    [<TestCase("let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 10")>]
    [<TestCase("match { [\"answer\"] = 42 }[\"answer\"] with | Some value -> value | None -> 0")>]
    [<TestCase("Console.writeLine \"hello\"")>]
    [<TestCase("[Some (1, [2; 3]); None] = [Some (1, [2; 3]); None]")>]
    [<TestCase("type Person = { Name: string; Age: int }\n{ Name = \"Ada\"; Age = 42 } = {| Name = \"Ada\"; Age = 42 |}")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\n{ person with Age = 43 }.Age")>]
    [<TestCase("type Person = { Name: string }\ntype Lookup = | Missing | Found of Person\nFound { Name = \"Ada\" } = Found { Name = \"Ada\" }")>]
    [<TestCase("{ [\"b\"] = [2]; [\"a\"] = [1] } = { [\"a\"] = [1]; [\"b\"] = [2] }")>]
    [<TestCase("let applyTwice fn value = fn (fn value)\nlet add amount = fun value -> value + amount\napplyTwice (add 2) 3")>]
    [<TestCase("let identity value = value\n(identity [1; 2], identity (Some \"ok\"))")>]
    [<TestCase("match [1; 2] with | head::tail when List.length tail = 1 -> head | _ -> 0")>]
    [<TestCase("match { [\"a\"] = \"a\" } with | { [same] = same } -> true | _ -> false")>]
    [<TestCase("(Int.tryParse \"42\", Int.tryParse \"x\", Float.tryParse \"1.5\", Bool.tryParse \"TRUE\", Bool.tryParse \"1\", Int.toString 42, Float.toString 1.5, Bool.toString false, String.replace \"a\" \"o\" \"banana\", String.indexOf \"x\" \"éx\", String.toLower \"ABC\", String.toUpper \"abc\", String.substring 1 1 \"aéz\", String.substring 8 1 \"short\", String.concat \",\" [\"a\"; \"b\"], String.split \",\" \"a,b\", String.endsWith \"bar\" \"foobar\")")>]
    [<TestCase("(List.map (fun value -> value + 1) [1; 2], List.iter (fun value -> ignore value) [1; 2], List.choose (fun value -> if value > 1 then Some value else None) [1; 2], List.collect (fun value -> [value; value]) [1; 2], List.exists (fun value -> value = 2) [1; 2], List.contains 2 [1; 2], List.rev [1; 2], List.distinct [1; 1; 2], List.fold (fun state value -> state + value) 0 [1; 2], List.filter (fun value -> value > 1) [1; 2], List.length [1; 2], List.tryFind (fun value -> value > 1) [1; 2], List.tryGet (fun value -> value = 2) [1; 2], List.tryItem 1 [1; 2], List.tryHead [1; 2], List.tail [1; 2], List.append [1] [2])")>]
    [<TestCase("(Option.defaultValue 7 None, Option.defaultWith (fun _ -> 8) None, Option.isNone None, Option.isSome (Some 1), Option.map (fun value -> value + 1) (Some 1))")>]
    [<TestCase("(Map.count Map.empty, Map.tryGet \"a\" (Map.ofList [(\"a\", 1)]), Map.containsKey \"a\" (Map.ofList [(\"a\", 1)]), Map.add \"b\" 2 (Map.ofList [(\"a\", 1)]), Map.fold (fun state key value -> state + value) 0 (Map.ofList [(\"a\", 1); (\"b\", 2)]), Map.filter (fun key value -> value > 1) (Map.ofList [(\"a\", 1); (\"b\", 2)]), Map.choose (fun key value -> if value > 1 then Some value else None) (Map.ofList [(\"a\", 1); (\"b\", 2)]), Map.map (fun value -> value + 1) (Map.ofList [(\"a\", 1)]), Map.iter (fun key value -> ignore value) (Map.ofList [(\"a\", 1)]), Map.remove \"a\" (Map.ofList [(\"a\", 1); (\"b\", 2)]))")>]
    member _.``Legacy LIR artifact and Go results conform`` (source: string) =
        let expected = Harness.legacy source
        match Harness.lir source with
        | Ok actual ->
            Assert.That(actual.Value, Is.EqualTo(expected.Value), source)
            Assert.That(System.String.Join("\n", actual.Stdout), Is.EqualTo(System.String.Join("\n", expected.Stdout)), source)
        | Error code -> Assert.Fail($"LIR failed with {code}: {source}")
        match Harness.go source with
        | Ok actual ->
            Assert.That(actual.Value, Is.EqualTo(expected.Value), source)
            Assert.That(System.String.Join("\n", actual.Stdout), Is.EqualTo(System.String.Join("\n", expected.Stdout)), source)
        | Error error -> Assert.Fail($"Go failed with {error}: {source}")

    [<Test>]
    member _.``Failure categories conform`` () =
        let source = "raise \"boom\""
        Assert.Throws<EvalException>(fun () -> Harness.legacy source |> ignore) |> ignore
        match Harness.lir source with
        | Error code -> Assert.That(code, Is.EqualTo("FSI3011"))
        | Ok value -> Assert.Fail($"Expected LIR failure, got {value}")
        match Harness.go source with
        | Ok value -> Assert.Fail($"Expected generated Go failure, got {value}")
        | Error error -> Assert.That(error, Does.Contain("boom"))
