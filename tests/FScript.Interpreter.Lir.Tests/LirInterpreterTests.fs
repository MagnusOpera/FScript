namespace FScript.Interpreter.Lir.Tests

open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type LirInterpreterTests() =
    let errorText (value: obj) =
        match value with
        | :? CompilerDiagnostic as diagnostic -> diagnostic.Message
        | :? (CompilerDiagnostic list) as diagnostics -> System.String.Join("; ", diagnostics |> List.map _.Message)
        | _ -> string value

    [<Test>]
    member _.``Captures portable console output`` () =
        match Compiler.compile { Program = Parser.parseProgram "Console.writeLine \"hello\""; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
            | Ok result ->
                Assert.That(result.Value, Is.EqualTo(LirValue.VUnit))
                Assert.That(System.String.Join("\n", result.Stdout), Is.EqualTo("hello"))


    [<Test>]
    member _.``Executes aggregate expressions in LIR`` () =
        let input = { Program = Parser.parseProgram "[1] @ [2; 3]"; Externs = [] }
        match Compiler.compile input with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 2L; LirValue.VInt 3L ]))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<TestCase("false && raise \"and-right\"", false)>]
    [<TestCase("true || raise \"or-right\"", true)>]
    member _.``Logical operations short circuit in LIR`` (source: string, expected: bool) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VBool expected))
            | Error diagnostic -> Assert.Fail(errorText diagnostic)

    [<TestCase("(1, true)")>]
    [<TestCase("Some 4")>]
    [<TestCase("{| Age = 42 |}.Age")>]
    member _.``Lowers tuples options and records`` (source: string) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Pass()
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Executes lexical let bindings and recursive closures`` () =
        let source = "let f x = (let rec loop n = if n = 0 then x else loop (n - 1)\n    loop 2\n)\nf 7"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 7L))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Raise preserves an interpreter diagnostic`` () =
        match Compiler.compile { Program = Parser.parseProgram "raise \"boom\""; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected failure")
            | Error diagnostics ->
                Assert.That(diagnostics.Code, Is.EqualTo("FSI3011"))
                Assert.That(diagnostics.Message, Is.EqualTo("boom"))

    [<TestCase("match [1;2] with\n    | x::xs -> x\n    | [] -> 0", 1L)>]
    [<TestCase("match Some 3 with\n    | Some x -> x + 1\n    | None -> 0", 4L)>]
    [<TestCase("match (1, true) with\n    | (x, true) -> x\n    | _ -> 0", 1L)>]
    [<TestCase("match [1] with\n    | x::xs when x < 0 -> x\n    | _ -> 42", 42L)>]
    member _.``Executes deterministic pattern decision trees`` (source: string, expected: int64) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt expected))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Executes ranges indexing updates and interpolation`` () =
        let cases =
            [ "[1..3]", LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 2L; LirValue.VInt 3L ]
              "[1; 2][1]", LirValue.VOption(Some(LirValue.VInt 2L))
              "let value = {| Count = 1 |}\n{| value with Count = 2 |}.Count", LirValue.VInt 2L
              "$\"value {1 + 2}\"", LirValue.VString "value 3" ]
        for source, expected in cases do
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
            | Ok compiled ->
                match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
                | Ok result -> Assert.That(result.Value, Is.EqualTo(expected), source)
                | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Executes declared union construction and matching`` () =
        let source = "type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Point -> 0\n    | Circle r -> r\nradius (Circle 3)"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 3L))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Executes ordered map construction spread and indexing`` () =
        let source = "let tail = { [\"a\"] = 2; [\"b\"] = 3 }\nlet values = { [\"a\"] = 1; ..tail }\nvalues[\"b\"]"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VOption(Some(LirValue.VInt 3L))))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Initializes mutual recursive groups atomically`` () =
        let source = "let rec even n =\n    if n = 0 then true else odd (n - 1)\nand odd n =\n    if n = 0 then false else even (n - 1)\n\neven 10"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VBool true))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<TestCase("String.toUpper \"hello\"", "HELLO")>]
    [<TestCase("Int.toString (List.length [1; 2; 3])", "3")>]
    [<TestCase("Int.toString (Option.defaultValue 9 None)", "9")>]
    [<TestCase("Int.toString (Map.count { [\"a\"] = 1 })", "1")>]
    member _.``Executes portable scalar and collection intrinsics`` (source: string, expected: string) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VString expected))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Retains host linkage but rejects only reachable external calls`` () =
        let external : CompilerExtern =
            { Name = "Host.echo"
              Scheme = Forall([], TFun(TString, TString))
              Arity = 1
              Kind = UnsupportedHostExtern }
        let compile source = Compiler.compile { Program = Parser.parseProgram source; Externs = [ external ] }
        match compile "1" with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 1L))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
        match compile "Host.echo \"value\"" with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected unsupported external diagnostic")
            | Error diagnostics -> Assert.That(diagnostics.Code, Is.EqualTo("FSI3021"))

    [<Test>]
    member _.``Default host runtime externs remain inert until reached`` () =
        let execute source =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Error diagnostics -> Error diagnostics.Head
            | Ok compiled -> LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir

        match execute "let unused = Fs.exists\n42" with
        | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 42L))
        | Error diagnostics -> Assert.Fail(errorText diagnostics)

        match execute "Fs.exists \"missing\"" with
        | Ok _ -> Assert.Fail("Expected unsupported host-runtime diagnostic")
        | Error diagnostics -> Assert.That(diagnostics.Code, Is.EqualTo("FSI3021"))

    [<Test>]
    member _.``Retains tasks in LIR and rejects them only when reached`` () =
        let compile source = Compiler.compile { Program = Parser.parseProgram source; Externs = [] }
        match compile "let unused = Task.spawn\n1" with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 1L))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
        match compile "Task.await (Task.spawn (fun _ -> 42))" with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            Assert.That(compiled.Lir.Declarations |> List.exists (fun declaration -> match declaration.Value.Node with Lir.HostExternal("Task.spawn", 1) -> true | _ -> false), Is.True)
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected unsupported task diagnostic")
            | Error diagnostics -> Assert.That(diagnostics.Code, Is.EqualTo("FSI3021"))

    [<Test>]
    member _.``Executes local binding patterns iteration and type tokens`` () =
        let cases =
            [ "let sum pair = (let (left, right) = pair\n    left + right\n)\nsum (1, 2)", LirValue.VInt 3L
              "for value in [1; 2; 3] do ignore value", LirValue.VUnit
              "type Package = { Name: string }\ntypeof Package", LirValue.VTypeToken(TRecord(Map.ofList [ "Name", TString ])) ]
        for source, expected in cases do
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
            | Ok compiled ->
                match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
                | Ok result -> Assert.That(result.Value, Is.EqualTo(expected), source)
                | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Executes the complete portable collection intrinsic catalog`` () =
        let cases =
            [ "List.map (fun value -> value + 1) [1; 2]", LirValue.VList [ LirValue.VInt 2L; LirValue.VInt 3L ]
              "List.choose (fun value -> if value > 1 then Some value else None) [1; 2; 3]", LirValue.VList [ LirValue.VInt 2L; LirValue.VInt 3L ]
              "List.collect (fun value -> [value; value]) [1; 2]", LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 1L; LirValue.VInt 2L; LirValue.VInt 2L ]
              "List.fold (fun state value -> state + value) 0 [1; 2; 3]", LirValue.VInt 6L
              "List.tryGet (fun value -> value = 2) [1; 2; 3]", LirValue.VOption(Some(LirValue.VInt 1L))
              "Option.map (fun value -> value + 1) (Some 4)", LirValue.VOption(Some(LirValue.VInt 5L))
              "Option.defaultWith (fun _ -> 7) None", LirValue.VInt 7L
              "Map.map (fun value -> value + 1) (Map.filter (fun key value -> value > 1) (Map.ofList [(\"b\", 2); (\"a\", 1)]))", LirValue.VMap [ LirValue.VString "b", LirValue.VInt 3L ]
              "Map.fold (fun state key value -> state + value) 0 { [\"a\"] = 1; [\"b\"] = 2 }", LirValue.VInt 3L ]
        for source, expected in cases do
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
            | Ok compiled ->
                match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
                | Ok result -> Assert.That(result.Value, Is.EqualTo(expected), source)
                | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Preserves top-level pattern bindings and expression order`` () =
        match Compiler.compile { Program = Parser.parseProgram "let (left, right) = (1, 2)\nleft + right"; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 3L))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

        match Compiler.compile { Program = Parser.parseProgram "raise \"first\"\nraise \"second\""; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected the first top-level expression to fail")
            | Error diagnostics -> Assert.That(diagnostics.Message, Is.EqualTo("first"))

    [<Test>]
    member _.``Stops evaluating expression children after the first failure`` () =
        let source = "(raise \"first\", raise \"second\")"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected tuple evaluation to fail")
            | Error diagnostic -> Assert.That(diagnostic.Message, Is.EqualTo("first"))

    [<Test>]
    member _.``Validates LIR before execution`` () =
        match Compiler.compile { Program = Parser.parseProgram "1"; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            let entry = compiled.Lir.Entry.Value
            let invalid = { compiled.Lir with Entry = Some { entry with Node = Lir.Load(Lir.SymbolId 999999) } }
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } invalid with
            | Ok _ -> Assert.Fail("Expected invalid LIR to be rejected")
            | Error diagnostic -> Assert.That(diagnostic.Code, Does.StartWith("FSL2"))

    [<Test>]
    member _.``Executes local mutual recursive closure groups`` () =
        let source = "(let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 10\n)"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VBool true))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<TestCase("match { [\"b\"] = 2; [\"a\"] = 1 } with\n    | { [key] = value; ..tail } -> (key, value, Map.count tail)\n    | {} -> (\"\", 0, 0)")>]
    [<TestCase("match {| Name = \"Ada\"; Age = 42 |} with\n    | {| Name: string |} -> (\"Name\", 1, 0)\n    | _ -> (\"\", 0, 0)")>]
    member _.``Executes map and type-reference patterns`` (source: string) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Pass()
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Supplies Env from the LIR interpreter context`` () =
        let source = "(Env.ScriptName, Env.Arguments)"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = Some "sample.fss"; Arguments = [ "one"; "two" ] } compiled.Lir with
            | Ok result ->
                Assert.That(result.Value, Is.EqualTo(LirValue.VTuple [ LirValue.VOption(Some(LirValue.VString "sample.fss")); LirValue.VList [ LirValue.VString "one"; LirValue.VString "two" ] ]))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)

    [<Test>]
    member _.``Rejects only reachable target imports`` () =
        let compile source = Compiler.compile { Program = Parser.parseProgram source; Externs = [] }
        match compile "[<Import(\"strings\", \"ToUpper\")>] let upper : string -> string\n1" with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 1L))
            | Error diagnostics -> Assert.Fail(errorText diagnostics)
        match compile "[<Import(\"strings\", \"ToUpper\")>] let upper : string -> string\nupper \"hello\"" with
        | Error diagnostics -> Assert.Fail(errorText diagnostics)
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected target-import diagnostic")
            | Error diagnostics -> Assert.That(diagnostics.Code, Is.EqualTo("FSI3023"))
