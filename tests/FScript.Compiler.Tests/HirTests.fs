namespace FScript.Compiler.Tests

open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type HirTests() =
    [<Test>]
    member _.``Lowers deterministic typed add program`` () =
        let source = "let add x y = x + y\nadd 1 2"
        let input = { Program = Parser.parseProgram source; Externs = [] }
        match Compiler.compile input with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            let actual = Hir.format compiled.Hir
            Assert.That(actual, Does.Contain("decl %"))
            Assert.That(actual, Does.Contain("primitive IntAdd"))
            Assert.That(actual, Does.Contain("entry"))
            Assert.That(Hir.format compiled.Hir, Is.EqualTo(actual))
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 3L))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes aggregate expressions in LIR`` () =
        let input = { Program = Parser.parseProgram "[1] @ [2; 3]"; Externs = [] }
        match Compiler.compile input with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 2L; LirValue.VInt 3L ]))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<TestCase("(1, true)")>]
    [<TestCase("Some 4")>]
    [<TestCase("{| Age = 42 |}.Age")>]
    member _.``Lowers tuples options and records`` (source: string) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Pass()
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes lexical let bindings and recursive closures`` () =
        let source = "let f x = (let rec loop n = if n = 0 then x else loop (n - 1)\n    loop 2\n)\nf 7"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 7L))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Raise preserves an interpreter diagnostic`` () =
        match Compiler.compile { Program = Parser.parseProgram "raise \"boom\""; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected failure")
            | Error diagnostics ->
                Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSI3011"))
                Assert.That(diagnostics |> List.map _.Message, Does.Contain("boom"))
