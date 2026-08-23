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
