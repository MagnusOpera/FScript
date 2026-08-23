namespace FScript.Compiler.Tests

open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type HirTests() =
    let span = Span.mk (Span.pos 1 0) (Span.pos 1 1)

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

    [<Test>]
    member _.``HIR validation rejects a declaration scheme mismatch`` () =
        let value: Hir.Expression = { Type = TInt; Span = span; Node = Hir.Constant(Hir.Int 1L) }
        let declaration: Hir.Declaration =
            { Id = Hir.SymbolId 0; Name = "value"; Scheme = Forall([], TString)
              IsRecursive = false; IsExported = false; Span = span; Value = value }
        match HirValidation.validate { Declarations = [ declaration ]; Entry = None } with
        | Ok _ -> Assert.Fail("Expected invalid HIR diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSH1104"))

    [<Test>]
    member _.``LIR validation rejects inconsistent control flow`` () =
        let constant valueType value = ({ Type = valueType; Span = span; Node = Lir.Constant value }: Lir.Expression)
        let branch: Lir.Expression =
            { Type = TInt; Span = span
              Node = Lir.Branch(constant TInt (Lir.Int 1L), constant TInt (Lir.Int 2L), constant TString (Lir.String "no")) }
        match LirValidation.validate { Declarations = []; Entry = Some branch } with
        | Ok _ -> Assert.Fail("Expected invalid LIR diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2007"))

    [<Test>]
    member _.``LIR validation rejects unknown primitives`` () =
        let intrinsic = ({ Type = TInt; Span = span; Node = Lir.Intrinsic "Unknown.value" }: Lir.Expression)
        match LirValidation.validate { Declarations = []; Entry = Some intrinsic } with
        | Ok _ -> Assert.Fail("Expected unavailable primitive diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2009"))
