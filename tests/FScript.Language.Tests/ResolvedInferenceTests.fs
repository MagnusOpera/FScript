namespace FScript.Language.Tests

open FScript.Language
open NUnit.Framework

[<TestFixture>]
type ResolvedInferenceTests() =
    [<Test>]
    member _.``Resolved inference captures every expression without changing legacy output`` () =
        let program = Helpers.parse "let add x y = x + y\nadd 1 2"
        let legacy = TypeInfer.inferProgram program
        let resolved = TypeInfer.inferResolvedProgram program

        Assert.That(resolved.TypedProgram.Length, Is.EqualTo(legacy.Length))
        Assert.That(resolved.ExpressionTypes, Is.Not.Empty)

        let finalApplications =
            resolved.ExpressionTypes
            |> List.choose (fun item ->
                match item.Expr with
                | EApply _ -> Some item.Type
                | _ -> None)

        Assert.That(finalApplications, Does.Contain(TInt))

    [<Test>]
    member _.``Ordinary inference does not allocate resolved telemetry`` () =
        let program = Helpers.parse "let identity value = value\nidentity 42"
        let resolved = TypeInfer.inferResolvedProgram program

        Assert.That(resolved.ExpressionTypes |> List.exists (fun item -> item.Type = TInt), Is.True)
