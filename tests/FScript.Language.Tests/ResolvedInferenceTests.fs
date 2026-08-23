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

    [<Test>]
    member _.``Resolved inference assigns deterministic semantic symbols and typed references`` () =
        let program = Helpers.parse "let apply f value = f value\napply (fun item -> item + 1) 41"
        let first = TypeInfer.inferResolvedProgram program
        let second = TypeInfer.inferResolvedProgram program

        Assert.That(first.Symbols = second.Symbols, Is.True)
        Assert.That(first.SymbolReferences = second.SymbolReferences, Is.True)
        Assert.That((first.Symbols |> List.map _.SymbolId) = [ 0 .. first.Symbols.Length - 1 ], Is.True)
        Assert.That(first.Symbols |> List.exists (fun symbol -> symbol.Name = "apply" && symbol.Kind = TypeInfer.DeclarationSymbol), Is.True)
        Assert.That(first.Symbols |> List.exists (fun symbol -> symbol.Name = "item" && symbol.Kind = TypeInfer.ParameterSymbol), Is.True)
        Assert.That(first.SymbolReferences |> List.exists (fun reference -> reference.Name = "apply" && reference.Type = TFun(TFun(TInt, TInt), TFun(TInt, TInt))), Is.True)

    [<Test>]
    member _.``Resolved declaration schemes normalize type variables`` () =
        let resolved = Helpers.parse "let identity value = value\nidentity 1" |> TypeInfer.inferResolvedProgram
        let scheme =
            resolved.Symbols
            |> List.find (fun symbol -> symbol.Name = "identity" && symbol.Kind = TypeInfer.DeclarationSymbol)
            |> _.Scheme
        Assert.That(scheme, Is.EqualTo(Some(Forall([ 0 ], TFun(TVar 0, TVar 0)))))
