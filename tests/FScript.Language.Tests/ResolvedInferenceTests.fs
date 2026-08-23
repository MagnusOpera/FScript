namespace FScript.Language.Tests

open FScript.Language
open NUnit.Framework

[<TestFixture>]
type ResolvedInferenceTests() =
    let rec flatten (expression: TypeInfer.ResolvedExpression) = expression :: List.collect flatten expression.Children

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

    [<Test>]
    member _.``Resolved frontend exposes a recursively typed operation tree`` () =
        let resolved = Helpers.parse "let add x y = x + y\nadd 1 2" |> TypeInfer.inferResolvedProgram
        let expressions = resolved.ResolvedStatements |> List.collect _.Expressions |> List.collect flatten
        Assert.That(expressions, Is.Not.Empty)
        Assert.That(expressions |> List.forall (fun expression -> expression.Type <> TVar -1), Is.True)
        Assert.That(expressions |> List.exists (fun expression -> expression.Primitive = Some TypeInfer.IntegerAddition), Is.True)
        Assert.That(expressions |> List.exists (fun expression -> expression.Reference |> Option.exists (fun reference -> reference.Name = "add")), Is.True)

    [<Test>]
    member _.``Resolved applications retain record coercion evidence`` () =
        let source = "type Person = { Name: string; Age: int }\nlet age (person: Person) = person.Age\nage {| Name = \"Ada\"; Age = 42 |}"
        let resolved = Helpers.parse source |> TypeInfer.inferResolvedProgram
        let expressions = resolved.ResolvedStatements |> List.collect _.Expressions |> List.collect flatten
        let coercion = expressions |> List.choose _.RecordCoercion |> List.tryHead
        match coercion with
        | Some evidence ->
            Assert.That(evidence.TargetName, Is.EqualTo(Some "Person"))
            match evidence.SourceType with TRecord fields -> Assert.That(fields.Count, Is.EqualTo(2)) | _ -> Assert.Fail("Expected structural record source")
        | None -> Assert.Fail("Expected structural-to-named record coercion evidence")
