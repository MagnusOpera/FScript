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
        Assert.That(expressions |> List.exists (fun expression -> match expression.Node with TypeInfer.ResolvedPrimitive(TypeInfer.IntegerAddition, _) -> true | _ -> false), Is.True)
        Assert.That(expressions |> List.exists (fun expression -> match expression.Node with TypeInfer.ResolvedReference reference when reference.Name = "add" && reference.Kind = TypeInfer.DeclarationSymbol -> true | _ -> false), Is.True)

    [<Test>]
    member _.``Resolved patterns reuse one symbol for repeated bindings`` () =
        let source = "match { [\"a\"] = \"a\" } with\n    | { [same] = same } -> true\n    | _ -> false"
        let resolved = Helpers.parse source |> TypeInfer.inferResolvedProgram
        let rec bindingIds (pattern: TypeInfer.ResolvedPattern) =
            match pattern.Node with
            | TypeInfer.ResolvedPatternBinding binding -> [ binding.SymbolId ]
            | TypeInfer.ResolvedMapPattern(clauses, tail) ->
                (clauses |> List.collect (fun (key, value) -> bindingIds key @ bindingIds value))
                @ (tail |> Option.map bindingIds |> Option.defaultValue [])
            | _ -> []
        let ids =
            resolved.ResolvedStatements
            |> List.collect (fun statement ->
                match statement.Node with
                | TypeInfer.ResolvedExpressionStatement { Node = TypeInfer.ResolvedMatch(_, cases) } -> cases |> List.collect (_.Pattern >> bindingIds)
                | _ -> [])
        Assert.That(ids.Length, Is.EqualTo(2))
        Assert.That(ids |> List.distinct |> List.length, Is.EqualTo(1))

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

    [<Test>]
    member _.``Resolved statements preserve structured export metadata`` () =
        let resolved = Helpers.parse "[<Export>] let answer = 42" |> TypeInfer.inferResolvedProgram
        match resolved.ResolvedStatements |> List.tryPick _.Export with
        | Some export ->
            Assert.That(export.Name, Is.EqualTo("answer"))
            Assert.That(export.Span.Start.Line, Is.EqualTo(1))
        | None -> Assert.Fail("Expected resolved export metadata")
        let attributes = resolved.ResolvedStatements |> List.collect _.Attributes
        Assert.That(attributes |> List.map _.Name, Does.Contain("Export"))

    [<Test>]
    member _.``Resolved statements attach structured import attributes`` () =
        let source = "[<Import(\"example.com/model\", \"Person\")>] type Person = { Name: string }\n[<Import(\"strings\", \"ToUpper\")>] let upper : string -> string"
        let resolved = Helpers.parse source |> TypeInfer.inferResolvedProgram
        let imports = resolved.ResolvedStatements |> List.collect _.Attributes |> List.filter (fun attribute -> attribute.Name = "Import")
        Assert.That(imports.Length, Is.EqualTo(2))
        Assert.That(imports |> List.map _.Arguments, Does.Contain([ "strings"; "ToUpper" ]))
