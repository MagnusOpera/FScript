namespace FScript.Compiler.Tests

open System.IO
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type HirTests() =
    let span = Span.mk (Span.pos 1 0) (Span.pos 1 1)
    let snapshot name = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Snapshots", name)).Replace("\r\n", "\n")
    let compiledAdd () =
        match Compiler.compile { Program = Parser.parseProgram "let add x y = x + y\nadd 1 2"; Externs = [] } with
        | Ok value -> value
        | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Compiler catalog describes every default runtime external without delegates`` () =
        let expected =
            [ "Console.readLine"; "Console.writeLine"; "Env"
              "Fs.combinePath"; "Fs.createDirectory"; "Fs.enumerateFiles"; "Fs.exists"
              "Fs.extension"; "Fs.fileNameWithoutExtension"; "Fs.glob"; "Fs.kind"
              "Fs.parentDirectory"; "Fs.readText"; "Fs.writeText"
              "Guid.new"; "Hash.md5"; "Json.deserialize"; "Json.serialize"
              "Regex.matchGroups"; "Task.await"; "Task.spawn"; "Xml.queryValues" ]
        Assert.That((CompilerExternCatalog.defaults |> List.map _.Name |> List.sort) = expected, Is.True)

        match Compiler.compile { Program = Parser.parseProgram "Fs.exists \"missing\""; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            Assert.That(
                compiled.Lir.Declarations
                |> List.exists (fun declaration ->
                    declaration.Name = "Fs.exists"
                    && match declaration.Value.Node with Lir.HostExternal("Fs.exists", 1) -> true | _ -> false),
                Is.True)

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
    member _.``Canonical HIR add snapshot is stable`` () =
        let compiled = compiledAdd ()
        let declarations = compiled.Hir.Declarations |> List.filter (fun declaration -> declaration.Name = "add" || declaration.Name.StartsWith("$expression"))
        Assert.That(Hir.format { compiled.Hir with Declarations = declarations }, Is.EqualTo(snapshot "add.hir"))

    [<Test>]
    member _.``Canonical LIR add snapshot is stable`` () =
        let compiled = compiledAdd ()
        let declarations = compiled.Lir.Declarations |> List.filter (fun declaration -> declaration.Name = "add" || declaration.Name.StartsWith("$expression"))
        Assert.That(Lir.format { compiled.Lir with Declarations = declarations }, Is.EqualTo(snapshot "add.lir"))

    [<Test>]
    member _.``HIR and LIR preserve named type declarations`` () =
        let source = "type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nperson.Name"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        let hirType = compiled.Hir.Types |> List.exactlyOne
        let lirType = compiled.Lir.Types |> List.exactlyOne
        Assert.That(hirType.Name, Is.EqualTo("Person"))
        Assert.That((match hirType.Definition with TRecord _ -> true | _ -> false), Is.True)
        Assert.That(lirType.Name, Is.EqualTo("Person"))
        Assert.That(compiled.Hir.Declarations |> List.exists (fun declaration -> declaration.Value.Type = TNamed "Person"), Is.True)

    [<Test>]
    member _.``HIR reuses deterministic resolved frontend symbol IDs`` () =
        let source = "let first = 1\ntype Choice = | Pick of int\nlet apply x = (let y = x\n    match Some y with\n    | Some z -> z\n)\napply first"
        let program = Parser.parseProgram source
        let external name scheme arity = ({ Name = name; Scheme = scheme; Arity = arity; Impl = fun _ _ -> VUnit }: ExternalFunction)
        let frontendExterns =
            CompilerExternCatalog.defaults
            |> List.map (fun descriptor -> external descriptor.Name descriptor.Scheme descriptor.Arity)
        let resolved = TypeInfer.inferResolvedProgramWithExterns frontendExterns program
        let compiled = match Compiler.compile { Program = program; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        let resolvedId kind name = resolved.Symbols |> List.find (fun symbol -> symbol.Kind = kind && symbol.Name = name) |> _.SymbolId
        let declarationId name = compiled.Hir.Declarations |> List.find (fun declaration -> declaration.Name = name) |> _.Id |> Hir.symbolNumber
        Assert.That(declarationId "first", Is.EqualTo(resolvedId TypeInfer.DeclarationSymbol "first"))
        Assert.That(declarationId "Choice.Pick", Is.EqualTo(resolvedId TypeInfer.UnionCaseSymbol "Choice.Pick"))
        Assert.That(declarationId "apply", Is.EqualTo(resolvedId TypeInfer.DeclarationSymbol "apply"))
        Assert.That(compiled.Hir.Types.Head.Id |> Hir.symbolNumber, Is.EqualTo(resolvedId TypeInfer.TypeSymbol "Choice"))
        let rec bindings (expression: Hir.Expression) =
            let patternBindings (pattern: Hir.Pattern) =
                let rec collect (pattern: Hir.Pattern) =
                    match pattern.Node with
                    | Hir.Binding(id, name) -> [ name, Hir.symbolNumber id ]
                    | Hir.SomePattern item -> collect item
                    | _ -> []
                collect pattern
            match expression.Node with
            | Hir.Lambda(id, name, _, body) -> (name, Hir.symbolNumber id) :: bindings body
            | Hir.Let(id, name, _, value, body) -> (name, Hir.symbolNumber id) :: (bindings value @ bindings body)
            | Hir.Match(target, cases) -> bindings target @ (cases |> List.collect (fun case -> patternBindings case.Pattern @ bindings case.Body))
            | Hir.Apply(left, right) -> bindings left @ bindings right
            | _ -> []
        let applyBindings = compiled.Hir.Declarations |> List.find (fun declaration -> declaration.Name = "apply") |> _.Value |> bindings |> Map.ofList
        Assert.That(applyBindings.["x"], Is.EqualTo(resolvedId TypeInfer.ParameterSymbol "x"))
        Assert.That(applyBindings.["y"], Is.EqualTo(resolvedId TypeInfer.LocalSymbol "y"))
        Assert.That(applyBindings.["z"], Is.EqualTo(resolvedId TypeInfer.PatternBindingSymbol "z"))

    [<Test>]
    member _.``HIR consumes normalized resolved declaration schemes`` () =
        TypeInfer.inferProgram (Parser.parseProgram "let perturb left right = (left, right)") |> ignore
        let compiled = match Compiler.compile { Program = Parser.parseProgram "let identity value = value\nidentity 42"; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        let identity = compiled.Hir.Declarations |> List.find (fun declaration -> declaration.Name = "identity")
        Assert.That(identity.Scheme, Is.EqualTo(Forall([ 0 ], TFun(TVar 0, TVar 0))))

    [<Test>]
    member _.``HIR lowering consumes only the semantic resolved program`` () =
        let program = Parser.parseProgram "let add x y = x + y\nadd 1 2"
        let inferenceExterns =
            CompilerExternCatalog.defaults
            |> List.map (fun descriptor ->
                ({ Name = descriptor.Name
                   Scheme = descriptor.Scheme
                   Arity = descriptor.Arity
                   Impl = fun _ _ -> VUnit }: ExternalFunction))
        let resolved = TypeInfer.inferResolvedProgramWithExterns inferenceExterns program
        let detached = { resolved with TypedProgram = []; ExpressionTypes = [] }
        match HirLowering.lower CompilerExternCatalog.defaults detached with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok hir ->
            Assert.That(hir.Declarations |> List.exists (fun declaration -> declaration.Name = "add"), Is.True)
            Assert.That(Hir.format hir, Does.Contain("primitive IntAdd"))

    [<Test>]
    member _.``HIR attaches structured export and import attributes`` () =
        let source = "[<Import(\"example.com/model\", \"Person\")>] type Person = { Name: string }\n[<Export>] let answer = 42"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        Assert.That((compiled.Hir.Types.Head.Attributes |> List.map _.Name) = [ "Import" ], Is.True)
        let answer = compiled.Hir.Declarations |> List.find (fun declaration -> declaration.Name = "answer")
        Assert.That((answer.Attributes |> List.map _.Name) = [ "Export" ], Is.True)

    [<Test>]
    member _.``HIR retains custom attributes on ordinary declarations`` () =
        let source = "[<BackendHint(\"fast\", stable)>]\ntype Person = { Name: string }\n[<Trace(\"answer\")>] let answer = 42"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        Assert.That(compiled.Hir.Types.Head.Attributes.Head.Name, Is.EqualTo("BackendHint"))
        Assert.That(compiled.Hir.Types.Head.Attributes.Head.Arguments = [ "fast"; "stable" ], Is.True)
        let answer = compiled.Hir.Declarations |> List.find (fun declaration -> declaration.Name = "answer")
        Assert.That(answer.Attributes.Head.Name, Is.EqualTo("Trace"))

    [<Test>]
    member _.``HIR validation rejects a declaration scheme mismatch`` () =
        let value: Hir.Expression = { Type = TInt; Span = span; Node = Hir.Constant(Hir.Int 1L) }
        let declaration: Hir.Declaration =
            { Id = Hir.SymbolId 0; Name = "value"; Scheme = Forall([], TString)
              Attributes = []; IsRecursive = false; IsExported = false; Export = None; Span = span; Value = value }
        match HirValidation.validate { Types = []; ImportedRecords = []; Declarations = [ declaration ]; Entry = None } with
        | Ok _ -> Assert.Fail("Expected invalid HIR diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSH1104"))

    [<Test>]
    member _.``LIR validation rejects inconsistent control flow`` () =
        let constant valueType value = ({ Type = valueType; Span = span; Node = Lir.Constant value }: Lir.Expression)
        let branch: Lir.Expression =
            { Type = TInt; Span = span
              Node = Lir.Branch(constant TInt (Lir.Int 1L), constant TInt (Lir.Int 2L), constant TString (Lir.String "no")) }
        match LirValidation.validate { Types = []; ImportedRecords = []; Functions = []; Declarations = []; Entry = Some branch } with
        | Ok _ -> Assert.Fail("Expected invalid LIR diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2007"))

    [<Test>]
    member _.``LIR validation rejects unknown primitives`` () =
        let intrinsic = ({ Type = TInt; Span = span; Node = Lir.Intrinsic "Unknown.value" }: Lir.Expression)
        match LirValidation.validate { Types = []; ImportedRecords = []; Functions = []; Declarations = []; Entry = Some intrinsic } with
        | Ok _ -> Assert.Fail("Expected unavailable primitive diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2009"))

    [<Test>]
    member _.``Initial LIR optimization pass is identity`` () =
        let expression = ({ Type = TInt; Span = span; Node = Lir.Constant(Lir.Int 1L) }: Lir.Expression)
        let program: Lir.Program = { Types = []; ImportedRecords = []; Functions = []; Declarations = []; Entry = Some expression }
        Assert.That(LirOptimization.optimize program = program, Is.True)
