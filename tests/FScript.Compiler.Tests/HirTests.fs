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
        Assert.That(Hir.format { compiled.Hir with Types = []; Declarations = declarations }, Is.EqualTo(snapshot "add.hir"))

    [<Test>]
    member _.``Canonical LIR add snapshot is stable`` () =
        let compiled = compiledAdd ()
        let declarations = compiled.Lir.Declarations |> List.filter (fun declaration -> declaration.Name = "add" || declaration.Name.StartsWith("$expression"))
        Assert.That(Lir.format { compiled.Lir with Types = []; Declarations = declarations }, Is.EqualTo(snapshot "add.lir"))

    [<Test>]
    member _.``HIR and LIR preserve named type declarations`` () =
        let source = "type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\nperson.Name"
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        let hirType = compiled.Hir.Types |> List.find (fun item -> item.Name = "Person")
        let lirType = compiled.Lir.Types |> List.find (fun item -> item.Name = "Person")
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
        let choiceType = compiled.Hir.Types |> List.find (fun item -> item.Name = "Choice")
        Assert.That(choiceType.Id |> Hir.symbolNumber, Is.EqualTo(resolvedId TypeInfer.TypeSymbol "Choice"))
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
        let personType = compiled.Hir.Types |> List.find (fun item -> item.Name = "Person")
        Assert.That((personType.Attributes |> List.map _.Name) = [ "Import" ], Is.True)
        let answer = compiled.Hir.Declarations |> List.find (fun declaration -> declaration.Name = "answer")
        Assert.That((answer.Attributes |> List.map _.Name) = [ "Export" ], Is.True)

    [<Test>]
    member _.``HIR retains custom attributes on ordinary declarations`` () =
        let source = "[<BackendHint(\"fast\", stable)>]\ntype Person = { Name: string }\n[<Trace(\"answer\")>] let answer = 42"
        let compiled = match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with Ok value -> value | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        let personType = compiled.Hir.Types |> List.find (fun item -> item.Name = "Person")
        Assert.That(personType.Attributes.Head.Name, Is.EqualTo("BackendHint"))
        Assert.That(personType.Attributes.Head.Arguments = [ "fast"; "stable" ], Is.True)
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
    member _.``HIR validation rejects unknown semantic types and every unresolved symbol ID`` () =
        let unknown = ({ Type = TNamed "Missing"; Span = span; Node = Hir.NoneValue }: Hir.Expression)
        match HirValidation.validate { Types = []; ImportedRecords = []; Declarations = []; Entry = Some unknown } with
        | Ok _ -> Assert.Fail("Expected unknown HIR type diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSH1109"))

        let reference = ({ Type = TUnit; Span = span; Node = Hir.Reference(Hir.SymbolId 0) }: Hir.Expression)
        let declaration: Hir.Declaration =
            { Id = Hir.SymbolId 10; Name = "value"; Scheme = Forall([], TUnit)
              Attributes = []; IsRecursive = false; IsExported = false; Export = None; Span = span; Value = reference }
        match HirValidation.validate { Types = []; ImportedRecords = []; Declarations = [ declaration ]; Entry = None } with
        | Ok _ -> Assert.Fail("Expected unresolved HIR symbol diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSH1102"))

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
    member _.``LIR validation rejects unknown types and invalid nested spans`` () =
        let unknown = ({ Type = TNamed "Missing"; Span = span; Node = Lir.NoneValue }: Lir.Expression)
        match LirValidation.validate { Types = []; ImportedRecords = []; Functions = []; Declarations = []; Entry = Some unknown } with
        | Ok _ -> Assert.Fail("Expected unknown LIR type diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2017"))

        let invalidSpan = Span.mk (Span.pos -1 0) (Span.pos -1 1)
        let child = ({ Type = TInt; Span = invalidSpan; Node = Lir.Constant(Lir.Int 1L) }: Lir.Expression)
        let tuple = ({ Type = TTuple [ TInt ]; Span = span; Node = Lir.Tuple [ child ] }: Lir.Expression)
        match LirValidation.validate { Types = []; ImportedRecords = []; Functions = []; Declarations = []; Entry = Some tuple } with
        | Ok _ -> Assert.Fail("Expected nested LIR span diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2002"))

    [<Test>]
    member _.``Initial LIR optimization pass is identity`` () =
        let expression = ({ Type = TInt; Span = span; Node = Lir.Constant(Lir.Int 1L) }: Lir.Expression)
        let program: Lir.Program = { Types = []; ImportedRecords = []; Functions = []; Declarations = []; Entry = Some expression }
        Assert.That(LirOptimization.optimize program = program, Is.True)

    [<Test>]
    member _.``LIR lowers patterns into projection and check plans`` () =
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram "match Some (1, 2) with | Some (left, right) -> left + right | None -> 0"; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        let decision =
            compiled.Lir.Declarations
            |> List.choose (fun declaration -> match declaration.Value.Node with Lir.DecisionTree(_, cases) -> Some cases | _ -> None)
            |> List.exactlyOne
        let first = decision.Head.Plan
        Assert.That(first.Checks |> List.exists (function Lir.SomeCheck [] -> true | _ -> false), Is.True)
        Assert.That(first.Checks |> List.exists (function Lir.TupleArityCheck([ Lir.OptionValue ], 2) -> true | _ -> false), Is.True)
        let projections = first.Bindings |> List.map _.Projection
        let expected =
            [ [ Lir.OptionValue; Lir.TupleItem 0 ]
              [ Lir.OptionValue; Lir.TupleItem 1 ] ]
        Assert.That((projections = expected), Is.True)

    [<Test>]
    member _.``Compiler emits intrinsic types and validates their union payload plans`` () =
        let source = "match Fs.kind \".\" with | FsKind.Directory path -> path | _ -> \"missing\""
        let compiled =
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Ok value -> value
            | Error diagnostics -> failwith (System.String.Join("; ", diagnostics |> List.map _.Message))
        Assert.That(compiled.Lir.Types |> List.map _.Name, Does.Contain("Environment"))
        Assert.That(compiled.Lir.Types |> List.map _.Name, Does.Contain("FsKind"))
        let fsKind = compiled.Lir.Types |> List.find (fun item -> item.Name = "FsKind")
        match fsKind.Definition with
        | TUnion(_, cases) -> Assert.That(cases |> Map.containsKey "Directory", Is.True)
        | _ -> Assert.Fail("Expected intrinsic FsKind union")

    [<Test>]
    member _.``LIR validates recursive named representations without recursion overflow`` () =
        let source = "type rec Tree = | Leaf of int | Branch of (Tree * Tree)\nlet rec sum tree = match tree with | Leaf value -> value | Branch (left, right) -> sum left + sum right\nsum (Branch (Leaf 1, Leaf 2))"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Ok compiled -> Assert.That(compiled.Lir.Types |> List.map _.Name, Does.Contain("Tree"))
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
