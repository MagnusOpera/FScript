namespace FScript.Conformance.Tests

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open FsCheck
open FsCheck.FSharp
open FScript.Compiler
open FScript.Language
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open NUnit.Framework

module internal BackendProperties =
    let defaultReplay seed = { Rnd=Rnd seed; Size=None }
    let parseReplay (text: string) =
        match text.Split(',') with
        | [|seed; gamma|] -> { Rnd=Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size=None }
        | [|seed; gamma; size|] ->
            let size = Int32.Parse size
            if size < 0 then invalidArg "text" "Replay size must be nonnegative"
            { Rnd=Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size=Some size }
        | _ -> invalidArg "text" "FSCRIPT_BACKEND_REPLAY must be seed,gamma or seed,gamma,size"

    let run replay count arbitrary property =
        let mutable result = None
        let runner =
            { new IRunner with
                member _.OnStartFixture _ = ()
                member _.OnArguments(_,_,_) = ()
                member _.OnShrink(_,_) = ()
                member _.OnFinished(_,value) = result <- Some value }
        Check.One(Config.Quick.WithMaxTest(count).WithStartSize(0).WithEndSize(32).WithReplay(Some replay).WithRunner(runner), Prop.forAll arbitrary property)
        result |> Option.defaultWith (fun () -> failwith "FsCheck did not report a result")

    let report shape seed result =
        let describe value =
            let p = unbox<BackendPrograms.Program> value
            $"Backend generator version: 1\nShape: {p.Shape}\nSource:\n{BackendPrograms.source p}\nTree: {p}"
        let details = Runner.onFinishedToStringWithFormatter describe "Backend conformance" result
        match result with
        | TestResult.Failed(_,_,_,_,_,rnd,size) ->
            details + $"\nReplay:\nFSCRIPT_BACKEND_REPLAY='{rnd.Seed},{rnd.Gamma},{size}' dotnet test tests/FScript.Conformance.Tests/FScript.Conformance.Tests.fsproj -c Release --filter 'Name~{shape}_{seed}'"
        | _ -> details

    let private references = lazy (
        let paths = (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") |> nonNull) :?> string
        (paths.Split(Path.PathSeparator) |> Array.toList) @ [typeof<unit>.Assembly.Location]
        |> List.distinct |> List.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference))

    let private csharp program =
        let files =
            match CSharpBackend.generate { Namespace="Generated"; ClassName="Program"; References=[] } program with
            | Ok files -> files
            | Error errors -> failwithf "C# generation failed: %A" errors
        let trees = files |> List.filter (fun file -> file.RelativePath.EndsWith(".cs", StringComparison.Ordinal))
                          |> List.map (fun file -> CSharpSyntaxTree.ParseText(file.Content, CSharpParseOptions(LanguageVersion.Latest), file.RelativePath))
        let options = CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel=OptimizationLevel.Release, deterministic=true, nullableContextOptions=NullableContextOptions.Enable)
        let compilation = CSharpCompilation.Create("PropertyCSharp", trees, references.Value, options)
        use output = new MemoryStream()
        let emitted = compilation.Emit output
        let diagnostics = emitted.Diagnostics |> Seq.filter (fun d -> d.Severity=DiagnosticSeverity.Warning || d.Severity=DiagnosticSeverity.Error) |> Seq.toList
        if not emitted.Success || not diagnostics.IsEmpty then failwithf "C# compilation failed: %A" diagnostics
        output.ToArray()

    let private dotnet program =
        let options = { AssemblyName="PropertyDotNet"; Namespace="Generated"; ClassName="Program"; OutputKind=Library; References=[] }
        match DotNetBackend.generate options program with
        | Ok files -> (files |> List.find (fun file -> file.RelativePath="PropertyDotNet.dll")).Content
        | Error errors -> failwithf ".NET generation failed: %A" errors

    let rec private unwrap (error: exn) =
        match error with
        | :? TargetInvocationException as e when not (isNull e.InnerException) -> unwrap (nonNull e.InnerException)
        | :? TypeInitializationException as e when not (isNull e.InnerException) -> unwrap (nonNull e.InnerException)
        | _ -> error

    // Only expected script failures are converted. Build/load/lookup exceptions
    // must fail the property, rather than accidentally agreeing as "raised".
    let private classify expectedMessage (error: exn) =
        match unwrap error with
        | :? DivideByZeroException -> Error Harness.DivisionByZero
        | EvalException error when error.Message="No match cases matched" -> Error Harness.MatchFailure
        | EvalException error when error.Message=expectedMessage -> Error Harness.Raised
        | :? InvalidOperationException as error when error.Message="Pattern match failure" -> Error Harness.MatchFailure
        | error -> raise error

    let private execute bytes =
        let context = new AssemblyLoadContext("FScript backend property", isCollectible=true)
        let original = Console.Out
        let lines = ResizeArray<string>()
        use stdout = { new StringWriter(Globalization.CultureInfo.InvariantCulture) with
                           override _.WriteLine(value: string) = lines.Add value }
        try
            use stream = new MemoryStream(bytes, false)
            let assembly = context.LoadFromStream stream
            let typ = assembly.GetType("Generated.Program", throwOnError=true) |> nonNull
            // Capture only Run, so generation/load failures are not script failures.
            Console.SetOut stdout
            let value = typ.GetMethod("Run") |> nonNull |> fun method -> method.Invoke(null, [||])
            let formatted = (typ.GetMethod("FormatResult") |> nonNull).Invoke(null, [|value|]) |> nonNull |> fun value -> value :?> string
            Ok { Harness.Value=formatted; Harness.Stdout=List.ofSeq lines }
        finally
            Console.SetOut original
            context.Unload()

    let private evaluateNative expectedMessage bytes =
        try execute bytes with
        | error ->
            match unwrap error with
            | :? InvalidOperationException as error when error.Message=expectedMessage -> Error Harness.Raised
            | _ -> classify expectedMessage error

    type private Scalar = Integer of int | Boolean of bool | Text of string
    let rec private scalarValue x = function
        | BackendPrograms.Number n -> Integer n
        | BackendPrograms.Flag b -> Boolean b
        | BackendPrograms.String s -> Text s
        | BackendPrograms.Variable -> Integer x
        | BackendPrograms.Add(a,b) -> Integer(integerValue x a + integerValue x b)
        | BackendPrograms.Subtract(a,b) -> Integer(integerValue x a - integerValue x b)
        | BackendPrograms.Equal(a,b) -> Boolean(scalarValue x a = scalarValue x b)
        | BackendPrograms.Choose(c,a,b) ->
            match scalarValue x c with
            | Boolean condition -> scalarValue x (if condition then a else b)
            | _ -> failwith "Invalid boolean tree"
        | BackendPrograms.Apply(a,b) -> scalarValue (integerValue x a) b
    and private integerValue x tree =
        match scalarValue x tree with Integer n -> n | _ -> failwith "Invalid integer tree"
    let private textValue tree =
        match scalarValue 0 tree with Text s -> s | _ -> failwith "Invalid text tree"

    let check p =
        let source = BackendPrograms.source p
        let program = Harness.compile source
        let expectedMessage = textValue p.Text
        let expected = try Ok(Harness.legacy source) with error -> classify expectedMessage error
        let lir =
            match Harness.lir source with
            | Ok value -> Ok value
            | Error "FSI3005" -> Error Harness.DivisionByZero
            | Error "FSI3012" -> Error Harness.MatchFailure
            | Error "FSI3011" | Error "FSI3022" -> Error Harness.Raised
            | Error code -> failwithf "Unexpected LIR diagnostic %s" code
        let cs = csharp program.Lir |> evaluateNative expectedMessage
        let net = dotnet program.Lir |> evaluateNative expectedMessage
        let equal target actual =
            if actual <> expected then failwithf "%s differs\nSource: %A\n%s: %A" target expected target actual
        equal "LIR" lir
        equal "C#" cs
        equal ".NET" net
        // Require that failure programs actually fail with their intended category.
        let intended =
            match p.Shape with
            | BackendPrograms.Raised -> Some Harness.Raised
            | BackendPrograms.DivideByZero -> Some Harness.DivisionByZero
            | _ -> None
        match intended, expected with
        | Some category, Error actual when category=actual -> ()
        | None, Ok _ -> ()
        | _ -> failwithf "Generator violated expected outcome: %A, %A" intended expected
        true

[<TestFixture; NonParallelizable>]
type BackendPropertyTests() =
    static member Cases =
        [for shape in BackendPrograms.shapes do
            for seed in [104729UL;130363UL] do
                yield TestCaseData(string shape, seed).SetName($"BackendProperty_{shape}_{seed}")]

    [<TestCaseSource(nameof BackendPropertyTests.Cases)>]
    member _.GeneratedProgramsAgree(shapeName: string, seed: uint64) =
        let shape = BackendPrograms.shapes |> List.find (fun s -> string s=shapeName)
        let replay =
            match Environment.GetEnvironmentVariable "FSCRIPT_BACKEND_REPLAY" with
            | null | "" -> BackendProperties.defaultReplay seed
            | value -> BackendProperties.parseReplay value
        match BackendProperties.run replay 8 (BackendPrograms.arbitrary shape) BackendProperties.check with
        | TestResult.Passed _ -> ()
        | result -> Assert.Fail(BackendProperties.report shape seed result)

    [<Test>]
    member _.GeneratedProgramsAndShrinksTypeCheck() =
        for shape in BackendPrograms.shapes do
            let property p =
                for smaller in Seq.append [p] (BackendPrograms.shrinkProgram p) do
                    if smaller.Shape <> shape then failwith "Shrinking changed program shape"
                    BackendPrograms.source smaller |> Harness.compile |> ignore
                true
            match BackendProperties.run (BackendProperties.defaultReplay 104729UL) 12 (BackendPrograms.arbitrary shape) property with
            | TestResult.Passed _ -> ()
            | result -> Assert.Fail(BackendProperties.report shape 104729UL result)
        // Force the parameter-reference case even if a seed changes later.
        let expression = BackendPrograms.Apply(BackendPrograms.Number 8,
                             BackendPrograms.Add(BackendPrograms.Variable, BackendPrograms.Number 3))
        for smaller in BackendPrograms.shrink expression do
            BackendPrograms.render smaller |> Harness.compile |> ignore

    [<Test>]
    member _.CounterexamplesShrinkAndReplay() =
        let arbitrary = BackendPrograms.arbitrary BackendPrograms.Scalars
        let property (p: BackendPrograms.Program) = p.Bound=0
        let run replay = BackendProperties.run replay 50 arbitrary property
        match run (BackendProperties.defaultReplay 104729UL) with
        | TestResult.Failed(data, original, reduced, _, _, seed, size) as failure ->
            Assert.That(data.NumberOfShrinks, Is.GreaterThan(0))
            let p = unbox<BackendPrograms.Program> (List.exactlyOne reduced)
            Assert.That(p.Bound, Is.EqualTo(1))
            match run (BackendProperties.parseReplay $"{seed.Seed},{seed.Gamma},{size}") with
            | TestResult.Failed(_, again, smaller, _, _, _, _) ->
                Assert.That(again, Is.EqualTo(original))
                Assert.That(smaller, Is.EqualTo(reduced))
            | _ -> Assert.Fail("Replay did not reproduce the injected mismatch")
            let message = BackendProperties.report BackendPrograms.Scalars 104729UL failure
            Assert.That(message, Does.Contain("FSCRIPT_BACKEND_REPLAY="))
            Assert.That(message, Does.Contain("Source:"))
        | _ -> Assert.Fail("Injected mismatch was not detected")

    [<TestCase("1,2")>]
    [<TestCase("1,3,-1")>]
    [<TestCase("1,3,0,4")>]
    member _.InvalidReplayIsRejected(input: string) =
        Assert.Throws<ArgumentException>(TestDelegate(fun () -> BackendProperties.parseReplay input |> ignore)) |> ignore
