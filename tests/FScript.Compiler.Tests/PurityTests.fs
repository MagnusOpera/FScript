namespace FScript.Compiler.Tests

open System.Text
open System.Text.Json.Nodes
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type PurityTests() =
    let external name valueType arity : CompilerExtern =
        { Name=name; Scheme=Forall([],valueType); Arity=arity; Kind=UnsupportedHostExtern }
    let compile registration purity externs source =
        match Compiler.compileWithRegistrationAndPurity registration purity {Program=Parser.parseProgram source;Externs=externs} with
        | Ok value -> value
        | Error errors -> failwith (System.String.Join("; ",errors |> List.map _.Message))
    let optimize program =
        match LirMonomorphization.specialize program with
        | Error errors -> failwith (System.String.Join("; ",errors |> List.map _.Message))
        | Ok value ->
            let result=LirSimplification.optimize value
            match LirValidation.validate result with
            | Ok value -> value
            | Error errors -> failwith (System.String.Join("; ",errors |> List.map _.Message))
    let pureFunctions names = {PurityContracts.empty with PureFunctions=Set.ofList names}

    [<Test>]
    member _.``Purity contracts survive the frontend HIR LIR and artifacts`` () =
        let registration={HostInterop.empty with Types=[{Name="Point";Fields=Map.ofList ["X",TInt];HostManaged=true}]}
        let purity={PureFunctions=Set.singleton "Host.make";PureTypes=Set.singleton "Point"}
        let externs=[external "Host.make" (TFun(TInt,TNamed "Point")) 1]
        let source="Host.make 42"
        let resolved=TypeInfer.inferResolvedProgramWithRegistrationAndPurity registration purity (Parser.parseProgram "42")
        Assert.That(resolved.Purity,Is.EqualTo(purity))
        let compiled=compile registration purity externs source
        Assert.That(compiled.Hir.Declarations |> List.find (fun d -> d.Name="Host.make") |> _.IsPure,Is.True)
        Assert.That(compiled.Hir.Types |> List.find (fun d -> d.Name="Point") |> _.IsPure,Is.True)
        Assert.That(Hir.format compiled.Hir,Does.Contain("pure"))
        let bytes=IrArtifact.serialize compiled.Lir
        Assert.That((IrArtifact.serialize compiled.Lir = bytes),Is.True)
        let loaded=match IrArtifact.deserialize bytes with Ok p -> p | Error errors -> failwith (string errors)
        Assert.That(loaded.Declarations |> List.find (fun d -> d.Name="Host.make") |> _.IsPure,Is.True)
        Assert.That(loaded.Declarations |> List.find (fun d -> d.Name="Host.make") |> _.ExternalArity,Is.EqualTo(Some 1))
        Assert.That(loaded.Types |> List.find (fun d -> d.Name="Point") |> _.IsPure,Is.True)
        // Existing version-1 artifacts have no purity flags; loading stays conservative.
        let json=JsonNode.Parse(Encoding.UTF8.GetString bytes) |> nonNull
        for category in ["types";"declarations"] do
            for value in (nonNull (nonNull json.["program"]).[category]).AsArray() do
                (nonNull value).AsObject().Remove("pure") |> ignore
                (nonNull value).AsObject().Remove("externalArity") |> ignore
        let legacy=match IrArtifact.deserialize (Encoding.UTF8.GetBytes(json.ToJsonString())) with Ok p -> p | Error errors -> failwith (string errors)
        Assert.That(legacy.Declarations |> List.exists _.IsPure,Is.False)
        Assert.That(legacy.Types |> List.exists _.IsPure,Is.False)

    [<Test>]
    member _.``A pure factory does not make its returned callback pure`` () =
        let externs=[external "Host.make" (TFun(TUnit,TFun(TInt,TInt))) 1]
        let compiled=compile HostInterop.empty (pureFunctions ["Host.make"]) externs "let discarded = Host.make () 1\n42"
        // Linking must preserve the application boundary independently of function type arity.
        let linked =
            { compiled.Lir with
                Declarations = compiled.Lir.Declarations |> List.map (fun d ->
                    if d.Name="Host.make" then {d with Value={d.Value with Node=Lir.TargetImport("provider","Make")}} else d) }
        let optimized=optimize linked
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.True)

    [<Test>]
    member _.``Observable generalized initializers are diagnosed rather than omitted`` () =
        let compiled=compile HostInterop.empty PurityContracts.empty [] "let fail () = raise \"boom\"\nlet discarded = fail ()\n42"
        match LirMonomorphization.specialize compiled.Lir with
        | Ok _ -> Assert.Fail("An observable open initializer must not disappear.")
        | Error errors -> Assert.That(errors |> List.map _.Code,Does.Contain("FSL2108"))

    [<Test>]
    member _.``Artifact validation rejects purity on ordinary values`` () =
        let compiled=compile HostInterop.empty PurityContracts.empty [] "42"
        let forged={compiled.Lir with Declarations=compiled.Lir.Declarations |> List.map (fun d -> {d with IsPure=true})}
        match IrArtifact.deserialize (IrArtifact.serialize forged) with
        | Ok _ -> Assert.Fail("A purity contract requires a valid external binding.")
        | Error errors -> Assert.That(errors |> List.map _.Code,Does.Contain("FSL2018"))

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``Collection effects follow their callbacks`` effectful =
        let callback=if effectful then "let callback value =\n    let ignored = Console.writeLine \"callback\"\n    value\n" else "let callback value = value + 1\n"
        let optimized=(compile HostInterop.empty PurityContracts.empty [] (callback+"let discarded = List.map callback [1;2]\n42")).Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.EqualTo(effectful))

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``Only declared pure unused FFI calls are pruned`` (isPure: bool) =
        let contracts=if isPure then pureFunctions ["Host.add"] else PurityContracts.empty
        let externs=[external "Host.add" (TFun(TInt,TFun(TInt,TInt))) 2]
        let optimized=(compile HostInterop.empty contracts externs "let discarded = Host.add 1 2\n42").Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.EqualTo(not isPure))
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="Host.add"),Is.EqualTo(not isPure))

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``External field reads need a pure type contract`` (isPure: bool) =
        let registration={HostInterop.empty with Types=[{Name="Handle";Fields=Map.ofList ["Value",TInt];HostManaged=true}]}
        let contracts=if isPure then {PurityContracts.empty with PureTypes=Set.singleton "Handle"} else PurityContracts.empty
        let externs=[external "Host.obtain" (TFun(TUnit,TNamed "Handle")) 1]
        let optimized=(compile registration contracts externs "let handle = Host.obtain ()\nlet discarded = handle.Value\n42").Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.EqualTo(not isPure))
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="handle"),Is.True,"A pure type does not make its factory pure.")

    [<TestCase("let discarded = 1 / 0\n42")>]
    [<TestCase("let discarded = match 1 with | 2 -> 0\n42")>]
    [<TestCase("let fail () : int = raise \"boom\"\nlet discarded = fail ()\n42")>]
    [<TestCase("let rec loop (value: int) : int = loop value\nlet discarded = loop 1\n42")>]
    [<TestCase("let discarded = Console.writeLine \"visible\"\n42")>]
    [<TestCase("let fail value = match value with | 2 -> 0\nlet discarded = fail 1\n42")>]
    member _.``Effects failures and unproven termination remain observable`` source =
        let optimized=(compile HostInterop.empty PurityContracts.empty [] source).Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.True,source)

    [<TestCase("Host.add (1 / 0) 2")>]
    [<TestCase("Host.add (argument ()) 2")>]
    member _.``Pure calls preserve argument failures and effects`` expression =
        let externs=[external "Host.add" (TFun(TInt,TFun(TInt,TInt))) 2]
        let optimized=(compile HostInterop.empty (pureFunctions ["Host.add"]) externs ("let argument () =\n    let ignored = Console.writeLine \"argument\"\n    1\nlet discarded = "+expression+"\n42")).Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.True)

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``FFI callback effects constrain the pure contract`` effectful =
        let externs=[external "Host.apply" (TFun(TFun(TInt,TInt),TFun(TInt,TInt))) 2]
        let callback=if effectful then "let callback value =\n    let ignored = Console.writeLine \"callback\"\n    value\n" else "let callback value = value + 1\n"
        let optimized=(compile HostInterop.empty (pureFunctions ["Host.apply"]) externs (callback+"let discarded = Host.apply callback 2\n42")).Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.EqualTo(effectful))

    [<Test>]
    member _.``Unused string replacement is total and prunable`` () =
        let optimized=(compile HostInterop.empty PurityContracts.empty [] "let discarded = String.replace \"\" \"x\" \"abc\"\n42").Lir |> optimize
        Assert.That(optimized.Declarations |> List.exists (fun d -> d.Name="discarded"),Is.False)

    [<TestCase("Missing.call")>]
    [<TestCase("Console.writeLine")>]
    member _.``Purity declarations cannot silently name missing or protected bindings`` name =
        match Compiler.compileWithPurity (pureFunctions [name]) {Program=Parser.parseProgram "42";Externs=[]} with
        | Ok _ -> Assert.Fail("Expected an invalid purity contract")
        | Error errors -> Assert.That(errors |> List.map _.Code,Does.Contain("FSH1000"))

    [<Test>]
    member _.``Exit code writes are observable and cannot be deduplicated`` () =
        let compiled=compile HostInterop.empty PurityContracts.empty [] "Env.setExitCode 7\nEnv.setExitCode 7\n42"
        let optimized=compiled.Lir |> optimize |> LirCommonSubexpressions.optimize
        let setters=optimized.Declarations |> List.filter (fun d -> d.Name.StartsWith("$expression") && d.Value.Type=TUnit)
        Assert.That(setters.Length,Is.EqualTo(2))

    [<Test>]
    member _.``Discarding results prunes pure work but preserves status and failures`` () =
        let run source =
            let compiled=compile HostInterop.empty PurityContracts.empty [] source
            compiled.Lir |> LirResultPolicy.discard |> optimize
        let discarded=run "let compute n = n*n + 1\ncompute 42"
        Assert.That(discarded.Declarations,Is.Empty)
        Assert.That(discarded.Functions,Is.Empty)
        let setter=run "Env.setExitCode 7\n42"
        Assert.That(setter.Declarations |> List.exists (fun d -> d.Value.Type=TUnit && d.Name.StartsWith("$expression")),Is.True)
        let failure=run "1/0"
        Assert.That(failure.Declarations,Is.Not.Empty)

    [<Test>]
    member _.``Discard mode accepts an unused open pure result`` () =
        let compiled=compile HostInterop.empty PurityContracts.empty [] "None"
        let optimized=compiled.Lir |> LirResultPolicy.discard |> optimize
        Assert.That(optimized.Declarations,Is.Empty)
