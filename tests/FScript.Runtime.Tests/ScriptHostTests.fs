namespace FScript.Runtime.Tests

open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime

type TaskSharingMetadataProvider =
    static member Provide() = ()
    static member FScriptThreadSafeTypes() : System.Collections.Generic.IEnumerable<string> = ["Handle"]
    static member FScriptThreadSafeFunctions() : System.Collections.Generic.IEnumerable<string> = ["Host.tick"]

[<TestFixture>]
type ScriptHostTests () =
    [<Test>]
    member _.``script_host lists and invokes exported function`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let loaded = ScriptHost.loadSource externs "export let add x y = x + y\nlet value = 42"
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("add"))

        match ScriptHost.invoke loaded "add" [ VInt 1L; VInt 2L ] with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected add result 3")

    [<Test>]
    member _.``script_host invokes exported closure function`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let loaded = ScriptHost.loadSource externs "let makeAdder x = fun y -> x + y\nexport let add2 = makeAdder 2"
        match ScriptHost.invoke loaded "add2" [ VInt 5L ] with
        | VInt 7L -> ()
        | _ -> Assert.Fail("Expected closure invocation result 7")

    [<Test>]
    member _.``script_host exposes exported values separately from functions`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let loaded = ScriptHost.loadSource externs "export let version = \"1.0\"\nexport let add x y = x + y"

        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("add"))
        Assert.That(ScriptHost.listValues loaded, Does.Contain("version"))

        match ScriptHost.getValue loaded "version" with
        | VString "1.0" -> ()
        | _ -> Assert.Fail("Expected exported value")

    [<Test>]
    member _.``script_host rejects invocation of exported value`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let loaded = ScriptHost.loadSource externs "export let version = \"1.0\""
        let act () = ScriptHost.invoke loaded "version" [ VUnit ] |> ignore
        Assert.Throws<EvalException>(System.Action act) |> ignore

    [<Test>]
    member _.``script_host hides non-exported functions`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let loaded = ScriptHost.loadSource externs "let private_fn x = x + 1\nlet value = 1"

        Assert.That(ScriptHost.listFunctions loaded, Is.Empty)
        let hidden () = ScriptHost.invoke loaded "private_fn" [ VInt 1L ] |> ignore
        Assert.Throws<EvalException>(System.Action hidden) |> ignore

    [<Test>]
    member _.``script_host exports recursive groups when marked exported`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let source = "export let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)"
        let loaded = ScriptHost.loadSource externs source
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("even"))
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("odd"))
        match ScriptHost.invoke loaded "even" [ VInt 4L ] with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected even 4 to be true")

    [<Test>]
    member _.``script_host loadFile resolves imports`` () =
        let tempDir = Path.Combine(Path.GetTempPath(), $"fscript-runtime-include-{System.Guid.NewGuid():N}")
        Directory.CreateDirectory(tempDir) |> ignore
        try
            let sharedPath = Path.Combine(tempDir, "shared.fss")
            let mainPath = Path.Combine(tempDir, "main.fss")
            File.WriteAllText(sharedPath, "let increment x = x + 1")
            File.WriteAllText(mainPath, "import \"shared.fss\" as Shared\nexport let add2 x = Shared.increment (Shared.increment x)")
            let externs = Registry.all { RootDirectory = tempDir; DeniedPathGlobs = [] }
            let loaded = ScriptHost.loadFile externs mainPath
            match ScriptHost.invoke loaded "add2" [ VInt 10L ] with
            | VInt 12L -> ()
            | _ -> Assert.Fail("Expected imported function to be available")
        finally
            if Directory.Exists(tempDir) then
                Directory.Delete(tempDir, true)

    [<Test>]
    member _.``script_host loadSource rejects import directives`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let act () = ScriptHost.loadSource externs "import \"shared.fss\" as Shared\nlet x = 1" |> ignore
        Assert.Throws<EvalException>(System.Action act) |> ignore

    [<Test>]
    member _.``script_host loadSourceWithIncludes resolves imports from resolver`` () =
        let root = Path.Combine(Path.GetTempPath(), $"fscript-runtime-resolver-{System.Guid.NewGuid():N}")
        let entryFile = Path.Combine(root, "main.fss")
        let sharedFile = Path.Combine(root, "shared.fss")
        let sources =
            Map [
                sharedFile, "let increment x = x + 1"
            ]
        let resolve path = sources |> Map.tryFind path
        let source = "import \"shared.fss\" as Shared\nexport let add2 x = Shared.increment (Shared.increment x)"
        let externs = Registry.all { RootDirectory = root; DeniedPathGlobs = [] }
        let loaded = ScriptHost.loadSourceWithIncludes externs root entryFile source resolve

        match ScriptHost.invoke loaded "add2" [ VInt 10L ] with
        | VInt 12L -> ()
        | _ -> Assert.Fail("Expected imported function to be available")

    [<Test>]
    member _.``script_host loadSourceWithIncludes resolver receives normalized full import paths`` () =
        let root = Path.Combine(Path.GetTempPath(), $"fscript-runtime-resolver-nested-{System.Guid.NewGuid():N}")
        let entryFile = Path.Combine(root, "main.fss")
        let sharedFile = Path.GetFullPath(Path.Combine(root, "shared/validation.fss"))
        let source = "import \"shared/validation.fss\" as Validation\nexport let run x = Validation.increment x"
        let externs = Registry.all { RootDirectory = root; DeniedPathGlobs = [] }

        let mutable resolvedPath : string option = None
        let resolve path =
            resolvedPath <- Some path
            if System.String.Equals(path, sharedFile, System.StringComparison.OrdinalIgnoreCase) then
                Some "let increment x = x + 1"
            else
                None

        let loaded = ScriptHost.loadSourceWithIncludes externs root entryFile source resolve

        match ScriptHost.invoke loaded "run" [ VInt 10L ] with
        | VInt 11L -> ()
        | _ -> Assert.Fail("Expected imported function to be available")

        match resolvedPath with
        | Some path -> Assert.That(path, Is.EqualTo(sharedFile))
        | None -> Assert.Fail("Expected resolver to be called")

    [<Test>]
    member _.``script_host loadSourceWithIncludes fails when resolver misses import`` () =
        let root = Path.Combine(Path.GetTempPath(), $"fscript-runtime-resolver-missing-{System.Guid.NewGuid():N}")
        let entryFile = Path.Combine(root, "main.fss")
        let source = "import \"shared.fss\" as Shared\nexport let run x = Shared.increment x"
        let externs = Registry.all { RootDirectory = root; DeniedPathGlobs = [] }
        let act () = ScriptHost.loadSourceWithIncludes externs root entryFile source (fun _ -> None) |> ignore
        Assert.Throws<ParseException>(System.Action act) |> ignore


    [<Test>]
    member _.``script_host contextual export remains usable as an identifier`` () =
        let loaded = ScriptHost.loadSource [] "let export = 40\nexport let answer = export + 2\nexport let identity export = export"
        match ScriptHost.getValue loaded "answer", ScriptHost.invoke loaded "identity" [VInt 7L] with
        | VInt 42L, VInt 7L -> ()
        | _ -> Assert.Fail("Expected contextual export and ordinary identifier uses")

    [<Test>]
    member _.``script_host can invoke an exported function named export`` () =
        let loaded = ScriptHost.loadSource [] "export let export export = export"
        match ScriptHost.invoke loaded "export" [VString "value"] with
        | VString "value" -> ()
        | _ -> Assert.Fail("Expected exported function named export")

    [<Test>]
    member _.``Exit codes are scoped to evaluation and do not terminate the host`` () =
        let original=System.Environment.ExitCode
        let loaded=ScriptHost.loadSource [] "Env.setExitCode 7\nEnv.setExitCode 9\n42"
        Assert.That(loaded.ExitCode,Is.EqualTo(9))
        Assert.That(Pretty.valueToString loaded.LastValue,Is.EqualTo("42"))
        Assert.That((ScriptHost.loadSource [] "42").ExitCode,Is.Zero)
        Assert.That(System.Environment.ExitCode,Is.EqualTo(original))

    [<Test>]
    member _.``Export invocations return their own exit code`` () =
        let loaded=ScriptHost.loadSource [] "export let configure code = Env.setExitCode code\n42"
        let result=ScriptHost.invokeWithExitCode loaded "configure" [VInt 7L]
        Assert.That(result.ExitCode,Is.EqualTo(7))
        Assert.That(Pretty.valueToString result.Value,Is.EqualTo("()"))
        Assert.That(loaded.ExitCode,Is.Zero)
        Assert.That((ScriptHost.invokeWithExitCode loaded "configure" [VInt 0L]).ExitCode,Is.Zero)

    [<Test>]
    member _.``Awaited tasks share status without stopping subsequent evaluation`` () =
        let registration=HostInterop.ofExterns [TaskFunctions.spawn;TaskFunctions.awaitTask]
        let loaded=ScriptHost.loadSourceWithRegistration registration "let worker=Task.spawn (fun () -> Env.setExitCode 3)\nTask.await worker\nEnv.setExitCode 7\n42"
        Assert.That(loaded.ExitCode,Is.EqualTo(7))
        Assert.That(Pretty.valueToString loaded.LastValue,Is.EqualTo("42"))

    [<TestCase("Env.setExitCode 256")>]
    [<TestCase("Env.setExitCode (0-1)")>]
    member _.``Exit codes use the portable shell range`` source =
        let error=Assert.Throws<EvalException>(System.Action(fun () -> ScriptHost.loadSource [] source |> ignore)) |> nonNull
        Assert.That(string error,Does.Contain("between 0 and 255"))

    [<Test>]
    member _.``Explicit task sharing analysis reports before interpreting without changing values`` () =
        let mutable calls = 0
        let tick: ExternalFunction =
            { Name = "Host.tick"; Scheme = Forall([], TFun(TUnit, TInt)); Arity = 1
              Impl = fun _ _ -> System.Threading.Interlocked.Increment(&calls) |> int64 |> VInt }
        let registration = { HostInterop.empty with Functions = [tick; TaskFunctions.spawn; TaskFunctions.awaitTask] }
        let source = "let first = Task.spawn (fun () -> Host.tick ())\nlet second = Task.spawn (fun () -> Host.tick ())\nTask.await first + Task.await second"
        let warnings = ResizeArray<TaskSharingAnalysis.Diagnostic>()
        let report warning =
            Assert.That(calls, Is.Zero, "All analysis warnings must precede interpretation")
            warnings.Add warning
        let loaded = ScriptHost.loadSourceWithTaskSharingAnalysis registration PurityContracts.empty TaskSharingAnalysis.defaults report source
        Assert.That(warnings |> Seq.exists (fun warning -> warning.Code = "FST1002"), Is.True)
        Assert.That(calls, Is.EqualTo(2))
        Assert.That(Pretty.valueToString loaded.LastValue, Is.EqualTo("3"))
        calls <- 0
        let ordinary = ScriptHost.loadSourceWithRegistration registration source
        Assert.That(calls, Is.EqualTo(2))
        Assert.That(Pretty.valueToString ordinary.LastValue, Is.EqualTo(Pretty.valueToString loaded.LastValue))

    [<Test>]
    member _.``Thread safety companions use independent BCL metadata`` () =
        let method = typeof<TaskSharingMetadataProvider>.GetMethod("Provide") |> nonNull
        let options = TaskSharingContracts.fromProviders [method]
        Assert.That(options.ThreadSafeTypes.Contains "Handle", Is.True)
        Assert.That(options.ThreadSafeFunctions.Contains "Host.tick", Is.True)

    [<Test>]
    member _.``Runtime console contracts classify actual implementations without granting purity`` () =
        let registration = { HostInterop.empty with Functions = [ConsoleExterns.write_line; ConsoleExterns.read_line; TaskFunctions.spawn; TaskFunctions.awaitTask] }
        let source = "let first = Task.spawn (fun () -> Console.writeLine \"one\")\nlet second = Task.spawn (fun () -> Console.writeLine \"two\")\nTask.await first\nTask.await second"
        let resolved = Parser.parseProgram source |> TypeInfer.inferResolvedProgramWithRegistration registration
        let options = TaskSharingContracts.withRuntime registration TaskSharingAnalysis.defaults
        Assert.That(options.ThreadSafeFunctions.Contains "Console.writeLine", Is.True)
        Assert.That(options.ThreadSafeFunctions.Contains "Console.readLine", Is.True)
        Assert.That(resolved.Purity.PureFunctions.Contains "Console.writeLine", Is.False)
        Assert.That(TaskSharingAnalysis.analyze registration options resolved, Is.Empty)
        let replacement = { ConsoleExterns.write_line with Impl = fun _ _ -> VUnit }
        let replaced = { registration with Functions = replacement :: [TaskFunctions.spawn; TaskFunctions.awaitTask] }
        let options = TaskSharingContracts.withRuntime replaced TaskSharingAnalysis.defaults
        Assert.That(options.ThreadSafeFunctions.Contains "Console.writeLine", Is.False)
        Assert.That(TaskSharingAnalysis.analyze replaced options resolved |> List.exists (fun warning -> warning.Code = "FST1002"), Is.True)

    [<Test>]
    member _.``Pure content contracts recognize implementations rather than names`` () =
        let registration = { HostInterop.empty with Functions = PureExterns.functions }
        let options = TaskSharingContracts.withRuntime registration TaskSharingAnalysis.defaults
        for fn in PureExterns.functions do
            Assert.That(options.ThreadSafeFunctions.Contains fn.Name, Is.True, fn.Name)
        let hash = PureExterns.functions |> List.find (fun fn -> fn.Name = "Hash.sha256")
        let replacement = { hash with Impl = fun _ _ -> VOption None }
        let options = TaskSharingContracts.withRuntime { registration with Functions = [replacement] } TaskSharingAnalysis.defaults
        Assert.That(options.ThreadSafeFunctions.Contains hash.Name, Is.False)
