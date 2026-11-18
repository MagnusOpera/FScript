namespace FScript.Runtime.Tests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type ScriptTests() =
    let grants = HostPermissions.Threading
    let execute root permissions denied path options =
        ScriptTestFiles.run (ScriptTestFiles.registration root permissions denied)
            (ScriptTestFiles.call path "[]" options+" |> Task.await")

    [<Test>]
    member _.``Source and sibling imports need no IO and default root is inherited`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "common.fss" "export let message = \"hello\""
            ScriptTestFiles.write root "jobs/child.fss" "import \"../common.fss\" as Common\nCommon.message"
            execute root grants [] "jobs/child.fss" "FScript.defaults" |> ScriptTestFiles.completed "hello" 0)

    [<Test>]
    member _.``Defaults and type descriptors are pure but invocation needs Threading`` () =
        let registration = ScriptTestFiles.registration "." HostPermissions.None []
        Assert.That(registration.Functions |> List.map _.Name, Does.Not.Contain("FScript.exec"))
        Assert.That(registration.Types |> List.map _.Name, Does.Contain("FScriptOptions"))
        Assert.That(registration.Unions |> List.map _.Name, Does.Contain("FScriptResult"))
        Assert.That(ScriptTestFiles.run registration "FScript.defaults.permissions" |> Pretty.valueToString, Is.EqualTo("None"))
        Assert.Throws<TypeException>(Action(fun () -> ScriptTestFiles.run registration "FScript.exec \"child.fss\" [] FScript.defaults" |> ignore)) |> ignore
        let original = FScriptExterns.exec (HostContext.create ".")
        let renamed = {original with Name="Host.child"; RequiredPermissions=HostPermissions.None}
        let registration = {registration with Functions=renamed::registration.Functions}
        Assert.Throws<EvalException>(Action(fun () -> ScriptTestFiles.run registration "Host.child \"child.fss\" [] FScript.defaults" |> ignore)) |> ignore
        Assert.Throws<EvalException>(Action(fun () -> Eval.invokeValueWithExitCode Map.empty (VExternal(original,[])) [VString "child.fss";VList [];FScriptSignatures.defaultValue] |> ignore)) |> ignore

    [<Test>]
    member _.``SDK forwarding cannot remove the threading requirement`` () =
        let original = FScriptExterns.exec (HostContext.create ".")
        let sdk = ExtensionBridge.toExtensionFunctions [original] |> List.head
        let forwarded = ExtensionBridge.toLanguageRegistration {FScript.Extensibility.HostInterop.empty with Functions=[{sdk with Name="Host.child"}]}
        Assert.That(forwarded.Functions.Head.RequiredPermissions, Is.EqualTo(grants))
        let wrapped = {sdk with Name="Host.wrapped"; Impl=fun context args -> sdk.Impl context args}
        let adapted = ExtensionBridge.toLanguageRegistration {FScript.Extensibility.HostInterop.empty with Functions=[wrapped]}
        let baseline = ScriptTestFiles.registration "." HostPermissions.Native []
        let registration = {baseline with Functions=adapted.Functions @ baseline.Functions}
        Assert.Throws<EvalException>(Action(fun () -> ScriptTestFiles.run registration "Host.wrapped \"child.fss\" [] FScript.defaults" |> ignore)) |> ignore

    [<Test>]
    member _.``Actual invocation grants override the captured host registration`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "ok.fss" "\"ok\""
            ScriptTestFiles.write root "io.fss" "Fs.readText \"file\" |> ignore\n\"bad\""
            let registration = ScriptTestFiles.registration root HostPermissions.All [] |> HostInterop.withPermissions grants
            ScriptTestFiles.run registration "FScript.exec \"ok.fss\" [] FScript.defaults |> Task.await" |> ScriptTestFiles.completed "ok" 0
            ScriptTestFiles.run registration "FScript.exec \"io.fss\" [] FScript.defaults |> Task.await" |> ScriptTestFiles.failure "TypeFailed" |> ignore
            ScriptTestFiles.run registration "FScript.exec \"ok.fss\" [] { FScript.defaults with permissions = Some [\"IO\"] } |> Task.await"
            |> ScriptTestFiles.failure "PermissionDenied" |> ignore)

    [<Test>]
    member _.``Child can remove Threading while its caller executes it asynchronously`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "ok.fss" "\"ok\""
            ScriptTestFiles.write root "task.fss" "Task.fromResult \"bad\" |> Task.await"
            let options = "{ FScript.defaults with permissions = Some [] }"
            execute root grants [] "ok.fss" options |> ScriptTestFiles.completed "ok" 0
            execute root grants [] "task.fss" options |> ScriptTestFiles.failure "TypeFailed" |> ignore)

    [<TestCase("../outside.fss", "FScript.defaults")>]
    [<TestCase("ok.fss", "{ FScript.defaults with rootDirectory = Some \"..\" }")>]
    [<TestCase("ok.fss", "{ FScript.defaults with rootDirectory = Some \"jobs\" }")>]
    member _.``Entry and root cannot escape or disagree`` (path: string, options: string) =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "ok.fss" "\"ok\""
            Directory.CreateDirectory(Path.Combine(root,"jobs")) |> ignore
            execute root grants [] path options |> ScriptTestFiles.failure "PathDenied" |> ignore)

    [<Test>]
    member _.``Equal and narrower roots and absolute entries are supported`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "jobs/ok.fss" "\"ok\""
            execute root grants [] (Path.Combine(root,"jobs/ok.fss")) "{ FScript.defaults with rootDirectory = Some \"jobs\" }" |> ScriptTestFiles.completed "ok" 0
            execute root grants [] "jobs/ok.fss" "{ FScript.defaults with rootDirectory = Some \".\" }" |> ScriptTestFiles.completed "ok" 0)

    [<Test>]
    member _.``Imports cannot escape a narrower root`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "common.fss" "export let value = \"outside\""
            ScriptTestFiles.write root "jobs/child.fss" "import \"../common.fss\" as Common\nCommon.value"
            execute root grants [] "jobs/child.fss" "{ FScript.defaults with rootDirectory = Some \"jobs\" }" |> ScriptTestFiles.failure "PathDenied" |> ignore)

    [<Test>]
    member _.``Ancestor denials cover child roots entry and imported source`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "jobs/blocked.fss" "export let value = \"blocked\""
            ScriptTestFiles.write root "jobs/child.fss" "import \"blocked.fss\" as Blocked\nBlocked.value"
            let options = "{ FScript.defaults with rootDirectory = Some \"jobs\" }"
            for path,denied in ["jobs/blocked.fss",["jobs/blocked.fss"]; "jobs/child.fss",["jobs/blocked.fss"]; "jobs/child.fss",["jobs"]] do
                execute root grants denied path options |> ScriptTestFiles.failure "PathDenied" |> ignore)

    [<Test>]
    member _.``Ancestor filesystem rules survive two root narrowings and prune enumeration`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "jobs/sub/secret/data.txt" "secret"
            ScriptTestFiles.write root "jobs/sub/read.fss" "Fs.readText \"secret/data.txt\" |> ignore\n\"bad\""
            ScriptTestFiles.write root "jobs/sub/write.fss" "Fs.writeText \"secret/new.txt\" \"bad\" |> ignore\n\"bad\""
            ScriptTestFiles.write root "jobs/sub/mkdir.fss" "Fs.createDirectory \"secret/new\" |> ignore\n\"bad\""
            ScriptTestFiles.write root "jobs/sub/probe.fss" "let exists = Fs.exists \"secret/data.txt\"\nlet hiddenKind = match Fs.kind \"secret/data.txt\" with | FsKind.Missing -> true | _ -> false\nlet files = Fs.enumerateFiles \".\" \"**/*.txt\" |> Option.defaultValue []\nif exists then \"leak\" else if hiddenKind && List.isEmpty files then \"hidden\" else \"leak\""
            ScriptTestFiles.write root "jobs/sub/list.fss" "let entries = Fs.glob \"**/*.txt\" |> Option.defaultValue []\nif List.isEmpty entries then \"hidden\" else \"leak\""
            ScriptTestFiles.write root "jobs/parent.fss" "match FScript.exec (String.concat \"\" [\"sub/\"; (Env.Arguments |> List.tryHead |> Option.defaultValue \"\")]) [] { FScript.defaults with rootDirectory = Some \"sub\" } |> Task.await with\n| FScriptResult.Completed info -> info.value\n| FScriptResult.Failed _ -> \"blocked\""
            let registration = ScriptTestFiles.registration root (grants ||| HostPermissions.IO) ["jobs/sub/secret"]
            for name,expected in ["read.fss","blocked";"write.fss","blocked";"mkdir.fss","blocked";"probe.fss","hidden";"list.fss","hidden"] do
                ScriptTestFiles.run registration (ScriptTestFiles.call "jobs/parent.fss" ($"[{ScriptTestFiles.quote name}]") "{ FScript.defaults with rootDirectory = Some \"jobs\" }"+" |> Task.await")
                |> ScriptTestFiles.completed expected 0
            Assert.That(File.Exists(Path.Combine(root,"jobs/sub/secret/new.txt")), Is.False)
            Assert.That(Directory.Exists(Path.Combine(root,"jobs/sub/secret/new")), Is.False))

    [<Test>]
    member _.``Symlinked entry and root are rejected`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "real/ok.fss" "\"ok\""
            try Directory.CreateSymbolicLink(Path.Combine(root,"link"),Path.Combine(root,"real")) |> ignore
            with :? UnauthorizedAccessException -> Assert.Ignore("Creating symlinks requires host privileges.")
            try
                execute root grants [] "link/ok.fss" "FScript.defaults" |> ScriptTestFiles.failure "PathDenied" |> ignore
                execute root grants [] "real/ok.fss" "{ FScript.defaults with rootDirectory = Some \"link\" }" |> ScriptTestFiles.failure "PathDenied" |> ignore
            finally Directory.Delete(Path.Combine(root,"link")))

    [<Test>]
    member _.``Arguments and exit codes are isolated and calls are fresh`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "child.fss" "Env.setExitCode 7\n(Env.Arguments |> List.tryHead |> Option.defaultValue \"\")"
            let registration = ScriptTestFiles.registration root grants []
            let source = "Env.setExitCode 3\nlet first = FScript.exec \"child.fss\" [\"first\"] FScript.defaults\nlet second = FScript.exec \"child.fss\" [\"second\"] FScript.defaults\n[first;second] |> Task.awaitAll"
            let loaded = ScriptHost.loadSourceWithRegistration registration source
            Assert.That(loaded.ExitCode, Is.EqualTo(3))
            match loaded.LastValue with
            | VList [first;second] -> ScriptTestFiles.completed "first" 7 first; ScriptTestFiles.completed "second" 7 second
            | value -> Assert.Fail(string value))

    [<TestCase("missing.fss", "LoadFailed")>]
    [<TestCase("wrong.txt", "InvalidOptions")>]
    [<TestCase("parse.fss", "ParseFailed")>]
    [<TestCase("type.fss", "TypeFailed")>]
    [<TestCase("throw.fss", "ExecutionFailed")>]
    [<TestCase("value.fss", "ResultTypeMismatch")>]
    [<TestCase("task.fss", "ResultTypeMismatch")>]
    [<TestCase("background.fss", "ExecutionFailed")>]
    member _.``Child failures are result values with diagnostics`` (path: string, prefix: string) =
        ScriptTestFiles.withRoot (fun root ->
            for file,source in ["parse.fss","let ="; "type.fss","notKnown"; "throw.fss","raise \"child failure\""; "value.fss","42"; "task.fss","Task.fromResult \"hello\""; "background.fss","let pending = Task.spawn (fun () -> raise \"background\")\n\"value\""] do
                ScriptTestFiles.write root file source
            execute root grants [] path "FScript.defaults" |> ScriptTestFiles.failure prefix |> ignore)

    [<Test>]
    member _.``Locals custom functions and automatic provider configuration do not enter the child`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "local.fss" "parentSecret"
            ScriptTestFiles.write root "custom.fss" "Host.secret ()"
            ScriptTestFiles.write root "ok.fss" "\"ok\""
            ScriptTestFiles.write root "fscript.toml" "THIS IS NOT A VALID MANIFEST"
            let fn : ExternalFunction = {Name="Host.secret";Scheme=Forall([],TFun(TUnit,TString));Arity=1;RequiredPermissions=HostPermissions.None;Impl=fun _ _ -> VString "secret"}
            let registration = ScriptTestFiles.registration root HostPermissions.All []
            let registration = {registration with Functions=fn::registration.Functions}
            for path in ["local.fss";"custom.fss"] do
                ScriptTestFiles.run registration ("let parentSecret = \"secret\"\n"+ScriptTestFiles.call path "[]" "FScript.defaults"+" |> Task.await") |> ScriptTestFiles.failure "TypeFailed" |> ignore
            ScriptTestFiles.run registration "FScript.exec \"ok.fss\" [] FScript.defaults |> Task.await" |> ScriptTestFiles.completed "ok" 0)

    [<TestCase("{ FScript.defaults with permissions = Some [\"unknown\"] }")>]
    [<TestCase("{ FScript.defaults with permissions = Some [\"Process\"] }")>]
    [<TestCase("{ FScript.defaults with timeoutMilliseconds = Some 0 }")>]
    [<TestCase("{ FScript.defaults with timeoutMilliseconds = Some 2147483648 }")>]
    member _.``Invalid child options fail before execution`` (options: string) =
        ScriptTestFiles.withRoot (fun root -> execute root grants [] "missing.fss" options |> ScriptTestFiles.failure "InvalidOptions" |> ignore)

    [<Test>]
    member _.``Nested execution is capped at exactly 32 child levels`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "recurse.fss" "match FScript.exec \"recurse.fss\" [] FScript.defaults |> Task.await with\n| FScriptResult.Completed info -> String.concat \"\" [\"x\"; info.value]\n| FScriptResult.Failed info -> info.message"
            let expected = String.replicate 31 "x"+"DepthLimitExceeded: At most 32 child script executions may be nested."
            execute root grants [] "recurse.fss" "FScript.defaults" |> ScriptTestFiles.completed expected 0)

    [<TestCase("loop.fss")>]
    [<TestCase("delay.fss")>]
    [<TestCase("nested.fss")>]
    member _.``Timeout cancels evaluation tasks and descendants`` (path: string) =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "loop.fss" "let rec loop n = loop (n + 1)\nloop 0"
            ScriptTestFiles.write root "delay.fss" "let pending = Task.delay 30000\n\"done\""
            ScriptTestFiles.write root "nested.fss" "FScript.exec \"delay.fss\" [] { FScript.defaults with timeoutMilliseconds = Some 30000 } |> Task.await |> ignore\n\"done\""
            let timer = Diagnostics.Stopwatch.StartNew()
            execute root grants [] path "{ FScript.defaults with timeoutMilliseconds = Some 100 }" |> ScriptTestFiles.failure "TimedOut" |> ignore
            Assert.That(timer.Elapsed.TotalSeconds, Is.LessThan(5)))

    [<Test>]
    member _.``Parent cancellation propagates and drains child work`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "delay.fss" "let pending = Task.delay 30000\n\"done\""
            let registration = ScriptTestFiles.registration root grants []
            let typed = Parser.parseProgram "FScript.exec \"delay.fss\" [] FScript.defaults |> Task.await" |> TypeInfer.inferProgramWithRegistration registration
            use cancelled = new CancellationTokenSource(100)
            let timer = Diagnostics.Stopwatch.StartNew()
            Assert.Catch<Exception>(Action(fun () -> Eval.evalProgramWithCancellation cancelled.Token registration typed |> ignore)) |> ignore
            Assert.That(timer.Elapsed.TotalSeconds, Is.LessThan(5)))

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``Unawaited child scripts and their tasks are joined on parent failure too`` (fail: bool) =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "child.fss" "let work () =\n  Task.delay 50 |> Task.await\n  Fs.writeText \"written.txt\" \"done\" |> ignore\nlet pending = Task.spawn work\n\"ok\""
            let registration = ScriptTestFiles.registration root (grants ||| HostPermissions.IO) []
            let source = "let pending = FScript.exec \"child.fss\" [] FScript.defaults\n"+(if fail then "raise \"parent failed\"" else "42")
            if fail then Assert.Catch<Exception>(Action(fun () -> ScriptTestFiles.run registration source |> ignore)) |> ignore
            else Assert.That(ScriptTestFiles.run registration source |> Pretty.valueToString, Is.EqualTo("42"))
            Assert.That(File.ReadAllText(Path.Combine(root,"written.txt")), Is.EqualTo("done")))


    [<Test>]
    member _.``Timeout drains child-started process work and terminates its direct child`` () =
        ScriptTestFiles.withRoot (fun root ->
            let pidFile = Path.Combine(root,"pid.txt")
            let fixture = typeof<ProcessFixture>.Assembly.Location
            let child = "Process.exec \"dotnet\" ["+ScriptTestFiles.quote fixture+"; \"--process-fixture\"; \"pid-file\"; "+ScriptTestFiles.quote pidFile+"] Process.defaults |> Task.await |> ignore\n\"done\""
            ScriptTestFiles.write root "process.fss" child
            execute root (grants ||| HostPermissions.Native) [] "process.fss" "{ FScript.defaults with timeoutMilliseconds = Some 2000 }"
            |> ScriptTestFiles.failure "TimedOut" |> ignore
            Assert.That(File.Exists pidFile, Is.True, "The child process must start before the timeout.")
            let pid = Int32.Parse(File.ReadAllText pidFile)
            try
                use child = Diagnostics.Process.GetProcessById pid
                Assert.That(child.HasExited, Is.True)
            with :? ArgumentException -> ())

    [<Test>]
    member _.``Console effects use the shared console only with IO`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "console.fss" "Console.writeLine \"child-output\"\n\"ok\""
            execute root grants [] "console.fss" "FScript.defaults" |> ScriptTestFiles.failure "TypeFailed" |> ignore
            let original = Console.Out
            use output = new StringWriter()
            try
                Console.SetOut output
                execute root (grants ||| HostPermissions.IO) [] "console.fss" "FScript.defaults" |> ScriptTestFiles.completed "ok" 0
            finally Console.SetOut original
            Assert.That(output.ToString(), Does.Contain("child-output")))


    [<Test>]
    member _.``Import cycles are failures and do not consume execution nesting`` () =
        ScriptTestFiles.withRoot (fun root ->
            ScriptTestFiles.write root "a.fss" "import \"b.fss\" as B\n\"a\""
            ScriptTestFiles.write root "b.fss" "import \"a.fss\" as A\n\"b\""
            execute root grants [] "a.fss" "FScript.defaults" |> ScriptTestFiles.failure "ParseFailed" |> ignore)
