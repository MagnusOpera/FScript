namespace FScript.Runtime.Tests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type ProcessTests() =
    let fixture = typeof<FScript.Runtime.Tests.ProcessFixture>.Assembly.Location
    let quote (text: string) = System.Text.Json.JsonSerializer.Serialize(text)
    let root = Directory.GetCurrentDirectory()
    let grants = HostPermissions.Native ||| HostPermissions.Threading
    let run permissions source =
        let registration = Registry.registration { HostContext.create root with Permissions = permissions }
        ScriptHost.loadSourceWithRegistration registration source |> _.LastValue
    let call args options = $"Process.exec \"dotnet\" [{quote fixture}; \"--process-fixture\"; {args}] {options}"
    let runner args timeout limit stdin directory environment cancellation =
        ProcessRunner.Run(root, "dotnet", Array.ofList(fixture::"--process-fixture"::args), directory, environment, stdin, timeout, limit, cancellation)
    let outcome args = runner args (Nullable()) 1048576L null null [||] CancellationToken.None |> _.GetAwaiter().GetResult()

    [<Test>]
    member _.``Defaults and data types are pure while execution requires both permissions`` () =
        for permissions in [HostPermissions.None; HostPermissions.IO; HostPermissions.Native; HostPermissions.Threading; HostPermissions.Native ||| HostPermissions.IO; HostPermissions.IO ||| HostPermissions.Threading] do
            let registration = Registry.registration { HostContext.create root with Permissions = permissions }
            Assert.That(registration.Types |> List.map _.Name, Does.Contain("ProcessOptions"))
            Assert.That(registration.Unions |> List.map _.Name, Does.Contain("ProcessResult"))
            Assert.That(registration.Functions |> List.map _.Name, Does.Not.Contain("Process.exec"))
            Assert.That(run permissions "Process.defaults.maxOutputBytes" |> Pretty.valueToString, Is.EqualTo("1048576"))
            Assert.Throws<TypeException>(Action(fun () -> run permissions (call "\"echo\"" "Process.defaults") |> ignore)) |> ignore
        let registration = Registry.registration { HostContext.create root with Permissions = grants }
        Assert.That(registration.Functions |> List.find (fun fn -> fn.Name="Process.exec") |> _.RequiredPermissions, Is.EqualTo(grants))

    [<Test>]
    member _.``Direct and renamed descriptors cannot bypass permission checks`` () =
        let original = ProcessExterns.exec (HostContext.create root)
        let forwarded = { original with Name="Host.process"; RequiredPermissions=HostPermissions.None }
        for permissions in [HostPermissions.Native; HostPermissions.Threading; HostPermissions.IO ||| HostPermissions.Threading] do
            Assert.Throws<EvalException>(Action(fun () -> Eval.invokeValueWithPermissionsAndExitCode permissions Map.empty (VExternal(forwarded, [])) [VString "dotnet";VList [];VUnit] |> ignore)) |> ignore
        Assert.Throws<EvalException>(Action(fun () -> Eval.invokeValueWithExitCode Map.empty (VExternal(original, [])) [VString "dotnet";VList [];VUnit] |> ignore)) |> ignore
        Assert.Throws<EvalException>(Action(fun () -> ScriptHost.loadSource [forwarded;ProcessExterns.defaults] "Host.process \"dotnet\" [] Process.defaults" |> ignore)) |> ignore
        let sdk = ExtensionBridge.toExtensionFunctions [original] |> List.head
        let registration = ExtensionBridge.toLanguageRegistration {FScript.Extensibility.HostInterop.empty with Functions=[{sdk with Name="Host.forwarded"}]}
        Assert.That(registration.Functions.Head.RequiredPermissions, Is.EqualTo(grants))
        let wrapped = { sdk with Name="Host.wrapped"; Impl=fun context args -> sdk.Impl context args }
        let adapted = ExtensionBridge.toLanguageRegistration {FScript.Extensibility.HostInterop.empty with Functions=[wrapped]}
        let baseline = Registry.registration {HostContext.create root with Permissions=HostPermissions.Native}
        let denied = {baseline with Functions=baseline.Functions @ adapted.Functions}
        Assert.Throws<EvalException>(Action(fun () -> ScriptHost.loadSourceWithRegistration denied "Host.wrapped \"dotnet\" [] Process.defaults" |> ignore)) |> ignore


    [<Test>]
    member _.``Interpreter returns union results and preserves argument boundaries`` () =
        let source = call "\"echo\"; \"a b\"; \"\"; \"quote\\\"\"; \"$HOME;*\"; \"hé🐈\"" "Process.defaults" + " |> Task.await"
        match run grants source with
        | VUnionCase("ProcessResult","Exited",Some(VRecord fields)) ->
            Assert.That(fields.["exitCode"] |> Pretty.valueToString, Is.EqualTo("7"))
            Assert.That(fields.["stdout"] |> Pretty.valueToString, Is.EqualTo(Pretty.valueToString(VString "a b\n\nquote\"\n$HOME;*\nhé🐈\n")))
            Assert.That(fields.["stderr"] |> Pretty.valueToString, Is.EqualTo(Pretty.valueToString(VString "error\n")))
        | other -> Assert.Fail(string other)

    [<Test>]
    member _.``Input is UTF8 and absent input closes stdin`` () =
        let result = runner ["stdin"] (Nullable()) 1048576L "hé🐈\n" null [||] CancellationToken.None |> _.GetAwaiter().GetResult()
        Assert.That(result.Stdout, Is.EqualTo("hé🐈\n"))
        Assert.That((outcome ["stdin"]).Stdout, Is.Empty)
        Assert.That((outcome ["invalid-utf8"]).Stdout, Is.EqualTo("�\n"))

    [<Test>]
    member _.``Working directory may escape root and environment overrides are local`` () =
        HostTestHelpers.withTempRoot "process-context" (fun directory ->
            let name = "FSCRIPT_PROCESS_TEST_" + Guid.NewGuid().ToString("N")
            let result = runner ["context"; name] (Nullable()) 1048576L null directory [|Collections.Generic.KeyValuePair(name,"value")|] CancellationToken.None |> _.GetAwaiter().GetResult()
            Assert.That(result.Message, Is.Null)
            Assert.That(result.Stdout, Does.EndWith(Path.GetFileName(directory)+"\nvalue"))
            Assert.That(Environment.GetEnvironmentVariable(name), Is.Null))

    [<Test>]
    member _.``Concurrent output drains without pipe deadlock`` () =
        let result = outcome ["flood";"200000"]
        Assert.That(result.Message, Is.Null)
        Assert.That(result.Stdout.Length, Is.EqualTo(200000))
        Assert.That(result.Stderr.Length, Is.EqualTo(200000))

    [<Test>]
    member _.``Limits fail with bounded partial output`` () =
        let result = runner ["flood";"200000"] (Nullable()) 1024L null null [||] CancellationToken.None |> _.GetAwaiter().GetResult()
        Assert.That(result.Message, Does.StartWith("OutputLimitExceeded:"))
        Assert.That(result.Stdout.Length, Is.LessThanOrEqualTo(1024))
        Assert.That(result.Stderr.Length, Is.LessThanOrEqualTo(1024))

    [<Test>]
    member _.``Timeout kills child and preserves partial output`` () =
        let result = runner ["pid"] (Nullable 1000L) 1048576L null null [||] CancellationToken.None |> _.GetAwaiter().GetResult()
        Assert.That(result.Message, Does.StartWith("TimedOut:"))
        let pid = Int32.Parse(result.Stdout.Trim())
        try
            use child = Diagnostics.Process.GetProcessById(pid)
            Assert.That(child.HasExited, Is.True)
        with :? ArgumentException -> ()

    [<Test>]
    member _.``Timeout also covers inherited output pipes`` () =
        let result = runner ["spawn"] (Nullable 500L) 1048576L null null [||] CancellationToken.None |> _.GetAwaiter().GetResult()
        Assert.That(result.Message, Does.StartWith("TimedOut:"))

    [<Test>]
    member _.``Cancellation remains a failed task`` () =
        use cancellation = new CancellationTokenSource(300)
        let work = runner ["delay";"30000"] (Nullable()) 1048576L null null [||] cancellation.Token
        Assert.Catch<OperationCanceledException>(Action(fun () -> work.GetAwaiter().GetResult() |> ignore)) |> ignore

    [<Test>]
    member _.``Invalid options and missing executables are results`` () =
        let invoke executable args timeout limit env = ProcessRunner.Run(root, executable, args, null, env, null, timeout, limit, CancellationToken.None).GetAwaiter().GetResult()
        for executable,args,timeout,limit,env in
            [ "", [||], Nullable(), 100L, [||]
              "dotnet", [|"bad\000arg"|], Nullable(), 100L, [||]
              "dotnet", [||], Nullable 0L, 100L, [||]
              "dotnet", [||], Nullable(), 0L, [||]
              "dotnet", [||], Nullable(), 100L, [|Collections.Generic.KeyValuePair("bad=name","value")|] ] do
            Assert.That((invoke executable args timeout limit env).Message, Does.StartWith("InvalidOptions:"))
        Assert.That((invoke "fscript-missing-" [||] (Nullable()) 100L [||]).Message, Does.StartWith("StartFailed:"))

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``Unawaited processes are joined even after foreground failure`` (fail: bool) =
        HostTestHelpers.withTempRoot "process-join" (fun directory ->
            let path = Path.Combine(directory,"result")
            let source = "let pending = " + call ($"\"write\"; {quote path}") "Process.defaults" + (if fail then "\n1 / 0" else "\n42")
            if fail then Assert.Throws<DivideByZeroException>(Action(fun () -> run grants source |> ignore)) |> ignore
            else Assert.That(run grants source |> Pretty.valueToString, Is.EqualTo("42"))
            Assert.That(File.ReadAllText(path), Is.EqualTo("done")))

    [<Test>]
    member _.``Tasks compose and startup failures are union values`` () =
        let source = "let a = " + call "\"echo\"; \"one\"" "Process.defaults" + "\nlet b = Process.exec \"fscript-missing-\" [] Process.defaults\n[a; b] |> Task.awaitAll |> List.length"
        Assert.That(run grants source |> Pretty.valueToString, Is.EqualTo("2"))


    [<Test>]
    member _.``Process result cases do not shadow script union constructors`` () =
        let source = """
type Outcome<'ok, 'error> = | Found of 'ok | Failed of 'error
let makeLocal () : Outcome<int, string> = Failed "local"
let local = makeLocal ()
let foreign = Process.exec "" [] Process.defaults |> Task.await
let localMessage = match local with | Failed message -> message | Found _ -> ""
let foreignMatched = match foreign with | ProcessResult.Failed _ -> true | _ -> false
(localMessage, foreignMatched)
"""
        Assert.That(run grants source |> Pretty.valueToString, Is.EqualTo("(\"local\", true)"))
