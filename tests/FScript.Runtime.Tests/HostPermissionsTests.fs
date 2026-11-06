namespace FScript.Runtime.Tests

open System
open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type HostPermissionsTests() =
    let context permissions = { HostContext.create (Directory.GetCurrentDirectory()) with Permissions = permissions }
    let registration permissions = Registry.registration (context permissions)
    let evaluate permissions source = ScriptHost.loadSourceWithRegistration (registration permissions) source
    let denied action = Assert.Throws<EvalException>(Action action) |> ignore

    [<Test>]
    member _.``Host defaults grant no effects while pure helpers remain available`` () =
        let host = HostContext.create "."
        Assert.That(host.Permissions, Is.EqualTo(HostPermissions.None))
        let names = Registry.all host |> List.map _.Name
        for name in ["Fs.readText"; "Fs.writeText"; "Fs.glob"; "Guid.new"; "Console.writeLine"; "Console.readLine"; "Task.spawn"; "Task.delay"] do
            Assert.That(names, Does.Not.Contain(name))
        Assert.That(names, Does.Contain("Fs.combinePath"))
        let loaded = evaluate HostPermissions.None "(Path.normalize \"a/../b\", Glob.isMatch \"*.fss\" \"a.fss\", Hash.sha256 \"abc\" |> Option.isSome)"
        Assert.That(Pretty.valueToString loaded.LastValue, Is.EqualTo("(\"b\", true, true)"))

    [<TestCase(HostPermissions.None, "Task.fromResult 42")>]
    [<TestCase(HostPermissions.IO, "Console.readLine ()")>]
    [<TestCase(HostPermissions.Interactive, "Console.readLine ()")>]
    [<TestCase(HostPermissions.Threading, "Fs.readText \"file\"")>]
    member _.``Unavailable operations are absent from hosted script inference`` (permissions, source) =
        Assert.Throws<TypeException>(Action(fun () -> evaluate permissions source |> ignore)) |> ignore

    [<Test>]
    member _.``IO and threading permit tasks without interactive input`` () =
        let permissions = HostPermissions.IO ||| HostPermissions.Threading
        let names = (registration permissions).Functions |> List.map _.Name
        Assert.That(names, Does.Contain("Console.writeLine"))
        Assert.That(names, Does.Not.Contain("Console.readLine"))
        let loaded = evaluate permissions "export let run () =\n  let pending = Task.delay 1 |> Task.continueWith (fun () -> 42)\n  7"
        Assert.That(ScriptHost.invoke loaded "run" [VUnit] |> Pretty.valueToString, Is.EqualTo("7"))

    [<Test>]
    member _.``Interactive console input needs both IO and Interactive`` () =
        let names = Registry.all (context (HostPermissions.IO ||| HostPermissions.Interactive)) |> List.map _.Name
        Assert.That(names, Does.Contain("Console.readLine"))

    [<Test>]
    member _.``Direct canonical invocation cannot bypass threading permission`` () =
        denied (fun () -> Eval.invokeValueWithExitCode Map.empty (VExternal(TaskFunctions.fromResult, [])) [VInt 42L] |> ignore)

    [<Test>]
    member _.``Forwarding task implementations cannot bypass threading permission`` () =
        let forwarded = { TaskFunctions.fromResult with Name = "Host.ready"; RequiredPermissions = HostPermissions.None }
        denied (fun () -> ScriptHost.loadSource [forwarded] "Host.ready 42" |> ignore)

    [<Test>]
    member _.``Returned callable retains its required permissions`` () =
        let mutable calls = 0
        let effect : ExternalFunction =
            { Name = "Host.effect"; Scheme = Forall([], TFun(TUnit,TUnit)); Arity = 1
              RequiredPermissions = HostPermissions.IO
              Impl = fun _ _ -> calls <- calls + 1; VUnit }
        let get : ExternalFunction =
            { Name = "Host.get"; Scheme = Forall([], TFun(TUnit,TFun(TUnit,TUnit))); Arity = 1
              RequiredPermissions = HostPermissions.None
              Impl = fun _ _ -> VExternal(effect,[]) }
        denied (fun () -> ScriptHost.loadSource [get] "Host.get () ()" |> ignore)
        Assert.That(calls, Is.Zero)

    [<Test>]
    member _.``SDK providers need Native and cannot use task context without Threading`` () =
        let functionFromProvider : FScript.Extensibility.ExternalFunction =
            { Name = "Host.spawn"; Scheme = FScript.Extensibility.Forall([], FScript.Extensibility.TFun(FScript.Extensibility.TFun(FScript.Extensibility.TUnit,FScript.Extensibility.TInt),FScript.Extensibility.TTask FScript.Extensibility.TInt)); Arity = 1
              Impl = fun ctx arguments -> ctx.SpawnTask arguments.Head }
        let sdk = { FScript.Extensibility.HostInterop.empty with Functions = [functionFromProvider] }
        let adapted = ExtensionBridge.toLanguageRegistration sdk
        Assert.Throws<TypeException>(Action(fun () -> ScriptHost.loadSourceWithRegistration adapted "Host.spawn (fun () -> 42)" |> ignore)) |> ignore
        let nativeOnly = adapted |> HostInterop.withPermissions HostPermissions.Native
        denied (fun () -> ScriptHost.loadSourceWithRegistration nativeOnly "Host.spawn (fun () -> 42)" |> ignore)
        let allowed = adapted |> HostInterop.withPermissions (HostPermissions.Native ||| HostPermissions.Threading)
        ScriptHost.loadSourceWithRegistration allowed "Host.spawn (fun () -> 42) |> ignore" |> ignore

    [<Test>]
    member _.``Forwarded SDK defaults preserve declarations and runtime guards`` () =
        let io = FsExterns.read_text (context HostPermissions.IO)
        let original = ExtensionBridge.toExtensionFunctions [io] |> List.head
        let alias = { original with Name = "Host.read" }
        let adapted = ExtensionBridge.toLanguageRegistration { FScript.Extensibility.HostInterop.empty with Functions = [alias] }
        Assert.That(adapted.Functions.Head.RequiredPermissions, Is.EqualTo(HostPermissions.IO))
        let wrapper = { original with Name = "Host.wrappedRead"; Impl = fun ctx args -> original.Impl ctx args }
        let nativeOnly = ExtensionBridge.toLanguageRegistrationWithPermissions HostPermissions.Native { FScript.Extensibility.HostInterop.empty with Functions = [wrapper] }
        denied (fun () -> ScriptHost.loadSourceWithRegistration nativeOnly "Host.wrappedRead \"anything\"" |> ignore)

    [<Test>]
    member _.``Interactive submissions retain the host policy`` () =
        let session = Interactive.createWithRegistration (registration HostPermissions.Threading)
        let first = Interactive.submit session (FScript.parse "let ready = Task.fromResult 41")
        let second = Interactive.submit first.Session (FScript.parse "ready |> Task.continueWith (fun value -> value + 1) |> Task.await")
        Assert.That(second.State.Permissions, Is.EqualTo(HostPermissions.Threading))
        Assert.That(Pretty.valueToString second.State.LastValue, Is.EqualTo("42"))

    [<Test>]
    member _.``Loading configured provider code is denied without Native`` () =
        let settings =
            EnvironmentConfiguration.resolveForCli None true (Directory.GetCurrentDirectory()) (Directory.GetCurrentDirectory()) None false ["missing-provider.dll"]
            |> Result.defaultWith failwith
        match EnvironmentLoader.load settings with
        | Error message -> Assert.That(message, Does.Contain("Native"))
        | Ok _ -> Assert.Fail("Provider loading must require Native")
