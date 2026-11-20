namespace FScript.Runtime.Tests

open System
open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Cli.Tests.Fixtures.Valid

/// Test provider observes the actual catalog supplied by the environment loader.
[<AbstractClass; Sealed>]
type AsyncCatalogProvider private () =
    [<FScript.Extensibility.FScriptExternProvider>]
    static member Register(context: FScript.Extensibility.HostContext) : FScript.Extensibility.ExternalFunction list =
        let hasThreading = context.DefaultFunctions |> List.exists (fun fn -> fn.Name.StartsWith("Task.") || fn.Name.StartsWith("Mailbox.") || fn.Name="FScript.exec")
        [{Name="Catalog.hasThreadingDefaults"; Scheme=FScript.Extensibility.Forall([],FScript.Extensibility.TBool); Arity=0
          Impl=fun _ _ -> FScript.Extensibility.VBool hasThreading}]

[<TestFixture>]
type ProcessRegistrationTests() =
    let get = function Ok value -> value | Error message -> failwith message
    let grants = HostPermissions.Native ||| HostPermissions.Threading
    let settings paths = EnvironmentConfiguration.resolve None (Directory.GetCurrentDirectory()) (Directory.GetCurrentDirectory()) None false paths |> get
    let execute registration =
        ScriptHost.loadSourceWithRegistration registration "Process.exec \"fscript-missing-process-\" [] Process.defaults |> Task.await |> (fun result -> match result with | ProcessResult.Failed _ -> 42 | _ -> 0)"
        |> _.LastValue |> Pretty.valueToString

    [<Test>]
    member _.``Runtime supplies Process without provider imports`` () =
        let loaded = EnvironmentLoader.loadWithInfoAndPermissions grants (settings []) |> get
        Assert.That(execute loaded.Registration, Is.EqualTo("42"))
        Assert.That(loaded.ProviderMethods, Is.Empty)

    [<Test>]
    member _.``Native allows async process IO while awaiting requires Threading`` () =
        let nativeOnly = EnvironmentLoader.loadWithPermissions HostPermissions.Native (settings []) |> get
        ScriptHost.loadSourceWithRegistration nativeOnly "Process.exec \"fscript-missing-process-\" [] Process.defaults |> ignore" |> ignore
        Assert.Throws<TypeException>(Action(fun () -> execute nativeOnly |> ignore)) |> ignore
        let safe = EnvironmentLoader.loadWithPermissions HostPermissions.None (settings []) |> get
        let defaults = ScriptHost.loadSourceWithRegistration safe "Process.defaults.maxOutputBytes"
        Assert.That(Pretty.valueToString defaults.LastValue, Is.EqualTo("1048576"))
        Assert.Throws<TypeException>(Action(fun () -> execute safe |> ignore)) |> ignore

    [<Test>]
    member _.``Disabling default externs removes Process`` () =
        let settings = EnvironmentConfiguration.resolve None (Directory.GetCurrentDirectory()) (Directory.GetCurrentDirectory()) None true [] |> get
        let loaded = EnvironmentLoader.loadWithPermissions grants settings |> get
        Assert.That(loaded.Functions |> List.map _.Name, Does.Not.Contain("Process.exec").And.Not.Contain("Process.defaults"))

    [<Test>]
    member _.``Provider catalogs exclude script concurrency functions`` () =
        let settings = settings [typeof<AsyncCatalogProvider>.Assembly.Location]
        let loaded = EnvironmentLoader.loadWithInfoAndPermissions HostPermissions.All settings |> get
        let result = ScriptHost.loadSourceWithRegistration loaded.Registration "Catalog.hasThreadingDefaults"
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("false"))
        Assert.That(loaded.Registration.Functions |> List.map _.Name, Does.Contain("Task.spawn").And.Contain("Mailbox.start"))

    [<Test>]
    member _.``SDK 2 assemblies cannot bind to the SDK 3 host`` () =
        let context = ExtensionLoadContext([])
        let requested = System.Reflection.AssemblyName("FScript.Extensibility, Version=2.0.0.0, Culture=neutral, PublicKeyToken=null")
        let error = Assert.Throws<FileLoadException>(Action(fun () -> context.LoadFromAssemblyName(requested) |> ignore)) |> nonNull
        Assert.That(error.ToString(), Does.Contain("2.0.0.0").And.Contain("3.0.0.0"))
