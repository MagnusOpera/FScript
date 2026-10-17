namespace FScript.Runtime.Tests

open System
open System.IO
open NUnit.Framework
open FScript.Runtime

[<TestFixture>]
type EnvironmentConfigurationTests() =
    let directory = Path.Combine(Path.GetTempPath(), "fscript-config-tests")
    let configPath = Path.Combine(directory, "tools", "fscript.toml")
    let get = function Ok value -> value | Error message -> failwith message

    [<Test>]
    member _.``Empty configuration preserves omitted properties and caller defaults`` () =
        let parsed = EnvironmentConfiguration.parse configPath "" |> get
        Assert.That(parsed.Root, Is.EqualTo(None))
        Assert.That(parsed.ExternAssemblies, Is.EqualTo(None))
        let settings = EnvironmentConfiguration.merge directory directory (Some parsed) None false []
        Assert.That(settings.RootDirectory, Is.EqualTo(directory))
        Assert.That(settings.DefaultExterns, Is.True)
        Assert.That(settings.ExternAssemblies, Is.Empty)

    [<Test>]
    member _.``Manifest paths resolve from manifest directory and merge stably`` () =
        let parsed = EnvironmentConfiguration.parse configPath "[runtime]\nroot = '.'\nextern-assemblies = ['../A.dll', './B.dll']" |> get
        let settings = EnvironmentConfiguration.merge directory directory (Some parsed) None false ["A.dll"; "C.dll"]
        Assert.That(settings.RootDirectory, Is.EqualTo(Path.Combine(directory, "tools")))
        Assert.That(settings.ExternAssemblies, Is.EqualTo(box [Path.Combine(directory,"A.dll"); Path.Combine(directory,"tools","B.dll"); Path.Combine(directory,"C.dll")]))

    [<Test>]
    member _.``Explicit flags override configured root and disable defaults`` () =
        let parsed = EnvironmentConfiguration.parse configPath "[runtime]\nroot = '.'\ndefault-externs = true" |> get
        let settings = EnvironmentConfiguration.merge directory directory (Some parsed) (Some "override") true []
        Assert.That(settings.RootDirectory, Is.EqualTo(Path.Combine(directory, "override")))
        Assert.That(settings.DefaultExterns, Is.False)
        let disabled = EnvironmentConfiguration.parse configPath "[runtime]\ndefault-externs = false" |> get
        Assert.That((EnvironmentConfiguration.merge directory directory (Some disabled) None false []).DefaultExterns, Is.False)

    [<TestCase("[runtime]\nroot = 1", "runtime.root")>]
    [<TestCase("[runtime]\ndefault-externs = 'true'", "runtime.default-externs")>]
    [<TestCase("[runtime]\nextern-assemblies = ['A.dll', 1]", "runtime.extern-assemblies")>]
    [<TestCase("[runtime]\nextern-assemblies = 'A.dll'", "runtime.extern-assemblies")>]
    [<TestCase("[runtime]\nroots = '.'", "runtime.roots")>]
    [<TestCase("[project]\nentry = 'main.fss'", "project")>]
    [<TestCase("runtime = true", "runtime")>]
    [<TestCase("[runtime\n", "configuration")>]
    member _.``Invalid manifests produce actionable errors`` (text, property) =
        match EnvironmentConfiguration.parse configPath text with
        | Ok _ -> Assert.Fail("Expected invalid configuration")
        | Error message ->
            Assert.That(message, Does.Contain(configPath))
            Assert.That(message, Does.Contain(property))

    [<Test>]
    member _.``Parsing does not load executable assemblies`` () =
        let parsed = EnvironmentConfiguration.parse configPath "[runtime]\nextern-assemblies = ['missing.dll']" |> get
        Assert.That(parsed.ExternAssemblies.Value.Head, Does.EndWith("missing.dll"))
        let effective = EnvironmentConfiguration.merge directory directory (Some parsed) None false []
        match EnvironmentLoader.load effective with
        | Ok _ -> Assert.Fail("Missing assembly must fail at loading, not parsing")
        | Error message -> Assert.That(message, Does.Contain("missing.dll"))

    [<Test>]
    member _.``Missing configuration is an error`` () =
        match EnvironmentConfiguration.read (Path.Combine(directory, Guid.NewGuid().ToString(), "fscript.toml")) with
        | Ok _ -> Assert.Fail("Expected failure")
        | Error message -> Assert.That(message, Does.Contain("fscript.toml"))
