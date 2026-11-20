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
        Assert.That(parsed.Packages, Is.EqualTo(None))
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

    [<TestCase("[compile]\nbackend = 'go'", "compile.backend")>]
    [<TestCase("[compile]\naot = 'true'", "compile.aot")>]
    [<TestCase("[compile]\nsize-optimized = 1", "compile.size-optimized")>]
    [<TestCase("[compile]\nsingle-file = 'false'", "compile.single-file")>]
    [<TestCase("[compile]\noutput = ''", "compile.output")>]
    [<TestCase("[compile]\nruntime = false", "compile.runtime")>]
    [<TestCase("[compile]\ntarget = 'dotnet'", "compile.target")>]
    [<TestCase("compile = true", "compile")>]
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
        match EnvironmentLoader.loadWithPermissions FScript.Language.HostPermissions.All effective with
        | Ok _ -> Assert.Fail("Missing assembly must fail at loading, not parsing")
        | Error message -> Assert.That(message, Does.Contain("missing.dll"))

    [<Test>]
    member _.``Missing configuration is an error`` () =
        match EnvironmentConfiguration.read (Path.Combine(directory, Guid.NewGuid().ToString(), "fscript.toml")) with
        | Ok _ -> Assert.Fail("Expected failure")
        | Error message -> Assert.That(message, Does.Contain("fscript.toml"))

    [<Test>]
    member _.``CLI discovery records provenance without executing providers`` () =
        let dir = Path.Combine(Path.GetTempPath(), $"fscript-discovery-{Guid.NewGuid():N}")
        Directory.CreateDirectory(dir) |> ignore
        try
            let manifest = Path.Combine(dir, "fscript.toml")
            File.WriteAllText(manifest, "[runtime]\nextern-assemblies = ['missing.dll']")
            let settings = EnvironmentConfiguration.resolveForCli None false directory dir (Some directory) false [] |> get
            Assert.That(settings.ConfigurationPath, Is.EqualTo(Some manifest))
            Assert.That(settings.ExternAssemblies, Is.EqualTo(box [Path.Combine(dir, "missing.dll")]))
            let disabled = EnvironmentConfiguration.resolveForCli None true directory dir None false [] |> get
            Assert.That(disabled.ConfigurationPath, Is.EqualTo None)
            let explicitOnly = EnvironmentConfiguration.resolve None directory dir None false [] |> get
            Assert.That(explicitOnly.ConfigurationPath, Is.EqualTo None)
        finally Directory.Delete(dir, true)

    [<Test>]
    member _.``Compile defaults preserve omission and normalize manifest output paths`` () =
        let empty = EnvironmentConfiguration.parse configPath "" |> get
        Assert.That(empty.Compilation, Is.EqualTo(CompileConfiguration.empty))
        let parsed = EnvironmentConfiguration.parse configPath "[compile]\nbackend = 'DOTNET'\naot = true\nsize-optimized = false\nsingle-file = false\nruntime = 'osx-arm64'\noutput = '../artifacts'" |> get
        Assert.That(parsed.Compilation.Backend, Is.EqualTo(Some "dotnet"))
        Assert.That(parsed.Compilation.AheadOfTime, Is.EqualTo(Some true))
        Assert.That(parsed.Compilation.MinimizeSize, Is.EqualTo(Some false))
        Assert.That(parsed.Compilation.OutputDirectory, Is.EqualTo(Some(Path.Combine(directory, "artifacts"))))
        let settings = EnvironmentConfiguration.merge directory directory (Some parsed) None false []
        Assert.That(settings.Compilation, Is.EqualTo(parsed.Compilation))


    [<TestCase("[]", 0)>]
    [<TestCase("['io','THREADING']", 6)>]
    [<TestCase("['All']", 15)>]
    [<TestCase("['None']", 0)>]
    member _.``Permission configuration parses explicit selections`` (names: string, expected: int) =
        let parsed = EnvironmentConfiguration.parse configPath ("[runtime]\npermissions = " + names) |> get
        Assert.That(parsed.Permissions, Is.EqualTo(Some(enum<HostPermissions> expected)))
        let settings = EnvironmentConfiguration.merge directory directory (Some parsed) None false []
        Assert.That(settings.Permissions, Is.EqualTo(parsed.Permissions))

    [<TestCase("['All','IO']")>]
    [<TestCase("['None','IO']")>]
    [<TestCase("['Unknown']")>]
    [<TestCase("['']")>]
    [<TestCase("[1]")>]
    [<TestCase("'All'")>]
    member _.``Malformed permission configuration is rejected before providers load`` (names: string) =
        let parsed = EnvironmentConfiguration.parse configPath ("[runtime]\npermissions = " + names)
        match parsed with
        | Ok _ -> Assert.Fail("Invalid permissions accepted")
        | Error message -> Assert.That(message, Does.Contain("runtime.permissions"))

    [<Test>]
    member _.``Manifest permissions narrow host grants and CLI selections replace manifests`` () =
        let parsed = EnvironmentConfiguration.parse configPath "[runtime]\npermissions = ['All']" |> get
        let settings = EnvironmentConfiguration.merge directory directory (Some parsed) None false []
        let host = EnvironmentLoader.loadWithPermissions HostPermissions.IO settings |> get
        Assert.That(host.Permissions, Is.EqualTo(HostPermissions.IO))
        let safe = EnvironmentLoader.load settings |> get
        Assert.That(safe.Permissions, Is.EqualTo(HostPermissions.None))
        let narrowed = PermissionPolicy.forCli (Some "None") settings |> get
        Assert.That((EnvironmentLoader.loadWithPermissions HostPermissions.All narrowed |> get).Permissions, Is.EqualTo(HostPermissions.None))
        let omitted = EnvironmentConfiguration.merge directory directory None None false []
        Assert.That(omitted.Permissions, Is.EqualTo(None))
        Assert.That((EnvironmentLoader.loadWithPermissions HostPermissions.IO omitted |> get).Permissions, Is.EqualTo(HostPermissions.IO))
        Assert.That((PermissionPolicy.forCli None omitted |> get).Permissions, Is.EqualTo(Some HostPermissions.None))
