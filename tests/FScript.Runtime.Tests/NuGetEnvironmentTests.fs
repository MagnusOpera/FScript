namespace FScript.Runtime.Tests

open System
open System.IO
open NUnit.Framework
open FScript.Runtime
open FScript.Language
open FScript.Cli.Tests.Fixtures.Valid

[<TestFixture>]
type NuGetEnvironmentTests() =
    let get = function Ok value -> value | Error message -> failwith message
    let settings (fixture: NuGetFixture) =
        EnvironmentConfiguration.resolve (Some fixture.ConfigPath) fixture.Directory fixture.Directory None false [] |> get

    [<Test>]
    member _.``Packages preserve omitted versions and parse without executing or downloading`` () =
        let parsed = EnvironmentConfiguration.parse "/tmp/fscript.toml" "[[runtime.packages]]\nname = 'Missing.Package'\n[[runtime.packages]]\nname = 'Pinned'\nversion = '1.2.3-preview.1'" |> get
        Assert.That(parsed.Packages, Is.EqualTo(Some [{ Name = "Missing.Package"; Version = None }; { Name = "Pinned"; Version = Some "1.2.3-preview.1" }]))
        let effective = EnvironmentConfiguration.merge "/tmp" "/tmp" (Some parsed) None false []
        Assert.That(effective.Packages.Length, Is.EqualTo 2)

    [<TestCase("[[runtime.packages]]\nversion = '1.0.0'", "name")>]
    [<TestCase("[[runtime.packages]]\nname = ''", "name")>]
    [<TestCase("[[runtime.packages]]\nname = 42", "name")>]
    [<TestCase("[[runtime.packages]]\nname = 'a'\nversion = ''", "version")>]
    [<TestCase("[[runtime.packages]]\nname = 'a'\nversion = '*'", "version")>]
    [<TestCase("[[runtime.packages]]\nname = 'a'\nversion = '[1.0,2.0)'", "version")>]
    [<TestCase("[[runtime.packages]]\nname = 'a'\nversion = 1", "version")>]
    [<TestCase("[[runtime.packages]]\nname = 'a'\nunknown = true", "unknown")>]
    [<TestCase("[[runtime.packages]]\nname = 'a'\n[[runtime.packages]]\nname = 'A'", "Duplicate")>]
    [<TestCase("[runtime]\npackages = ['a']", "packages")>]
    member _.``Invalid package declarations fail with actionable properties`` (source, expected) =
        match EnvironmentConfiguration.parse "/tmp/fscript.toml" source with
        | Ok _ -> Assert.Fail "Invalid package declaration was accepted"
        | Error message -> Assert.That(message, Does.Contain(expected).And.Contain("fscript.toml"))

    [<TestCase("1.0.0", "1.0.0")>]
    [<TestCase("", "2.0.0")>]
    [<TestCase("3.0.0-preview.1", "3.0.0-preview.1")>]
    member _.``Pinned latest stable and explicit preview packages discover attributed providers`` (requested: string, expected: string) =
        use fixture = new NuGetFixture()
        for version in ["1.0.0"; "2.0.0"; "3.0.0-preview.1"] do fixture.AddProvider version
        fixture.WriteManifest requested
        let loaded = EnvironmentLoader.loadWithInfo (settings fixture) |> get
        Assert.That(loaded.Packages.Head.Version, Is.EqualTo expected)
        Assert.That(loaded.AssemblyPaths |> List.exists File.Exists, Is.True)
        Assert.That(loaded.AssemblyPaths.Head, Does.StartWith(Path.Combine(fixture.Directory, ".fscript")))
        Assert.That(Directory.Exists(Path.Combine(fixture.Directory,".fscript","packages", fixture.Name.ToLowerInvariant())), Is.True)
        let script = ScriptHost.loadSourceWithRegistration loaded.Registration "Ext.acceptNode (Ext.node 42)"
        Assert.That(Pretty.valueToString script.LastValue, Is.EqualTo "42")
        // A second initialization reuses cached runtime assets without corrupting loaded files.
        let again = EnvironmentLoader.loadWithInfo (settings fixture) |> get
        Assert.That(again.Packages.Head.Version, Is.EqualTo expected)

    [<Test>]
    member _.``Latest is reevaluated when a newer stable version becomes available`` () =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        fixture.WriteManifest ""
        Assert.That((EnvironmentLoader.loadWithInfo (settings fixture) |> get).Packages.Head.Version, Is.EqualTo "1.0.0")
        fixture.AddProvider "2.0.0"
        Assert.That((EnvironmentLoader.loadWithInfo (settings fixture) |> get).Packages.Head.Version, Is.EqualTo "2.0.0")

    [<Test>]
    member _.``Missing and non-provider packages fail atomically`` () =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        fixture.WriteManifest "9.0.0"
        match EnvironmentLoader.load (settings fixture) with
        | Ok _ -> Assert.Fail "Missing version was accepted"
        | Error message -> Assert.That(message, Does.Contain("NuGet").And.Contain(fixture.Name))
        let plain = fixture.Name + ".Plain"
        fixture.AddPackage(plain, "1.0.0", [| typeof<FScript.Extensibility.HostContext>.Assembly.Location |], "")
        File.WriteAllText(fixture.ConfigPath, $"[[runtime.packages]]\nname = '{plain}'\nversion = '1.0.0'")
        match EnvironmentLoader.load (settings fixture) with
        | Ok _ -> Assert.Fail "Package without a provider was accepted"
        | Error message -> Assert.That(message, Does.Contain("FScriptExternProvider").And.Contain(plain))

    [<Test>]
    member _.``Transitive runtime assets are staged without treating dependencies as providers`` () =
        use fixture = new NuGetFixture()
        let dependency = fixture.Name + ".Dependency"
        fixture.AddPackage(dependency, "1.0.0", [| typeof<FScript.Extensibility.HostContext>.Assembly.Location |], "")
        fixture.AddPackage(fixture.Name, "1.0.0", [| typeof<ValidExternProvider>.Assembly.Location |], $"<dependencies><group targetFramework='net10.0'><dependency id='{dependency}' version='[1.0.0]'/></group></dependencies>")
        fixture.WriteManifest "1.0.0"
        let loaded = EnvironmentLoader.loadWithInfo (settings fixture) |> get
        Assert.That(loaded.AssemblyPaths |> List.exists (fun p -> Path.GetFileName(p) = "FScript.Extensibility.dll"), Is.True)
        Assert.That(loaded.Packages.Length, Is.EqualTo 1)
        Assert.That(loaded.Registration.Functions |> List.exists (fun f -> f.Name = "Ext.node"), Is.True)

    [<Test>]
    member _.``Incompatible pinned dependency constraints are errors`` () =
        use fixture = new NuGetFixture()
        let dependency = fixture.Name + ".Dependency"
        fixture.AddPackage(dependency, "1.0.0", [| typeof<FScript.Extensibility.HostContext>.Assembly.Location |], "")
        fixture.AddPackage(dependency, "2.0.0", [| typeof<FScript.Extensibility.HostContext>.Assembly.Location |], "")
        fixture.AddPackage(fixture.Name, "1.0.0", [| typeof<ValidExternProvider>.Assembly.Location |], $"<dependencies><group targetFramework='net10.0'><dependency id='{dependency}' version='[2.0.0]'/></group></dependencies>")
        fixture.WriteManifest "1.0.0"
        File.AppendAllText(fixture.ConfigPath, $"\n[[runtime.packages]]\nname = '{dependency}'\nversion = '1.0.0'\n")
        match EnvironmentLoader.load (settings fixture) with
        | Ok _ -> Assert.Fail "A dependency downgrade was accepted"
        | Error message -> Assert.That(message, Does.Contain(dependency))

    [<Test>]
    member _.``Package registrations persist in interactive submissions`` () =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        fixture.WriteManifest "1.0.0"
        let registration = EnvironmentLoader.load (settings fixture) |> get
        let first = Interactive.submit (Interactive.createWithRegistration registration) (FScript.parse "let node = Ext.node 42")
        let second = Interactive.submit first.Session (FScript.parse "Ext.acceptNode node")
        Assert.That(Pretty.valueToString second.State.LastValue, Is.EqualTo "42")

    [<Test>]
    member _.``Pinned package reuses manifest local cache when the source is offline`` () =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        fixture.WriteManifest "1.0.0"
        EnvironmentLoader.load (settings fixture) |> get |> ignore
        Directory.Delete(Path.Combine(fixture.Directory, "feed"), true)
        let registration = EnvironmentLoader.load (settings fixture) |> get
        let script = ScriptHost.loadSourceWithRegistration registration "Ext.answer"
        Assert.That(Pretty.valueToString script.LastValue, Is.EqualTo "42")

    [<Test>]
    member _.``Package and local provider registrations share conflict detection`` () =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        fixture.WriteManifest "1.0.0"
        let assembly = typeof<ValidExternProvider>.Assembly.Location
        File.AppendAllText(fixture.ConfigPath, $"\n[runtime]\nextern-assemblies = ['{assembly}']\n")
        match EnvironmentLoader.load (settings fixture) with
        | Ok _ -> Assert.Fail "Duplicate package and local registrations were accepted"
        | Error message -> Assert.That(message, Does.Contain("conflicts").And.Contain("Ext.answer"))

    [<TestCase(true)>]
    [<TestCase(false)>]
    member _.``Two packages exchange shared opaque payloads in one isolated dependency context`` (explicitOwner: bool) =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        let consumer = fixture.Name + ".Consumer"
        let assembly = typeof<FScript.Cli.Tests.Fixtures.Consumer.ConsumerExternProvider>.Assembly.Location
        fixture.AddPackage(consumer, "1.0.0", [| assembly |], $"<dependencies><group targetFramework='net10.0'><dependency id='{fixture.Name}' version='[1.0.0]'/></group></dependencies>")
        fixture.WriteManifest "1.0.0"
        if not explicitOwner then File.WriteAllText(fixture.ConfigPath, "")
        File.AppendAllText(fixture.ConfigPath, $"\n[[runtime.packages]]\nname = '{consumer}'\nversion = '1.0.0'\n")
        let registration = EnvironmentLoader.load (settings fixture) |> get
        let consumed = ScriptHost.loadSourceWithRegistration registration "Consumer.readNode (Ext.node 42)"
        Assert.That(Pretty.valueToString consumed.LastValue, Is.EqualTo "42")
        let node = ScriptHost.loadSourceWithRegistration registration "Ext.node 42"
        match node.LastValue with
        | VHostManaged host ->
            // The test host already references this assembly; NuGet must use its own copy.
            Assert.That(obj.ReferenceEquals(host.Payload.GetType().Assembly, typeof<ProviderNode>.Assembly), Is.False)
            let another = EnvironmentLoader.load (settings fixture) |> get
            let second = ScriptHost.loadSourceWithRegistration another "Ext.node 42"
            match second.LastValue with
            | VHostManaged other -> Assert.That(obj.ReferenceEquals(host.Payload.GetType().Assembly, other.Payload.GetType().Assembly), Is.False)
            | _ -> Assert.Fail "Expected opaque Node"
        | _ -> Assert.Fail "Expected opaque Node"

    [<Test>]
    member _.``Different packages cannot introduce conflicting CLR assembly identities`` () =
        use fixture = new NuGetFixture()
        let original = typeof<ValidExternProvider>.Assembly.Location
        let alternate = Path.Combine(fixture.Directory, "Alternate.dll")
        File.Copy(original, alternate)
        // A valid PE with the same assembly name but different content under another filename.
        use stream = new FileStream(alternate, FileMode.Append)
        stream.WriteByte 0uy
        stream.Close()
        let dependency = fixture.Name + ".Collision"
        fixture.AddPackage(dependency, "1.0.0", [| alternate |], "")
        fixture.AddPackage(fixture.Name, "1.0.0", [| original |], $"<dependencies><group targetFramework='net10.0'><dependency id='{dependency}' version='[1.0.0]'/></group></dependencies>")
        fixture.WriteManifest "1.0.0"
        match EnvironmentLoader.load (settings fixture) with
        | Ok _ -> Assert.Fail "Conflicting CLR identities were accepted"
        | Error message -> Assert.That(message, Does.Contain("conflicting runtime assemblies"))
