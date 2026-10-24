using System.Text.Json.Nodes;
using FScript.Cli.Tests.Fixtures.Valid;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

[TestFixture]
public sealed class EnvironmentTests
{
    private string _directory = null!;
    private string Config => Path.Combine(_directory, "fscript.toml");
    private string Uri => new Uri(Path.Combine(_directory, "script.fss")).AbsoluteUri;

    [SetUp]
    public void SetUp()
    {
        _directory = Path.Combine(Path.GetTempPath(), "fscript-lsp-environment-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(_directory);
        var assembly = typeof(ValidExternProvider).Assembly.Location.Replace("\\", "/");
        File.WriteAllText(Config, $"[runtime]\nextern-assemblies = ['{assembly}']\n");
    }

    [TearDown]
    public void TearDown() => Directory.Delete(_directory, true);

    private LspClient.Client Start(bool trusted = true)
    {
        var client = LspClient.Start();
        LspTestFixture.InitializeWith(client, new JsonObject
        {
            ["configurationPath"] = Config,
            ["workspaceFolderUri"] = new Uri(_directory + Path.DirectorySeparatorChar).AbsoluteUri,
            ["workspaceTrusted"] = trusted
        });
        return client;
    }

    private JsonObject Open(LspClient.Client client, string source, string? uri = null)
    {
        uri ??= Uri;
        LspClient.SendNotification(client, "textDocument/didOpen", new JsonObject
        {
            ["textDocument"] = new JsonObject { ["uri"] = uri, ["languageId"] = "fscript", ["version"] = 1, ["text"] = source }
        });
        return LspClient.ReadUntil(client, 10_000, message => message["method"]?.GetValue<string>() == "textDocument/publishDiagnostics" && message["params"]?["uri"]?.GetValue<string>() == uri);
    }

    private JsonNode? Request(LspClient.Client client, string method, int line = 0, int character = 0)
    {
        LspClient.SendRequest(client, 800, method, new JsonObject
        {
            ["textDocument"] = new JsonObject { ["uri"] = Uri },
            ["position"] = new JsonObject { ["line"] = line, ["character"] = character }
        });
        return LspClient.ReadUntil(client, 10_000, message => message["id"]?.ToString() == "800")["result"];
    }

    [Test]
    public void Real_provider_signatures_and_managed_fields_drive_editor_features()
    {
        File.WriteAllText(Path.Combine(_directory, "provider-registration.requested"), "");
        var client = Start();
        try
        {
            Assert.That(File.Exists(Path.Combine(_directory, "provider-registration.executed")), Is.True);
            var diagnostics = Open(client, "let node = Ext.node 3\nlet result = node.Value\nExt.answer\n");
            Assert.That(diagnostics["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Request(client, "textDocument/completion", 2, 4)?.ToJsonString(), Does.Contain("Ext.answer"));
            Assert.That(Request(client, "textDocument/completion", 1, 18)?.ToJsonString(), Does.Contain("Value").And.Not.Contain("Payload"));
            Assert.That(Request(client, "textDocument/hover", 1, 20)?.ToJsonString(), Does.Contain("int"));
            Assert.That(Request(client, "textDocument/hover", 0, 18)?.ToJsonString(), Does.Contain("Node"));
            var info = Request(client, "fscript/environmentInfo");
            Assert.That(Path.TrimEndingDirectorySeparator(info?["rootDirectory"]?.GetValue<string>() ?? ""), Is.EqualTo(_directory));
            Assert.That(info?["failed"]?.GetValue<bool>(), Is.False);
            Assert.That(Request(client, "fscript/viewInferredAst")?["ok"]?.GetValue<bool>(), Is.True);
        }
        finally { LspClient.Stop(client); }
    }

    [Test]
    public void Foreign_union_constructors_annotations_and_hover_use_provider_metadata()
    {
        var client = Start();
        try
        {
            var diagnostics = Open(client, "let mode (x: Protocol.Mode) = Ext.mode x\nmode (Protocol.Mode.Enabled 42)\n");
            Assert.That(diagnostics["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Request(client, "textDocument/completion", 0, 0)?.ToJsonString(), Does.Contain("Protocol.Mode.Enabled"));
            Assert.That(Request(client, "textDocument/hover", 0, 17)?.ToJsonString(), Does.Contain("Enabled"));
            Assert.That(Open(client, "Ext.mode (Enabled true)")["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty);
        }
        finally { LspClient.Stop(client); }
    }

    [TestCase("let node = Ext.node true")]
    [TestCase("let bad (node: Node) = node.Hidden")]
    [TestCase("let bad (node: Node) = { node with Value = 4 }")]
    [TestCase("let bad = Ext.acceptNode { Value = 4 }")]
    public void Provider_metadata_is_authoritative_for_diagnostics(string source)
    {
        var client = Start();
        try { Assert.That(Open(client, source)["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty); }
        finally { LspClient.Stop(client); }
    }

    [Test]
    public void Restricted_mode_does_not_execute_configured_providers()
    {
        File.WriteAllText(Path.Combine(_directory, "provider-registration.requested"), "");
        var client = Start(false);
        try
        {
            Assert.That(File.Exists(Path.Combine(_directory, "provider-registration.executed")), Is.False);
            Assert.That(Open(client, "Ext.answer")["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty);
            Assert.That(Request(client, "fscript/environmentInfo")?["blockedByTrust"]?.GetValue<bool>(), Is.True);
            Assert.That(Request(client, "textDocument/completion", 0, 4)?.ToJsonString(), Does.Not.Contain("Ext.answer"));
        }
        finally { LspClient.Stop(client); }
    }

    [Test]
    public void Failed_environment_reports_error_and_keeps_defaults_available()
    {
        File.WriteAllText(Config, "[runtime]\nextern-assemblies = ['missing.dll']");
        var client = Start();
        try
        {
            var info = Request(client, "fscript/environmentInfo");
            Assert.That(info?["failed"]?.GetValue<bool>(), Is.True);
            Assert.That(info?["error"]?.GetValue<string>(), Does.Contain(Config).And.Contain("missing.dll"));
            Assert.That(Open(client, "Console.writeLine \"ok\"")["params"]?["diagnostics"]?.AsArray(), Is.Empty);
        }
        finally { LspClient.Stop(client); }
    }

    [Test]
    public void Independent_processes_and_restarts_do_not_reuse_registrations_for_shared_source()
    {
        var first = Start();
        File.WriteAllText(Config, "[runtime]\ndefault-externs = false");
        var second = Start();
        try
        {
            Assert.That(Open(first, "Ext.answer")["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Open(second, "Ext.answer")["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty);
            Assert.That(Open(second, "Console.writeLine \"x\"")["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty);
            Assert.That(Request(second, "textDocument/completion", 0, 4)?.ToJsonString(), Does.Not.Contain("Ext.answer"));
        }
        finally { LspClient.Stop(first); LspClient.Stop(second); }
    }
    [Test]
    public void Replacing_provider_binary_and_restarting_drops_old_signatures()
    {
        var provider = Path.Combine(_directory, "provider.dll");
        File.Copy(typeof(ValidExternProvider).Assembly.Location, provider);
        File.WriteAllText(Config, "[runtime]\ndefault-externs = false\nextern-assemblies = ['./provider.dll']");
        var first = Start();
        try { Assert.That(Open(first, "Ext.answer")["params"]?["diagnostics"]?.AsArray(), Is.Empty); }
        finally { LspClient.Stop(first); }
        File.Copy(typeof(FScript.Cli.Tests.Fixtures.Conflict.ConflictExternProvider).Assembly.Location, provider, true);
        var second = Start();
        try
        {
            Assert.That(Open(second, "Fs.exists")["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Request(second, "textDocument/hover", 0, 3)?.ToJsonString(), Does.Contain("bool"));
            Assert.That(Open(second, "Ext.answer")["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty);
        }
        finally { LspClient.Stop(second); }
    }

    [Test]
    public void Shared_imports_are_checked_in_each_requesting_environment()
    {
        File.WriteAllText(Path.Combine(_directory, "shared.fss"), "let value = Ext.answer\n");
        var first = Start();
        File.WriteAllText(Config, "[runtime]\ndefault-externs = false");
        var second = Start();
        try
        {
            const string source = "import \"shared.fss\" as Shared\nShared.value\n";
            Assert.That(Open(first, source)["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Open(second, source)["params"]?["diagnostics"]?.AsArray(), Is.Not.Empty);
        }
        finally { LspClient.Stop(first); LspClient.Stop(second); }
    }

    [Test]
    public void Completion_survives_incomplete_source_and_lists_registered_host_types()
    {
        var client = Start();
        try
        {
            Open(client, "Ext.");
            Assert.That(Request(client, "textDocument/completion", 0, 4)?.ToJsonString(), Does.Contain("Ext.node"));
            Open(client, "let inspect (node: Node) = node.Value");
            Assert.That(Request(client, "textDocument/completion", 0, 21)?.ToJsonString(), Does.Contain("Node"));
            Assert.That(Request(client, "textDocument/hover", 0, 20)?.ToJsonString(), Does.Contain("host-managed"));
        }
        finally { LspClient.Stop(client); }
    }

    [Test]
    public void Default_environment_exposes_Env_fields_in_hello_script()
    {
        var client = LspClient.Start();
        LspTestFixture.Initialize(client);
        try
        {
            const string source = "let name =\n    match Env.Arguments with\n    | head :: _ -> head\n    | _ -> \"Unknown\"\nConsole.writeLine $\"Hello {name} !\"\n";
            Assert.That(Open(client, source)["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Request(client, "textDocument/completion", 1, 14)?.ToJsonString(), Does.Contain("Arguments").And.Contain("ScriptName"));
            Assert.That(Request(client, "textDocument/hover", 1, 17)?.ToJsonString(), Does.Contain("string list"));
        }
        finally { LspClient.Stop(client); }
    }

    [Test]
    public void Provider_constructible_records_support_literals_updates_and_type_hover()
    {
        var client = Start();
        try
        {
            const string source = "let point = { X = 1 }\nlet accept (point: Point) = Ext.acceptPoint { point with X = 42 }\naccept point";
            Assert.That(Open(client, source)["params"]?["diagnostics"]?.AsArray(), Is.Empty);
            Assert.That(Request(client, "textDocument/hover", 1, 20)?.ToJsonString(), Does.Contain("script-constructible record"));
            Assert.That(Request(client, "textDocument/completion", 1, 20)?.ToJsonString(), Does.Contain("Point"));
        }
        finally { LspClient.Stop(client); }
    }

}
