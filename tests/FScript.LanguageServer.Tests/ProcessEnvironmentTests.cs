using System.Text.Json.Nodes;
using FScript.Cli.Tests.Fixtures.Valid;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

public class ProcessEnvironmentTests
{
    [TestCase(false, true)]
    [TestCase(true, false)]
    [TestCase(true, true)]
    public void Process_follows_default_externs_configuration(bool defaults, bool trusted)
    {
        using var fixture = new NuGetFixture();
        File.WriteAllText(fixture.ConfigPath, $"[runtime]\npermissions = ['Native']\ndefault-externs = {defaults.ToString().ToLowerInvariant()}\n");
        var client = LspClient.Start();
        try
        {
            LspTestFixture.InitializeWith(client, new JsonObject {
                ["configurationPath"] = fixture.ConfigPath,
                ["workspaceFolderUri"] = new Uri(fixture.Directory + Path.DirectorySeparatorChar).AbsoluteUri,
                ["workspaceTrusted"] = trusted
            });
            var uri = new Uri(Path.Combine(fixture.Directory, "main.fss")).AbsoluteUri;
            LspClient.SendNotification(client, "textDocument/didOpen", new JsonObject { ["textDocument"] = new JsonObject {
                ["uri"] = uri, ["languageId"] = "fscript", ["version"] = 1,
                ["text"] = "let result = Process.exec \"missing\" [] Process.defaults\n"
            }});
            var response = LspClient.ReadUntil(client, 10_000, m => m["method"]?.ToString() == "textDocument/publishDiagnostics" && m["params"]?["uri"]?.ToString() == uri);
            Assert.That(response["params"]!["diagnostics"]!.AsArray().Count, defaults || !trusted ? Is.EqualTo(0) : Is.GreaterThan(0));
        }
        finally { LspClient.Stop(client); }
    }
}
