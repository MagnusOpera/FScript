using System.Text.Json.Nodes;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

public class PermissionEnvironmentTests
{
    [TestCase(null, "Console.writeLine \"denied\"", false)]
    [TestCase("[]", "Task.fromResult 42", false)]
    [TestCase("['IO']", "Console.writeLine \"allowed\"", true)]
    [TestCase("['IO']", "Task.fromResult 42", false)]
    [TestCase("['Threading']", "Task.fromResult 42", true)]
    [TestCase("['Invalid']", "Console.writeLine \"denied\"", false)]
    public void Editor_catalog_follows_script_permissions(string? permissions, string source, bool allowed)
    {
        var directory = Directory.CreateTempSubdirectory("fscript-lsp-permissions-");
        var client = LspClient.Start();
        try
        {
            JsonObject? options = null;
            if (permissions is not null)
            {
                var config = Path.Combine(directory.FullName, "fscript.toml");
                File.WriteAllText(config, $"[runtime]\npermissions = {permissions}\n");
                options = new JsonObject { ["configurationPath"] = config, ["workspaceTrusted"] = true };
            }
            LspTestFixture.InitializeWith(client, options);
            var uri = new Uri(Path.Combine(directory.FullName, "main.fss")).AbsoluteUri;
            LspClient.SendNotification(client, "textDocument/didOpen", new JsonObject { ["textDocument"] = new JsonObject {
                ["uri"] = uri, ["languageId"] = "fscript", ["version"] = 1, ["text"] = source
            }});
            var response = LspClient.ReadUntil(client, 10_000, m => m["method"]?.ToString() == "textDocument/publishDiagnostics" && m["params"]?["uri"]?.ToString() == uri);
            Assert.That(response["params"]!["diagnostics"]!.AsArray().Count, allowed ? Is.Zero : Is.GreaterThan(0));
        }
        finally { LspClient.Stop(client); directory.Delete(true); }
    }
}
