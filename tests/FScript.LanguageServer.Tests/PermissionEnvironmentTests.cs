using System.Text.Json.Nodes;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

public class PermissionEnvironmentTests
{
    [TestCase("math_test.fss", true)]
    [TestCase("math.fss", false)]
    public void Test_apis_have_editor_types_in_test_files(string filename, bool testFile)
    {
        var directory = Directory.CreateTempSubdirectory("fscript-lsp-tests-");
        var client = LspClient.Start();
        try
        {
            LspTestFixture.InitializeWith(client, null);
            var uri = new Uri(Path.Combine(directory.FullName, filename)).AbsoluteUri;
            LspClient.SendNotification(client, "textDocument/didOpen", new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = uri, ["languageId"] = "fscript", ["version"] = 1,
                    ["text"] = "Test.each \"doubling\" [(\"positive\", 2)] (fun x -> Assert.equal 4 (x * 2))"
                }
            });
            var diagnostics = LspClient.ReadUntil(client, 10_000,
                message => message["method"]?.ToString() == "textDocument/publishDiagnostics" && message["params"]?["uri"]?.ToString() == uri);
            Assert.That(diagnostics["params"]!["diagnostics"]!.AsArray().Count, testFile ? Is.Zero : Is.GreaterThan(0));
        }
        finally { LspClient.Stop(client); directory.Delete(true); }
    }

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
