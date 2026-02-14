using System.Text.Json.Nodes;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

[TestFixture]
public sealed class CSharpServerCoreTests
{
    [Test]
    public void CSharp_server_initialize_returns_capabilities()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var hoverReq = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = "file:///tmp/test.fss" },
                ["position"] = new JsonObject { ["line"] = 0, ["character"] = 0 }
            };

            LspClient.SendRequest(client, 42, "textDocument/hover", hoverReq);
            var hoverResp = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 42);
            Assert.That(hoverResp["result"], Is.Null);
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_returns_stdlib_source()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);
            var requestParams = new JsonObject { ["uri"] = "fscript-stdlib:///Option.fss" };

            LspClient.SendRequest(client, 43, "fscript/stdlibSource", requestParams);
            var resp = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 43);

            var result = resp["result"] as JsonObject ?? throw new Exception("Expected result object");
            Assert.That(result["ok"]?.GetValue<bool>(), Is.True);
            var data = result["data"] as JsonObject ?? throw new Exception("Expected data object");
            var text = data["text"]?.GetValue<string>() ?? string.Empty;
            Assert.That(text.Contains("let", StringComparison.Ordinal), Is.True);
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_returns_method_not_found_for_unknown_request()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);
            LspClient.SendRequest(client, 44, "fscript/unknown", null);
            var resp = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 44);
            var err = resp["error"] as JsonObject ?? throw new Exception("Expected error object");
            Assert.That(err["code"]?.GetValue<int>(), Is.EqualTo(-32601));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_didOpen_publishes_parse_diagnostics()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-diagnostics-test.fss";
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = uri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = "let x ="
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);

            var diagMsg = LspClient.ReadUntil(client, 10_000, msg =>
            {
                if (msg["method"]?.GetValue<string>() != "textDocument/publishDiagnostics")
                {
                    return false;
                }

                var p = msg["params"] as JsonObject;
                var u = p?["uri"]?.GetValue<string>();
                var diagnostics = p?["diagnostics"] as JsonArray;
                return u == uri && diagnostics is { Count: > 0 };
            });

            var hasParseCode = false;
            var paramsObj = diagMsg["params"] as JsonObject;
            var diagnosticsArray = paramsObj?["diagnostics"] as JsonArray;
            if (diagnosticsArray is not null)
            {
                foreach (var diag in diagnosticsArray)
                {
                    if (diag is JsonObject d && d["code"]?.GetValue<string>() == "parse")
                    {
                        hasParseCode = true;
                        break;
                    }
                }
            }

            Assert.That(hasParseCode, Is.True);
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_viewAst_returns_program_json()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-view-ast-test.fss";
            var source = "let value = 42\nvalue\n";
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = uri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var requestParams = new JsonObject { ["textDocument"] = new JsonObject { ["uri"] = uri } };
            LspClient.SendRequest(client, 45, "fscript/viewAst", requestParams);
            var response = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 45);

            var kindValue = ((response["result"] as JsonObject)?["data"] as JsonObject)?["kind"]?.GetValue<string>();
            Assert.That(kindValue, Is.EqualTo("program"));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_viewInferredAst_returns_typed_program_json()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-view-inferred-test.fss";
            var source = "let inc x = x + 1\ninc 1\n";
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = uri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var requestParams = new JsonObject { ["textDocument"] = new JsonObject { ["uri"] = uri } };
            LspClient.SendRequest(client, 46, "fscript/viewInferredAst", requestParams);
            var response = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 46);

            var kindValue = ((response["result"] as JsonObject)?["data"] as JsonObject)?["kind"]?.GetValue<string>();
            Assert.That(kindValue, Is.EqualTo("typedProgram"));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }
}
