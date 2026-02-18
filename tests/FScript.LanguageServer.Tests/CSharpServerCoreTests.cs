using System.Text.Json.Nodes;
using NUnit.Framework;
using System.IO;

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
    public void CSharp_server_didOpen_accepts_Env_binding_without_unbound_diagnostics()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-env-test.fss";
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = uri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = "Env.Arguments |> List.length"
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
                return u == uri;
            });

            var paramsObj = diagMsg["params"] as JsonObject;
            var diagnosticsArray = paramsObj?["diagnostics"] as JsonArray;
            Assert.That(diagnosticsArray, Is.Not.Null);
            Assert.That(diagnosticsArray!.Count, Is.EqualTo(0));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_typeDefinition_resolves_Environment_to_stdlib()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-env-typedef-test.fss";
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = uri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = "let size = Env.Arguments |> List.length\nsize\n"
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg =>
                msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = uri },
                ["position"] = new JsonObject { ["line"] = 0, ["character"] = 11 }
            };
            LspClient.SendRequest(client, 142, "textDocument/typeDefinition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000,
                msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 142);

            var result = definitionResponse["result"] as JsonObject
                ?? throw new Exception("Expected typeDefinition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo("fscript-stdlib:///Environment.fss"));
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

    [Test]
    public void CSharp_server_completion_uses_member_insertText_for_dotted_prefix()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-completion-dotted-prefix.fss";
            var source = "let _ = Option.map\n";
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

            var requestParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = uri },
                ["position"] = new JsonObject { ["line"] = 0, ["character"] = 15 }
            };

            LspClient.SendRequest(client, 60, "textDocument/completion", requestParams);
            var response = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 60);
            var items = ((response["result"] as JsonObject)?["items"] as JsonArray) ?? new JsonArray();

            var optionMapItem = items
                .OfType<JsonObject>()
                .FirstOrDefault(item => string.Equals(item["label"]?.GetValue<string>(), "Option.map", StringComparison.Ordinal));

            Assert.That(optionMapItem, Is.Not.Null, "Expected Option.map completion item at dotted prefix.");
            Assert.That(optionMapItem!["insertText"]?.GetValue<string>(), Is.EqualTo("map"));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_inlayHints_do_not_pollute_type_declaration_lines()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var repoRoot = FindRepoRoot();
            var samplePath = Path.Combine(repoRoot, "samples", "types-showcase.fss");
            var source = File.ReadAllText(samplePath);
            var uri = new Uri(samplePath).AbsoluteUri;

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

            var requestParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = uri },
                ["range"] = new JsonObject
                {
                    ["start"] = new JsonObject { ["line"] = 0, ["character"] = 0 },
                    ["end"] = new JsonObject { ["line"] = 3, ["character"] = 0 }
                }
            };
            LspClient.SendRequest(client, 47, "textDocument/inlayHint", requestParams);
            var response = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 47);

            var result = response["result"] as JsonArray ?? new JsonArray();
            var labels = result
                .Select(node =>
                {
                    var obj = node as JsonObject;
                    var label = obj?["label"]?.GetValue<string>() ?? string.Empty;
                    var pos = obj?["position"] as JsonObject;
                    var line = pos?["line"]?.GetValue<int>() ?? -1;
                    var character = pos?["character"]?.GetValue<int>() ?? -1;
                    return $"{label}@{line}:{character}";
                })
                .Where(x => !string.IsNullOrWhiteSpace(x))
                .ToList();

            Assert.That(labels.Any(label => label.Contains("address:", StringComparison.OrdinalIgnoreCase)), Is.False, string.Join(", ", labels));
            Assert.That(labels.Any(label => label.Contains(": address", StringComparison.OrdinalIgnoreCase)), Is.False, string.Join(", ", labels));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_inlayHints_infer_map_pattern_binding_types()
    {
        var client = LspClient.StartCSharp();
        try
        {
            LspTestFixture.Initialize(client);

            var uri = "file:///tmp/csharp-map-pattern-inlay-test.fss";
            var source = """
let scores = { ["a"] = 1; ["b"] = 2 }
let mapPreview =
    match scores with
    | {} ->
        "empty"
    | { [subject] = score; ..remaining } ->
        $"{subject}:{score}:{Map.count remaining}"
""";
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

            var requestParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = uri },
                ["range"] = new JsonObject
                {
                    ["start"] = new JsonObject { ["line"] = 5, ["character"] = 0 },
                    ["end"] = new JsonObject { ["line"] = 5, ["character"] = 80 }
                }
            };
            LspClient.SendRequest(client, 48, "textDocument/inlayHint", requestParams);
            var response = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 48);

            var result = response["result"] as JsonArray ?? new JsonArray();
            var labels = result
                .Select(node => (node as JsonObject)?["label"]?.GetValue<string>() ?? string.Empty)
                .Where(label => !string.IsNullOrWhiteSpace(label))
                .ToList();

            Assert.That(labels.Contains(": string"), Is.True, string.Join(", ", labels));
            Assert.That(labels.Contains(": int"), Is.True, string.Join(", ", labels));
            Assert.That(labels.Contains(": int map"), Is.True, string.Join(", ", labels));
            Assert.That(labels.Contains(": unknown"), Is.False, string.Join(", ", labels));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
        }
    }

    [Test]
    public void CSharp_server_displays_alias_types_and_navigates_alias_qualified_type_annotations()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-alias-types-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var commonPath = Path.Combine(tempDir, "common.fss");
            var mainPath = Path.Combine(tempDir, "main.fss");
            File.WriteAllText(commonPath, "type ProjectInfo = { Name: string; Language: string }\n");
            var source = """
import "common.fss" as Common

[<export>] let summary (project: Common.ProjectInfo) =
    project.Name
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var commonUri = new Uri(commonPath).AbsoluteUri;

            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var inlayParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["range"] = new JsonObject
                {
                    ["start"] = new JsonObject { ["line"] = 2, ["character"] = 0 },
                    ["end"] = new JsonObject { ["line"] = 2, ["character"] = 80 }
                }
            };
            LspClient.SendRequest(client, 81, "textDocument/inlayHint", inlayParams);
            var inlayResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 81);
            var inlayLabels = (inlayResponse["result"] as JsonArray ?? new JsonArray())
                .OfType<JsonObject>()
                .Select(item => item["label"]?.GetValue<string>() ?? string.Empty)
                .Where(label => !string.IsNullOrWhiteSpace(label))
                .ToList();

            Assert.That(inlayLabels.Any(label => label.Contains("__imp", StringComparison.Ordinal)), Is.False, string.Join(", ", inlayLabels));

            var hoverParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = 2, ["character"] = 31 }
            };
            LspClient.SendRequest(client, 83, "textDocument/hover", hoverParams);
            var hoverResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 83);
            var hoverResult = hoverResponse["result"] as JsonObject ?? throw new Exception("Expected hover result.");
            var contents = hoverResult["contents"] as JsonObject ?? throw new Exception("Expected hover contents.");
            var hoverText = contents["value"]?.GetValue<string>() ?? string.Empty;
            Assert.That(hoverText.Contains("Common.ProjectInfo", StringComparison.Ordinal), Is.True, hoverText);
            Assert.That(hoverText.Contains("__imp", StringComparison.Ordinal), Is.False, hoverText);

            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = 2, ["character"] = 41 }
            };
            LspClient.SendRequest(client, 82, "textDocument/typeDefinition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 82);
            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected typeDefinition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(commonUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(0));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_navigates_alias_qualified_function_calls()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-alias-functions-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var helpersPath = Path.Combine(tempDir, "_helpers.fss");
            var mainPath = Path.Combine(tempDir, "main.fss");
            File.WriteAllText(helpersPath, """
let append_part part acc =
    if part = "" then acc else $"{acc} {part}"
""");

            var source = """
import "_helpers.fss" as Helpers

let append_build_arg acc key value =
    Helpers.append_part $"--build-arg {key}=\"{value}\"" acc
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var helpersUri = new Uri(helpersPath).AbsoluteUri;

            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = 3, ["character"] = 17 }
            };
            LspClient.SendRequest(client, 84, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 84);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(helpersUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(0));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_navigates_local_parameter_usage_to_parameter_declaration()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-local-parameter-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let with_batch_projects context create_command =
    match context with
    | Some batch ->
        create_command batch
    | _ ->
        [create_command ""]
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "create_command", true);
            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = usagePosition.character }
            };
            LspClient.SendRequest(client, 85, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 85);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
            var declarationPosition = FindTokenPosition(source, "create_command", false);
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
            Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_prefers_nearest_shadowed_local_binding()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-local-shadow-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let with_shadow value =
    let map value =
        let value = value + 1
        value
    map value
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = 3, ["character"] = 10 }
            };
            LspClient.SendRequest(client, 86, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 86);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
            var declarationPosition = FindTokenPosition(source, "let value = value + 1", false);
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
            Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_navigates_top_level_tuple_pattern_bindings()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-top-level-tuple-pattern-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let (firstRaw, secondRaw) =
    match Env.Arguments with
    | first :: second :: _ -> (first, second)
    | _ -> ("0", "0")

let (first, second) =
    match (Int.tryParse firstRaw, Int.tryParse secondRaw) with
    | (Some firstValue, Some secondValue) -> (firstValue, secondValue)
    | _ -> raise "Invalid arguments: 2 ints expected"

first + second
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "second", true);
            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = usagePosition.character }
            };
            LspClient.SendRequest(client, 87, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 87);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");

            var tupleDeclarationLine = source.Split('\n')[5].TrimEnd('\r');
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(5));
            Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(tupleDeclarationLine.IndexOf("second", StringComparison.Ordinal)));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_navigates_match_pattern_binding_in_top_level_tuple_let_expression()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-match-pattern-binding-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let (firstRaw, secondRaw) =
    match Env.Arguments with
    | first :: second :: _ -> (first, second)
    | _ -> ("0", "0")

let (first, second) =
    match (Int.tryParse firstRaw, Int.tryParse secondRaw) with
    | (Some firstValue, Some secondValue) -> (firstValue, secondValue)
    | _ -> raise "Invalid arguments: 2 ints expected"

first + second
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "secondValue)", true);
            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = usagePosition.character }
            };
            LspClient.SendRequest(client, 88, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 88);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");

            var declarationPosition = FindTokenPosition(source, "secondValue", false);
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
            Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_resolves_local_binding_from_token_edge_position()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-local-binding-edge-position-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
[<export>] let restore (context: Protocol.ActionContext) (locked: bool option) (evaluate: bool option) (args: string option) =
  let locked = locked |> Helpers.map_true_default true "--locked-mode"
  let force_evaluate = evaluate |> Helpers.map_true "--force-evaluate"
  let args = args |> Helpers.with_args

  let create_command project_file =
    ""
    |> Helpers.append_part "restore"
    |> Helpers.append_part project_file
    |> Helpers.append_part locked
    |> Helpers.append_part force_evaluate
    |> Helpers.append_part args
    |> command_op

  { Batchable = true; Operations = with_batch_projects context create_command }
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "create_command }", false);
            var edgeCharacter = usagePosition.character + "create_command".Length;
            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = edgeCharacter }
            };
            LspClient.SendRequest(client, 89, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 89);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");

            var declarationPosition = FindTokenPosition(source, "let create_command project_file", false);
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
            Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_resolves_local_symbol_inside_record_field_value()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-local-record-field-value-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let create_command project_file =
  project_file

let operation =
  { Operations = create_command }

operation
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "create_command }", false);
            var definitionParams = new JsonObject
            {
                ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = usagePosition.character }
            };
            LspClient.SendRequest(client, 90, "textDocument/definition", definitionParams);
            var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 90);

            var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
            Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
            var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
            var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");

            var declarationPosition = FindTokenPosition(source, "let create_command project_file", false);
            Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
            Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_resolves_create_command_argument_inside_record_operation_expression()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-record-operation-create-command-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let with_batch_projects context create_command =
  match context with
  | Some _ -> [create_command ""]
  | _ -> [create_command ""]

[<export>] let restore context =
  let create_command project_file =
    project_file

  { Batchable = true; Operations = with_batch_projects context create_command }
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "create_command }", false);
            var declarationPosition = FindTokenPosition(source, "let create_command project_file", false);
            var probeCharacters = new[]
            {
                usagePosition.character,
                usagePosition.character + "create_command".Length - 1,
                usagePosition.character + "create_command".Length,
                usagePosition.character + "create_command".Length + 1,
                usagePosition.character + "create_command".Length + 2
            };

            for (var i = 0; i < probeCharacters.Length; i++)
            {
                var definitionParams = new JsonObject
                {
                    ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                    ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = probeCharacters[i] }
                };
                var requestId = 100 + i;
                LspClient.SendRequest(client, requestId, "textDocument/definition", definitionParams);
                var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == requestId);

                var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
                Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
                var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
                var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
                Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
                Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
            }
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_resolves_force_evaluate_binding_inside_restore_pipeline()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-restore-force-evaluate-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let with_batch_projects context create_command =
  [create_command ""]

[<export>] let restore context (locked: bool option) (evaluate: bool option) (args: string option) =
  let locked = locked
  let force_evaluate = evaluate
  let args = args

  let create_command project_file =
    ""
    |> Helpers.append_part "restore"
    |> Helpers.append_part project_file
    |> Helpers.append_part locked
    |> Helpers.append_part force_evaluate
    |> Helpers.append_part args
    |> command_op

  { Batchable = true; Operations = with_batch_projects context create_command }
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "force_evaluate", true);
            var declarationPosition = FindTokenPosition(source, "let force_evaluate = evaluate", false);
            var probeCharacters = new[]
            {
                usagePosition.character,
                usagePosition.character + "force_evaluate".Length - 1,
                usagePosition.character + "force_evaluate".Length
            };

            for (var i = 0; i < probeCharacters.Length; i++)
            {
                var definitionParams = new JsonObject
                {
                    ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                    ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = probeCharacters[i] }
                };
                var requestId = 200 + i;
                LspClient.SendRequest(client, requestId, "textDocument/definition", definitionParams);
                var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == requestId);

                var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
                Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
                var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
                var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
                Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
                Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
            }
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_resolves_dependencies_binding_inside_record_value()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-record-dependencies-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
[<export>] let defaults (context: Protocol.ActionContext) =
  let dependencies =
    context.Directory
    |> first_project_file
    |> Option.map (fun project_file -> dependency_dirs (project_refs_from project_file))
    |> Option.defaultValue []

  { Id = None; Outputs = ["bin/"; "obj/"; "**/*.binlog"]; Dependencies = dependencies }
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "dependencies }", false);
            var declarationPosition = FindTokenPosition(source, "let dependencies =", false);
            var probeCharacters = new[]
            {
                FindTokenPosition(source, "Dependencies", false).character,
                usagePosition.character,
                usagePosition.character + "dependencies".Length - 1,
                usagePosition.character + "dependencies".Length
            };

            for (var i = 0; i < probeCharacters.Length; i++)
            {
                var definitionParams = new JsonObject
                {
                    ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                    ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = probeCharacters[i] }
                };
                var requestId = 300 + i;
                LspClient.SendRequest(client, requestId, "textDocument/definition", definitionParams);
                var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == requestId);

                var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
                Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
                var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
                var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
                Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
                Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
            }
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    [Test]
    public void CSharp_server_definition_resolves_symbol_inside_string_interpolation()
    {
        var client = LspClient.StartCSharp();
        var tempDir = Path.Combine(Path.GetTempPath(), $"fscript-lsp-string-interpolation-definition-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDir);
        try
        {
            LspTestFixture.Initialize(client);

            var mainPath = Path.Combine(tempDir, "main.fss");
            var source = """
let force_evaluate = "--force-evaluate"
let summary = $"flag={force_evaluate}"
summary
""";
            File.WriteAllText(mainPath, source);

            var mainUri = new Uri(mainPath).AbsoluteUri;
            var didOpenParams = new JsonObject
            {
                ["textDocument"] = new JsonObject
                {
                    ["uri"] = mainUri,
                    ["languageId"] = "fscript",
                    ["version"] = 1,
                    ["text"] = source
                }
            };
            LspClient.SendNotification(client, "textDocument/didOpen", didOpenParams);
            _ = LspClient.ReadUntil(client, 10_000, msg => msg["method"]?.GetValue<string>() == "textDocument/publishDiagnostics");

            var usagePosition = FindTokenPosition(source, "force_evaluate}", false);
            var declarationPosition = FindTokenPosition(source, "let force_evaluate =", false);
            var probeCharacters = new[]
            {
                usagePosition.character,
                usagePosition.character + "force_evaluate".Length - 1,
                usagePosition.character + "force_evaluate".Length
            };

            for (var i = 0; i < probeCharacters.Length; i++)
            {
                var definitionParams = new JsonObject
                {
                    ["textDocument"] = new JsonObject { ["uri"] = mainUri },
                    ["position"] = new JsonObject { ["line"] = usagePosition.line, ["character"] = probeCharacters[i] }
                };
                var requestId = 400 + i;
                LspClient.SendRequest(client, requestId, "textDocument/definition", definitionParams);
                var definitionResponse = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == requestId);

                var result = definitionResponse["result"] as JsonObject ?? throw new Exception("Expected definition location.");
                Assert.That(result["uri"]?.GetValue<string>(), Is.EqualTo(mainUri));
                var range = result["range"] as JsonObject ?? throw new Exception("Expected range.");
                var start = range["start"] as JsonObject ?? throw new Exception("Expected start range.");
                Assert.That(start["line"]?.GetValue<int>(), Is.EqualTo(declarationPosition.line));
                Assert.That(start["character"]?.GetValue<int>(), Is.EqualTo(declarationPosition.character));
            }
        }
        finally
        {
            try { LspTestFixture.Shutdown(client); } catch { }
            LspClient.Stop(client);
            if (Directory.Exists(tempDir))
            {
                Directory.Delete(tempDir, true);
            }
        }
    }

    private static (int line, int character) FindTokenPosition(string source, string token, bool fromEnd)
    {
        var index = fromEnd
            ? source.LastIndexOf(token, StringComparison.Ordinal)
            : source.IndexOf(token, StringComparison.Ordinal);

        if (index < 0)
        {
            throw new Exception($"Token '{token}' not found.");
        }

        var line = 0;
        var character = 0;
        for (var i = 0; i < index; i++)
        {
            if (source[i] == '\n')
            {
                line++;
                character = 0;
            }
            else if (source[i] != '\r')
            {
                character++;
            }
        }

        return (line, character);
    }

    private static string FindRepoRoot()
    {
        var current = new DirectoryInfo(AppContext.BaseDirectory);
        while (current is not null)
        {
            var candidate = Path.Combine(current.FullName, "FScript.sln");
            if (File.Exists(candidate))
            {
                return current.FullName;
            }

            current = current.Parent;
        }

        throw new Exception("Unable to locate repository root from test base directory");
    }
}
