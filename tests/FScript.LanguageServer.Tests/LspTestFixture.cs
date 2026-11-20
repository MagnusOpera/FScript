using System.Text.Json.Nodes;
using NUnit.Framework;

namespace FScript.LanguageServer.Tests;

internal static class LspTestFixture
{
    public static void InitializeWith(LspClient.Client client, JsonObject? initializationOptions)
    {
        var initializeParams = new JsonObject
        {
            ["processId"] = null,
            ["rootUri"] = null,
            ["capabilities"] = new JsonObject()
        };
        if (initializationOptions is not null)
        {
            initializeParams["initializationOptions"] = initializationOptions;
        }

        LspClient.SendRequest(client, 1, "initialize", initializeParams);
        var response = LspClient.ReadUntil(client, 20_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 1);
        Assert.That(response["result"], Is.Not.Null);
        LspClient.SendNotification(client, "initialized", null);
    }

    // Broad editor feature tests explicitly request their historical full catalog.
    private static readonly Lazy<string> FullCatalogConfig = new(() => {
        var path = Path.Combine(Path.GetTempPath(), "fscript-lsp-catalog-" + Guid.NewGuid().ToString("N") + ".toml");
        File.WriteAllText(path, "[runtime]\npermissions = ['All']\n");
        AppDomain.CurrentDomain.ProcessExit += (_, _) => File.Delete(path);
        return path;
    });

    public static void Initialize(LspClient.Client client) => InitializeWith(client, new JsonObject {
        ["configurationPath"] = FullCatalogConfig.Value,
        ["workspaceTrusted"] = true
    });

    public static void Shutdown(LspClient.Client client)
    {
        LspClient.SendRequest(client, 2, "shutdown", null);
        _ = LspClient.ReadUntil(client, 10_000, msg => msg["id"] is JsonValue idv && idv.TryGetValue<int>(out var id) && id == 2);
        LspClient.SendNotification(client, "exit", null);
    }
}
