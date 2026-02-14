using System.Text.Json.Nodes;
using FScript.CSharpInterop;

namespace FScript.LanguageServer.CSharp;

internal static class LspHandlers
{
    internal static JsonObject CreateInitializeResult()
    {
        var sync = new JsonObject
        {
            ["openClose"] = true,
            ["change"] = 1
        };

        var capabilities = new JsonObject
        {
            ["textDocumentSync"] = sync,
            ["hoverProvider"] = true
        };

        return new JsonObject
        {
            ["capabilities"] = capabilities,
            ["serverInfo"] = new JsonObject
            {
                ["name"] = "FScript Language Server (C#)"
            }
        };
    }

    internal static JsonObject HandleStdlibSource(JsonObject? @params)
    {
        var uri = @params?["uri"]?.GetValue<string>();
        if (string.IsNullOrWhiteSpace(uri))
        {
            return Error("internal", "Missing stdlib URI.");
        }

        var textOption = InteropServices.tryLoadStdlibSourceText(uri);
        if (textOption is null)
        {
            return Error("internal", $"Unable to load stdlib source for '{uri}'.");
        }

        var text = textOption.Value;
        return new JsonObject
        {
            ["ok"] = true,
            ["data"] = new JsonObject
            {
                ["uri"] = uri,
                ["text"] = text,
                ["languageId"] = "fscript"
            }
        };
    }

    private static JsonObject Error(string kind, string message)
    {
        return new JsonObject
        {
            ["ok"] = false,
            ["error"] = new JsonObject
            {
                ["kind"] = kind,
                ["message"] = message
            }
        };
    }
}
