using System.Text.Json.Nodes;

namespace FScript.LanguageServer.CSharp;

internal sealed class LspServer
{
    private readonly Dictionary<string, string> _documents = new(StringComparer.Ordinal);
    private bool _shutdownRequested;

    public void Run()
    {
        var input = Console.OpenStandardInput();
        var output = Console.OpenStandardOutput();

        while (true)
        {
            var raw = JsonRpcWire.ReadMessage(input);
            if (raw is null)
            {
                break;
            }

            JsonObject? message;
            try
            {
                message = JsonNode.Parse(raw) as JsonObject;
            }
            catch
            {
                continue;
            }

            if (message is null)
            {
                continue;
            }

            var method = message["method"]?.GetValue<string>();
            var id = message["id"];
            var @params = message["params"] as JsonObject;

            if (method is null)
            {
                continue;
            }

            if (id is null)
            {
                HandleNotification(method, @params);
                if (_shutdownRequested && string.Equals(method, "exit", StringComparison.Ordinal))
                {
                    break;
                }

                continue;
            }

            HandleRequest(output, id, method, @params);
        }
    }

    private void HandleNotification(string method, JsonObject? @params)
    {
        switch (method)
        {
            case "textDocument/didOpen":
                HandleDidOpen(@params);
                break;
            case "textDocument/didChange":
                HandleDidChange(@params);
                break;
            case "textDocument/didClose":
                HandleDidClose(@params);
                break;
            case "exit":
                break;
        }
    }

    private void HandleRequest(Stream output, JsonNode id, string method, JsonObject? @params)
    {
        switch (method)
        {
            case "initialize":
                SendResponse(output, id, LspHandlers.CreateInitializeResult());
                break;
            case "shutdown":
                _shutdownRequested = true;
                SendResponse(output, id, null);
                break;
            case "textDocument/hover":
                SendResponse(output, id, null);
                break;
            case "fscript/stdlibSource":
                SendResponse(output, id, LspHandlers.HandleStdlibSource(@params));
                break;
            default:
                SendError(output, id, -32601, $"Method not found: {method}");
                break;
        }
    }

    private void HandleDidOpen(JsonObject? @params)
    {
        var textDocument = @params?["textDocument"] as JsonObject;
        var uri = textDocument?["uri"]?.GetValue<string>();
        var text = textDocument?["text"]?.GetValue<string>();
        if (!string.IsNullOrEmpty(uri) && text is not null)
        {
            _documents[uri] = text;
        }
    }

    private void HandleDidChange(JsonObject? @params)
    {
        var textDocument = @params?["textDocument"] as JsonObject;
        var uri = textDocument?["uri"]?.GetValue<string>();
        if (string.IsNullOrEmpty(uri))
        {
            return;
        }

        var changes = @params?["contentChanges"] as JsonArray;
        if (changes is null || changes.Count == 0)
        {
            return;
        }

        var last = changes[changes.Count - 1] as JsonObject;
        var text = last?["text"]?.GetValue<string>();
        if (text is not null)
        {
            _documents[uri] = text;
        }
    }

    private void HandleDidClose(JsonObject? @params)
    {
        var textDocument = @params?["textDocument"] as JsonObject;
        var uri = textDocument?["uri"]?.GetValue<string>();
        if (!string.IsNullOrEmpty(uri))
        {
            _documents.Remove(uri);
        }
    }

    private static void SendResponse(Stream output, JsonNode id, JsonNode? result)
    {
        var payload = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["id"] = id.DeepClone(),
            ["result"] = result
        };

        JsonRpcWire.WriteMessage(output, payload.ToJsonString());
    }

    private static void SendError(Stream output, JsonNode id, int code, string message)
    {
        var payload = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["id"] = id.DeepClone(),
            ["error"] = new JsonObject
            {
                ["code"] = code,
                ["message"] = message
            }
        };

        JsonRpcWire.WriteMessage(output, payload.ToJsonString());
    }
}
