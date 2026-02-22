using System.Text.Json.Nodes;
using Microsoft.FSharp.Core;
using FSLspHandlers = FScript.LanguageServer.LspHandlers;
using FSLspProtocol = FScript.LanguageServer.LspProtocol;

namespace FScript.LanguageServer.CSharp;

internal sealed class LspServer
{
    private bool _shutdownRequested;

    public void Run()
    {
        var input = Console.OpenStandardInput();

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
            var idNode = message["id"];
            var paramsObj = message["params"] as JsonObject;

            if (string.IsNullOrWhiteSpace(method))
            {
                continue;
            }

            try
            {
                if (idNode is null)
                {
                    HandleNotification(method!, paramsObj);
                    if (_shutdownRequested && string.Equals(method, "exit", StringComparison.Ordinal))
                    {
                        break;
                    }
                }
                else
                {
                    HandleRequest(idNode, method!, paramsObj);
                }
            }
            catch (Exception ex)
            {
                var payload = new JsonObject
                {
                    ["type"] = 1,
                    ["message"] = $"FScript LSP internal error (C# host): {ex.Message}"
                };
                FSLspProtocol.sendNotification("window/logMessage", FSharpOption<JsonNode?>.Some(payload));
            }
        }
    }

    private static void HandleNotification(string method, JsonObject? paramsObj)
    {
        switch (method)
        {
            case "initialized":
                break;
            case "textDocument/didOpen":
                if (paramsObj is not null)
                {
                    FSLspHandlers.handleDidOpen(paramsObj);
                }
                break;
            case "textDocument/didChange":
                if (paramsObj is not null)
                {
                    FSLspHandlers.handleDidChange(paramsObj);
                }
                break;
            case "textDocument/didClose":
                if (paramsObj is not null)
                {
                    FSLspHandlers.handleDidClose(paramsObj);
                }
                break;
            case "exit":
                break;
        }
    }

    private void HandleRequest(JsonNode idNode, string method, JsonObject? paramsObj)
    {
        switch (method)
        {
            case "initialize":
                FSLspHandlers.handleInitialize(idNode, paramsObj is null ? FSharpOption<JsonObject>.None : FSharpOption<JsonObject>.Some(paramsObj));
                break;
            case "shutdown":
                _shutdownRequested = true;
                FSLspProtocol.sendResponse(idNode, FSharpOption<JsonNode?>.None);
                break;
            case "textDocument/completion":
                if (paramsObj is not null) FSLspHandlers.handleCompletion(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/semanticTokens/full":
                if (paramsObj is not null) FSLspHandlers.handleSemanticTokens(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/hover":
                if (paramsObj is not null) FSLspHandlers.handleHover(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/definition":
                if (paramsObj is not null) FSLspHandlers.handleDefinition(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/typeDefinition":
                if (paramsObj is not null) FSLspHandlers.handleTypeDefinition(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/documentSymbol":
                if (paramsObj is not null) FSLspHandlers.handleDocumentSymbol(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/references":
                if (paramsObj is not null) FSLspHandlers.handleReferences(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/documentHighlight":
                if (paramsObj is not null) FSLspHandlers.handleDocumentHighlight(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/signatureHelp":
                if (paramsObj is not null) FSLspHandlers.handleSignatureHelp(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/rename":
                if (paramsObj is not null) FSLspHandlers.handleRename(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/prepareRename":
                if (paramsObj is not null) FSLspHandlers.handlePrepareRename(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "workspace/symbol":
                if (paramsObj is not null) FSLspHandlers.handleWorkspaceSymbol(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/codeAction":
                if (paramsObj is not null) FSLspHandlers.handleCodeAction(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "textDocument/inlayHint":
                if (paramsObj is not null) FSLspHandlers.handleInlayHints(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "fscript/viewAst":
                if (paramsObj is not null) FSLspHandlers.handleViewAst(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            case "fscript/viewInferredAst":
                if (paramsObj is not null) FSLspHandlers.handleViewInferredAst(idNode, paramsObj); else SendInvalidParams(idNode);
                break;
            default:
                FSLspProtocol.sendError(idNode, -32601, "Method not found");
                break;
        }
    }

    private static void SendInvalidParams(JsonNode idNode)
    {
        FSLspProtocol.sendError(idNode, -32602, "Invalid params");
    }
}
