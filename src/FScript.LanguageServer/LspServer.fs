namespace FScript.LanguageServer

open System
open System.Text.Json.Nodes

module LspServer =
    open LspModel

    let run () =
        let mutable keepRunning = true
        let mutable shutdownReceived = false

        while keepRunning do
            match LspProtocol.tryReadMessage () with
            | None -> keepRunning <- false
            | Some payload ->
                try
                    match JsonNode.Parse(payload) with
                    | null -> ()
                    | rootNode ->
                        match asObject rootNode with
                        | None -> ()
                        | Some root ->
                            match tryGetString root "method", root["id"], tryGetObject root "params" with
                            | Some "initialize", idNode, paramsObj when not (isNull idNode) ->
                                LspHandlers.handleInitialize idNode paramsObj
                            | Some "initialized", _, _ ->
                                ()
                            | Some "shutdown", idNode, _ when not (isNull idNode) ->
                                shutdownReceived <- true
                                LspProtocol.sendResponse idNode None
                            | Some "exit", _, _ ->
                                keepRunning <- false
                                if shutdownReceived then
                                    Environment.ExitCode <- 0
                                else
                                    Environment.ExitCode <- 1
                            | Some "textDocument/didOpen", _, Some paramsObj ->
                                LspHandlers.handleDidOpen paramsObj
                            | Some "textDocument/didChange", _, Some paramsObj ->
                                LspHandlers.handleDidChange paramsObj
                            | Some "textDocument/didClose", _, Some paramsObj ->
                                LspHandlers.handleDidClose paramsObj
                            | Some "textDocument/completion", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleCompletion idNode paramsObj
                            | Some "textDocument/semanticTokens/full", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleSemanticTokens idNode paramsObj
                            | Some "textDocument/hover", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleHover idNode paramsObj
                            | Some "textDocument/definition", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleDefinition idNode paramsObj
                            | Some "textDocument/typeDefinition", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleTypeDefinition idNode paramsObj
                            | Some "textDocument/documentSymbol", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleDocumentSymbol idNode paramsObj
                            | Some "textDocument/references", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleReferences idNode paramsObj
                            | Some "textDocument/documentHighlight", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleDocumentHighlight idNode paramsObj
                            | Some "textDocument/signatureHelp", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleSignatureHelp idNode paramsObj
                            | Some "textDocument/rename", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleRename idNode paramsObj
                            | Some "textDocument/prepareRename", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handlePrepareRename idNode paramsObj
                            | Some "workspace/symbol", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleWorkspaceSymbol idNode paramsObj
                            | Some "textDocument/codeAction", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleCodeAction idNode paramsObj
                            | Some "textDocument/inlayHint", idNode, Some paramsObj when not (isNull idNode) ->
                                LspHandlers.handleInlayHints idNode paramsObj
                            | Some _, idNode, _ when not (isNull idNode) ->
                                LspProtocol.sendError idNode -32601 "Method not found"
                            | _ -> ()
                with ex ->
                    // Never crash the server loop on malformed input.
                    let p = JsonObject()
                    p["type"] <- JsonValue.Create(1)
                    p["message"] <- JsonValue.Create($"FScript LSP internal error: {ex.Message}")
                    LspProtocol.sendNotification "window/logMessage" (Some p)
