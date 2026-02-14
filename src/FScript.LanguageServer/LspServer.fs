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
                            let methodName = tryGetString root "method"
                            let idNode = tryGetNode root "id"
                            let paramsObj = tryGetObject root "params"

                            match methodName, idNode, paramsObj with
                            | Some "initialize", Some idNode, paramsObj ->
                                LspHandlers.handleInitialize idNode paramsObj
                            | Some "initialized", _, _ ->
                                ()
                            | Some "shutdown", Some idNode, _ ->
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
                            | Some "textDocument/completion", Some idNode, Some paramsObj ->
                                LspHandlers.handleCompletion idNode paramsObj
                            | Some "textDocument/semanticTokens/full", Some idNode, Some paramsObj ->
                                LspHandlers.handleSemanticTokens idNode paramsObj
                            | Some "textDocument/hover", Some idNode, Some paramsObj ->
                                LspHandlers.handleHover idNode paramsObj
                            | Some "textDocument/definition", Some idNode, Some paramsObj ->
                                LspHandlers.handleDefinition idNode paramsObj
                            | Some "textDocument/typeDefinition", Some idNode, Some paramsObj ->
                                LspHandlers.handleTypeDefinition idNode paramsObj
                            | Some "textDocument/documentSymbol", Some idNode, Some paramsObj ->
                                LspHandlers.handleDocumentSymbol idNode paramsObj
                            | Some "textDocument/references", Some idNode, Some paramsObj ->
                                LspHandlers.handleReferences idNode paramsObj
                            | Some "textDocument/documentHighlight", Some idNode, Some paramsObj ->
                                LspHandlers.handleDocumentHighlight idNode paramsObj
                            | Some "textDocument/signatureHelp", Some idNode, Some paramsObj ->
                                LspHandlers.handleSignatureHelp idNode paramsObj
                            | Some "textDocument/rename", Some idNode, Some paramsObj ->
                                LspHandlers.handleRename idNode paramsObj
                            | Some "textDocument/prepareRename", Some idNode, Some paramsObj ->
                                LspHandlers.handlePrepareRename idNode paramsObj
                            | Some "workspace/symbol", Some idNode, Some paramsObj ->
                                LspHandlers.handleWorkspaceSymbol idNode paramsObj
                            | Some "textDocument/codeAction", Some idNode, Some paramsObj ->
                                LspHandlers.handleCodeAction idNode paramsObj
                            | Some "textDocument/inlayHint", Some idNode, Some paramsObj ->
                                LspHandlers.handleInlayHints idNode paramsObj
                            | Some "fscript/viewAst", Some idNode, Some paramsObj ->
                                LspHandlers.handleViewAst idNode paramsObj
                            | Some "fscript/viewInferredAst", Some idNode, Some paramsObj ->
                                LspHandlers.handleViewInferredAst idNode paramsObj
                            | Some "fscript/stdlibSource", Some idNode, Some paramsObj ->
                                LspHandlers.handleStdlibSource idNode paramsObj
                            | Some _, Some idNode, _ ->
                                LspProtocol.sendError idNode -32601 "Method not found"
                            | _ -> ()
                with ex ->
                    // Never crash the server loop on malformed input.
                    let p = JsonObject()
                    p["type"] <- JsonValue.Create(1)
                    p["message"] <- JsonValue.Create($"FScript LSP internal error: {ex.Message}")
                    LspProtocol.sendNotification "window/logMessage" (Some p)
