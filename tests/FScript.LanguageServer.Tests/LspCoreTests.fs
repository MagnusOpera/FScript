namespace FScript.LanguageServer.Tests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Diagnostics
open System.Threading
open NUnit.Framework
open FsUnit
open LspTestFixture

[<TestFixture>]
type LspCoreTests () =
    [<Test>]
    member _.``Initialize returns capabilities`` () =
        let client = LspClient.start ()
        try
            initialize client

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create("file:///tmp/test.fss")
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(0)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position
            LspClient.sendRequest client 3 "textDocument/completion" (Some completionProbe)

            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 3 with _ -> false
                    | _ -> false)

            completionResp["result"] |> should not' (equal null)

            let inlayReq = JsonObject()
            let textDocument2 = JsonObject()
            textDocument2["uri"] <- JsonValue.Create("file:///tmp/test.fss")
            inlayReq["textDocument"] <- textDocument2
            let range = JsonObject()
            let startPos = JsonObject()
            startPos["line"] <- JsonValue.Create(0)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(0)
            endPos["character"] <- JsonValue.Create(10)
            range["start"] <- startPos
            range["end"] <- endPos
            inlayReq["range"] <- range
            LspClient.sendRequest client 31 "textDocument/inlayHint" (Some inlayReq)
            let inlayResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 31 with _ -> false
                    | _ -> false)

            inlayResp["result"] |> should not' (equal null)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Custom AST requests reject non-file URIs`` () =
        let client = LspClient.start ()
        try
            initialize client

            let requestParams = JsonObject()
            requestParams["uri"] <- JsonValue.Create("untitled:ast-test")

            LspClient.sendRequest client 62 "fscript/viewAst" (Some requestParams)
            let response =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 62 with _ -> false
                    | _ -> false)

            let okValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["ok"] with
                    | :? JsonValue as okNode ->
                        try okNode.GetValue<bool>() with _ -> true
                    | _ -> true
                | _ -> true

            let message =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["error"] with
                    | :? JsonObject as error ->
                        match error["message"] with
                        | :? JsonValue as mv ->
                            try mv.GetValue<string>() with _ -> ""
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            Assert.That(okValue, Is.False)
            Assert.That(message.Contains("file-based scripts only", StringComparison.Ordinal), Is.True)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
