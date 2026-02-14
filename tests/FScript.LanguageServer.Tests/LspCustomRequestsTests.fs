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
type LspCustomRequestsTests () =
    [<Test>]
    member _.``Custom request viewAst returns parsed program as JSON`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/view-ast-test.fss"
            let source = "let value = 42\nvalue\n"

            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(source)

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)
            LspClient.readUntil client 10000 (fun msg ->
                match msg["method"] with
                | :? JsonValue as mv ->
                    try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                | _ -> false)
            |> ignore

            let requestParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            requestParams["textDocument"] <- textDocument

            LspClient.sendRequest client 60 "fscript/viewAst" (Some requestParams)
            let response =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 60 with _ -> false
                    | _ -> false)

            let okValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["ok"] with
                    | :? JsonValue as okNode ->
                        try okNode.GetValue<bool>() with _ -> false
                    | _ -> false
                | _ -> false

            let kindValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonObject as data ->
                        match data["kind"] with
                        | :? JsonValue as k ->
                            try k.GetValue<string>() with _ -> ""
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            Assert.That(okValue, Is.True)
            Assert.That(kindValue, Is.EqualTo("program"))
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Custom request viewInferredAst returns typed program as JSON`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/view-inferred-ast-test.fss"
            let source = "let inc x = x + 1\ninc 1\n"

            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(source)

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)
            LspClient.readUntil client 10000 (fun msg ->
                match msg["method"] with
                | :? JsonValue as mv ->
                    try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                | _ -> false)
            |> ignore

            let requestParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            requestParams["textDocument"] <- textDocument

            LspClient.sendRequest client 61 "fscript/viewInferredAst" (Some requestParams)
            let response =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 61 with _ -> false
                    | _ -> false)

            let okValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["ok"] with
                    | :? JsonValue as okNode ->
                        try okNode.GetValue<bool>() with _ -> false
                    | _ -> false
                | _ -> false

            let kindValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonObject as data ->
                        match data["kind"] with
                        | :? JsonValue as k ->
                            try k.GetValue<string>() with _ -> ""
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            Assert.That(okValue, Is.True)
            Assert.That(kindValue, Is.EqualTo("typedProgram"))
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Custom request stdlibSource returns embedded source text`` () =
        let client = LspClient.start ()
        try
            initialize client

            let requestParams = JsonObject()
            requestParams["uri"] <- JsonValue.Create("fscript-stdlib:///Option.fss")

            LspClient.sendRequest client 611 "fscript/stdlibSource" (Some requestParams)
            let response =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 611 with _ -> false
                    | _ -> false)

            let okValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["ok"] with
                    | :? JsonValue as okNode ->
                        try okNode.GetValue<bool>() with _ -> false
                    | _ -> false
                | _ -> false

            let sourceText =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonObject as data ->
                        match data["text"] with
                        | :? JsonValue as textNode ->
                            try textNode.GetValue<string>() with _ -> ""
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            Assert.That(okValue, Is.True)
            Assert.That(sourceText.Contains("let map mapper value", StringComparison.Ordinal), Is.True)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
