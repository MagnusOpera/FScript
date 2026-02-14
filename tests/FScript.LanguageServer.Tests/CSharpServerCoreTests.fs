namespace FScript.LanguageServer.Tests

open System
open System.Text.Json.Nodes
open NUnit.Framework
open FsUnit
open LspTestFixture

[<TestFixture>]
type CSharpServerCoreTests () =
    [<Test>]
    member _.``CSharp server initialize returns capabilities`` () =
        let client = LspClient.startCSharp ()
        try
            initialize client

            let hoverReq = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create("file:///tmp/test.fss")
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(0)
            hoverReq["textDocument"] <- textDocument
            hoverReq["position"] <- position

            LspClient.sendRequest client 42 "textDocument/hover" (Some hoverReq)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 42 with _ -> false
                    | _ -> false)

            hoverResp["result"] |> should equal null
        finally
            try shutdown client with _ -> ()
            LspClient.stop client

    [<Test>]
    member _.``CSharp server returns stdlib source`` () =
        let client = LspClient.startCSharp ()
        try
            initialize client

            let requestParams = JsonObject()
            requestParams["uri"] <- JsonValue.Create("fscript-stdlib:///Option.fss")

            LspClient.sendRequest client 43 "fscript/stdlibSource" (Some requestParams)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 43 with _ -> false
                    | _ -> false)

            let result =
                match resp["result"] with
                | :? JsonObject as obj -> obj
                | _ -> failwith "Expected result object"

            let ok =
                match result["ok"] with
                | :? JsonValue as value -> value.GetValue<bool>()
                | _ -> false
            ok |> should equal true

            let data =
                match result["data"] with
                | :? JsonObject as obj -> obj
                | _ -> failwith "Expected data object"

            let text =
                match data["text"] with
                | :? JsonValue as value -> value.GetValue<string>()
                | _ -> ""
            text.Contains("let", StringComparison.Ordinal) |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client

    [<Test>]
    member _.``CSharp server returns method not found for unknown request`` () =
        let client = LspClient.startCSharp ()
        try
            initialize client

            LspClient.sendRequest client 44 "fscript/unknown" None
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 44 with _ -> false
                    | _ -> false)

            let err =
                match resp["error"] with
                | :? JsonObject as obj -> obj
                | _ -> failwith "Expected error object"
            let code =
                match err["code"] with
                | :? JsonValue as value -> value.GetValue<int>()
                | _ -> 0
            code |> should equal -32601
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
