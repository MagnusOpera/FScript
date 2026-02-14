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

    [<Test>]
    member _.``CSharp server didOpen publishes parse diagnostics`` () =
        let client = LspClient.startCSharp ()
        try
            initialize client

            let uri = "file:///tmp/csharp-diagnostics-test.fss"
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create("let x =")

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv when (try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false) ->
                        match msg["params"] with
                        | :? JsonObject as p ->
                            match p["uri"], p["diagnostics"] with
                            | :? JsonValue as u, (:? JsonArray as diagnosticsArray) ->
                                (try u.GetValue<string>() = uri with _ -> false) && (diagnosticsArray.Count > 0)
                            | _ -> false
                        | _ -> false
                    | _ -> false)

            let hasParseCode =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as diagnostics ->
                        diagnostics
                        |> Seq.exists (fun diag ->
                            match diag with
                            | :? JsonObject as d ->
                                match d["code"] with
                                | :? JsonValue as codeValue ->
                                    try codeValue.GetValue<string>() = "parse" with _ -> false
                                | _ -> false
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasParseCode |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client

    [<Test>]
    member _.``CSharp server viewAst returns program JSON`` () =
        let client = LspClient.startCSharp ()
        try
            initialize client

            let uri = "file:///tmp/csharp-view-ast-test.fss"
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

            LspClient.sendRequest client 45 "fscript/viewAst" (Some requestParams)
            let response =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 45 with _ -> false
                    | _ -> false)

            let kindValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonObject as data ->
                        match data["kind"] with
                        | :? JsonValue as value ->
                            try value.GetValue<string>() with _ -> ""
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            kindValue |> should equal "program"
        finally
            try shutdown client with _ -> ()
            LspClient.stop client

    [<Test>]
    member _.``CSharp server viewInferredAst returns typed program JSON`` () =
        let client = LspClient.startCSharp ()
        try
            initialize client

            let uri = "file:///tmp/csharp-view-inferred-test.fss"
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

            LspClient.sendRequest client 46 "fscript/viewInferredAst" (Some requestParams)
            let response =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 46 with _ -> false
                    | _ -> false)

            let kindValue =
                match response["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonObject as data ->
                        match data["kind"] with
                        | :? JsonValue as value ->
                            try value.GetValue<string>() with _ -> ""
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            kindValue |> should equal "typedProgram"
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
