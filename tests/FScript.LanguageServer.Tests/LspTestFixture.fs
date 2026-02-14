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
module internal LspTestFixture =
    let initializeWith (client: LspClient.Client) (initializationOptions: JsonObject option) =
        let initializeParams = JsonObject()
        initializeParams["processId"] <- JsonValue.Create<int option>(None)
        initializeParams["rootUri"] <- JsonValue.Create<string option>(None)
        initializeParams["capabilities"] <- JsonObject()
        match initializationOptions with
        | Some options -> initializeParams["initializationOptions"] <- options
        | None -> ()

        LspClient.sendRequest client 1 "initialize" (Some initializeParams)

        let response =
            LspClient.readUntil client 20000 (fun msg ->
                match msg["id"] with
                | :? JsonValue as idv ->
                    try idv.GetValue<int>() = 1 with _ -> false
                | _ -> false)

        response["result"] |> should not' (equal null)
        LspClient.sendNotification client "initialized" None

    let initialize (client: LspClient.Client) =
        initializeWith client None

    let shutdown (client: LspClient.Client) =
        LspClient.sendRequest client 2 "shutdown" None
        LspClient.readUntil client 10000 (fun msg ->
            match msg["id"] with
            | :? JsonValue as idv ->
                try idv.GetValue<int>() = 2 with _ -> false
            | _ -> false)
        |> ignore

        LspClient.sendNotification client "exit" None
