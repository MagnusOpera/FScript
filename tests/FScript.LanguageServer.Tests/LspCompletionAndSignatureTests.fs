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
type LspCompletionAndSignatureTests () =
    [<Test>]
    member _.``Inlay hints show map key union and unknown for unresolved map signature`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-map-unknown-test.fss"
            let source =
                "let remove k m =\n"
                + "    match m with\n"
                + "    | { [key] = _; ..rest } when key = k -> rest\n"
                + "    | _ -> m\n"

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

            let req = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            req["textDocument"] <- textDocument
            let range = JsonObject()
            let startPos = JsonObject()
            startPos["line"] <- JsonValue.Create(0)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(0)
            endPos["character"] <- JsonValue.Create(30)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 43 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 43 with _ -> false
                    | _ -> false)

            let labels =
                match resp["result"] with
                | :? JsonArray as hints ->
                    hints
                    |> Seq.choose (fun hint ->
                        match hint with
                        | :? JsonObject as h ->
                            match h["label"] with
                            | :? JsonValue as v ->
                                try Some (v.GetValue<string>()) with _ -> None
                            | _ -> None
                        | _ -> None)
                    |> Seq.toList
                | _ -> []

            labels |> should contain ": int|string"
            labels |> should contain ": unknown map"
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover shows signature for injected runtime extern function`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-injected-extern-test.fss"
            let source = "let ok = Fs.exists \".\"\nok"

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

            let hoverParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(13)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 70 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 70 with _ -> false
                    | _ -> false)

            let hoverValue =
                match hoverResp["result"] with
                | :? JsonObject as result ->
                    match result["contents"] with
                    | :? JsonObject as contents ->
                        match contents["value"] with
                        | :? JsonValue as value -> value.GetValue<string>()
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            let hasExpectedHover =
                hoverValue.Contains("Fs.exists", StringComparison.Ordinal)
                && hoverValue.Contains("string -> bool", StringComparison.Ordinal)
                && hoverValue.Contains("injected-function", StringComparison.Ordinal)

            Assert.That(hasExpectedHover, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion includes local bindings and filters by prefix`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/completion-test.fss"
            let source = "let alpha = 1\nlet beta = 2\nal"

            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(source)

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            // Ignore diagnostics publish for this valid document.
            LspClient.readUntil client 10000 (fun msg ->
                match msg["method"] with
                | :? JsonValue as mv ->
                    try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                | _ -> false)
            |> ignore

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(2)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 4 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 4 with _ -> false
                    | _ -> false)

            let hasAlpha =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.exists (fun item ->
                            match item with
                            | :? JsonObject as o ->
                                match o["label"] with
                                | :? JsonValue as v ->
                                    try v.GetValue<string>() = "alpha" with _ -> false
                                | _ -> false
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasAlpha |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion supports module-qualified stdlib symbols`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/module-completion-test.fss"
            let source = "List.ma"

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

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(7)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 5 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 5 with _ -> false
                    | _ -> false)

            let hasListMap =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.exists (fun item ->
                            match item with
                            | :? JsonObject as o ->
                                match o["label"] with
                                | :? JsonValue as v ->
                                    try v.GetValue<string>() = "List.map" with _ -> false
                                | _ -> false
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasListMap |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion proposes record fields after dot`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/record-field-completion-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet home = { City = \"Paris\"; Zip = 75000 }\nhome.City"

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

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(5)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 19 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 19 with _ -> false
                    | _ -> false)

            let labels =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.choose (fun item ->
                            match item with
                            | :? JsonObject as o ->
                                match o["label"] with
                                | :? JsonValue as v ->
                                    try Some (v.GetValue<string>()) with _ -> None
                                | _ -> None
                            | _ -> None)
                        |> Seq.toList
                    | _ -> []
                | _ -> []

            labels |> should contain "City"
            labels |> should contain "Zip"
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion filters record fields by dotted member prefix`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/record-field-prefix-completion-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet home = { City = \"Paris\"; Zip = 75000 }\nhome.City"

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

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(6)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 20 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 20 with _ -> false
                    | _ -> false)

            let labels =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.choose (fun item ->
                            match item with
                            | :? JsonObject as o ->
                                match o["label"] with
                                | :? JsonValue as v ->
                                    try Some (v.GetValue<string>()) with _ -> None
                                | _ -> None
                            | _ -> None)
                        |> Seq.toList
                    | _ -> []
                | _ -> []

            labels |> should equal [ "City" ]
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion proposes fields for annotated function parameters`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/record-param-completion-test.fss"
            let source =
                "type Address = { City: string; Zip: int }\nlet format (address: Address) =\n    address.City"

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

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(13)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 22 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 22 with _ -> false
                    | _ -> false)

            let labels =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.choose (fun item ->
                            match item with
                            | :? JsonObject as o ->
                                match o["label"] with
                                | :? JsonValue as v ->
                                    try Some (v.GetValue<string>()) with _ -> None
                                | _ -> None
                            | _ -> None)
                        |> Seq.toList
                    | _ -> []
                | _ -> []

            labels |> should equal [ "City" ]
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion ranks symbol matches before keywords for non-empty prefix`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/completion-ranking-test.fss"
            let source = "let alpha = 1\nlet alphabet = 2\nal"

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

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(2)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 24 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 24 with _ -> false
                    | _ -> false)

            let firstLabel =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items when items.Count > 0 ->
                        match items[0] with
                        | :? JsonObject as o ->
                            match o["label"] with
                            | :? JsonValue as v ->
                                try Some (v.GetValue<string>()) with _ -> None
                            | _ -> None
                        | _ -> None
                    | _ -> None
                | _ -> None

            firstLabel |> should equal (Some "alpha")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Completion marks exact symbol match as preselected and provides sort metadata`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/completion-preselect-test.fss"
            let source = "let alpha = 1\nalpha"

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

            let completionProbe = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(5)
            completionProbe["textDocument"] <- textDocument
            completionProbe["position"] <- position

            LspClient.sendRequest client 28 "textDocument/completion" (Some completionProbe)
            let completionResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 28 with _ -> false
                    | _ -> false)

            let alphaMeta =
                match completionResp["result"] with
                | :? JsonObject as result ->
                    match result["items"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.tryPick (fun item ->
                            match item with
                            | :? JsonObject as o ->
                                match o["label"] with
                                | :? JsonValue as labelV when (try labelV.GetValue<string>() = "alpha" with _ -> false) ->
                                    let preselected =
                                        match o["preselect"] with
                                        | :? JsonValue as pv -> (try Some (pv.GetValue<bool>()) with _ -> None)
                                        | _ -> None
                                    let hasSortText =
                                        match o["sortText"] with
                                        | :? JsonValue as sv -> (try not (String.IsNullOrWhiteSpace(sv.GetValue<string>())) with _ -> false)
                                        | _ -> false
                                    Some (preselected, hasSortText)
                                | _ -> None
                            | _ -> None)
                    | _ -> None
                | _ -> None

            alphaMeta |> should equal (Some (Some true, true))
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Signature help returns function signature for call target`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/signature-help-test.fss"
            let source = "let add x y = x + y\nadd(1, 2)"

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

            let sigParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(4)
            sigParams["textDocument"] <- textDocument
            sigParams["position"] <- position
            let ctx = JsonObject()
            ctx["triggerCharacter"] <- JsonValue.Create("(")
            sigParams["context"] <- ctx

            LspClient.sendRequest client 7 "textDocument/signatureHelp" (Some sigParams)
            let sigResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 7 with _ -> false
                    | _ -> false)

            let hasAddSignature =
                match sigResp["result"] with
                | :? JsonObject as result ->
                    match result["signatures"] with
                    | :? JsonArray as signatures ->
                        signatures
                        |> Seq.exists (fun sigNode ->
                            match sigNode with
                            | :? JsonObject as sigObj ->
                                match sigObj["label"] with
                                | :? JsonValue as label ->
                                    try label.GetValue<string>().StartsWith("add", StringComparison.Ordinal) with _ -> false
                                | _ -> false
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasAddSignature |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Signature help sets active parameter index from cursor position`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/signature-help-active-parameter-test.fss"
            let source = "let add3 x y z = x + y + z\nadd3(1, 2, 3)"

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

            let sigParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(11)
            sigParams["textDocument"] <- textDocument
            sigParams["position"] <- position
            let ctx = JsonObject()
            ctx["triggerCharacter"] <- JsonValue.Create(",")
            sigParams["context"] <- ctx

            LspClient.sendRequest client 25 "textDocument/signatureHelp" (Some sigParams)
            let sigResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 25 with _ -> false
                    | _ -> false)

            let activeParameter =
                match sigResp["result"] with
                | :? JsonObject as result ->
                    match result["activeParameter"] with
                    | :? JsonValue as v ->
                        try Some (v.GetValue<int>()) with _ -> None
                    | _ -> None
                | _ -> None

            activeParameter |> should equal (Some 2)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover returns markdown signature and kind`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-test.fss"
            let source = "let double x = x * 2\nlet result = double 21"

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

            let hoverParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(14)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 9 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 9 with _ -> false
                    | _ -> false)

            let hasExpectedHoverValue =
                match hoverResp["result"] with
                | :? JsonObject as result ->
                    match result["contents"] with
                    | :? JsonObject as contents ->
                        match contents["value"] with
                        | :? JsonValue as value ->
                            let text = value.GetValue<string>()
                            text.Contains("double", StringComparison.Ordinal)
                            && text.Contains("function", StringComparison.Ordinal)
                            && text.Contains("defined at L", StringComparison.Ordinal)
                        | _ -> false
                    | _ -> false
                | _ -> false

            hasExpectedHoverValue |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover shows typed signature for inferable function even when another binding has a type error`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-best-effort-type-signature-test.fss"
            let source =
                "type ActionContext = { Name: string }\n"
                + "type ShellOperation = | Command of string\n"
                + "let command_op command = Command command\n"
                + "let bad = 1 + true\n"
                + "let tool (context: ActionContext) (args: string option) =\n"
                + "    [command_op \"x\"]\n"
                + "tool\n"

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

            let hoverParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(6)
            position["character"] <- JsonValue.Create(2)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 49 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 49 with _ -> false
                    | _ -> false)

            let hoverValue =
                match hoverResp["result"] with
                | :? JsonObject as result ->
                    match result["contents"] with
                    | :? JsonObject as contents ->
                        match contents["value"] with
                        | :? JsonValue as value -> value.GetValue<string>()
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            let hasExpectedHoverValue =
                hoverValue.Contains("tool: (context: ActionContext) -> (args: string option) -> ShellOperation list", StringComparison.Ordinal)

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover shows typed signature for include-based script functions`` () =
        let client = LspClient.start ()
        try
            initialize client

            let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-lsp-hover-include-{Guid.NewGuid():N}")
            Directory.CreateDirectory(tempRoot) |> ignore
            let includeFile = Path.Combine(tempRoot, "_protocol.fss")
            let mainFile = Path.Combine(tempRoot, "main.fss")
            File.WriteAllText(includeFile, "type ActionContext = { Name: string }\ntype ProjectInfo = { Ok: bool }\n")
            File.WriteAllText(mainFile, "import \"_protocol.fss\"\n[<export>]\nlet defaults (context: ActionContext) =\n    { Ok = true }\n")

            let uri = Uri(mainFile).AbsoluteUri
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(File.ReadAllText(mainFile))

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            LspClient.readUntil client 10000 (fun msg ->
                match msg["method"] with
                | :? JsonValue as mv ->
                    try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                | _ -> false)
            |> ignore

            let hoverParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(8)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 46 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 46 with _ -> false
                    | _ -> false)

            let hoverValue =
                match hoverResp["result"] with
                | :? JsonObject as result ->
                    match result["contents"] with
                    | :? JsonObject as contents ->
                        match contents["value"] with
                        | :? JsonValue as value ->
                            value.GetValue<string>()
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            let hasExpectedHoverValue =
                let text = hoverValue
                match hoverResp["result"] with
                | :? JsonObject -> text.Contains("defaults: (context: ActionContext) -> ProjectInfo", StringComparison.Ordinal)
                | _ -> false

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
