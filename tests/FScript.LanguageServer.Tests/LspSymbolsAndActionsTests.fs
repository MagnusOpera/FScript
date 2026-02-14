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
type LspSymbolsAndActionsTests () =
    [<Test>]
    member _.``Semantic tokens full returns token data`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/semantic-tokens-test.fss"
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create("type User = { Name: string }\nlet user = { Name = \"Ada\" }\nlet x = 42")

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

            LspClient.sendRequest client 17 "textDocument/semanticTokens/full" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 17 with _ -> false
                    | _ -> false)

            let hasTokenData =
                match resp["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonArray as items ->
                        (items |> Seq.length) >= 5
                    | _ -> false
                | _ -> false

            hasTokenData |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Semantic tokens classify attribute keyword and module-qualified function`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/semantic-tokens-classification-test.fss"
            let source = "[<export>]\nlet values = List.map (fun n -> n + 1) [1]"
            let lines = source.Split('\n')

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

            LspClient.sendRequest client 29 "textDocument/semanticTokens/full" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 29 with _ -> false
                    | _ -> false)

            let decodedTokens =
                let mutable previousLine = 0
                let mutable previousStart = 0
                match resp["result"] with
                | :? JsonObject as result ->
                    match result["data"] with
                    | :? JsonArray as data ->
                        data
                        |> Seq.toArray
                        |> Array.chunkBySize 5
                        |> Array.choose (fun chunk ->
                            if chunk.Length <> 5 then None else
                            let readInt (n: JsonNode | null) =
                                match n with
                                | null -> None
                                | :? JsonValue as v -> (try Some (v.GetValue<int>()) with _ -> None)
                                | _ -> None

                            match readInt chunk[0], readInt chunk[1], readInt chunk[2], readInt chunk[3] with
                            | Some deltaLine, Some deltaStart, Some length, Some tokenType ->
                                let line = previousLine + deltaLine
                                let start = if deltaLine = 0 then previousStart + deltaStart else deltaStart
                                previousLine <- line
                                previousStart <- start
                                let text =
                                    if line >= 0 && line < lines.Length && start >= 0 && start + length <= lines[line].Length then
                                        lines[line].Substring(start, length)
                                    else
                                        ""
                                Some (line, start, text, tokenType)
                            | _ -> None)
                    | _ -> Array.empty
                | _ -> Array.empty

            let hasExportKeyword =
                decodedTokens
                |> Array.exists (fun (_, _, text, tokenType) -> text = "export" && tokenType = 0)
            let hasListMapFunction =
                decodedTokens
                |> Array.exists (fun (_, _, text, tokenType) -> text = "List.map" && tokenType = 3)

            hasExportKeyword |> should equal true
            hasListMapFunction |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``DidOpen publishes diagnostics on parse error`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/diagnostic-test.fss"
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
                            | :? JsonValue as u, diagnosticsNode ->
                                match diagnosticsNode with
                                | :? JsonArray as diagnosticsArray ->
                                    (try u.GetValue<string>() = uri with _ -> false)
                                    && ((diagnosticsArray |> Seq.length) > 0)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    | _ -> false)

            diagMsg |> should not' (equal null)

            let hasExpectedDiagnosticMetadata =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as diagnostics ->
                        diagnostics
                        |> Seq.exists (fun diag ->
                            match diag with
                            | :? JsonObject as d ->
                                let sourceOk =
                                    match d["source"] with
                                    | :? JsonValue as v -> (try v.GetValue<string>() = "fscript-lsp" with _ -> false)
                                    | _ -> false
                                let codeOk =
                                    match d["code"] with
                                    | :? JsonValue as v -> (try v.GetValue<string>() = "parse" with _ -> false)
                                    | _ -> false
                                sourceOk && codeOk
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasExpectedDiagnosticMetadata |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``DidOpen does not publish unused binding warning`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/unused-binding-test.fss"
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create("let unused_value = 1\nlet used_value = 2\nused_value")

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diag =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv ->
                        try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                    | _ -> false)

            let hasUnusedWarning =
                match diag["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.exists (fun item ->
                            match item with
                            | :? JsonObject as d ->
                                let codeOk =
                                    match d["code"] with
                                    | :? JsonValue as cv -> (try cv.GetValue<string>() = "unused" with _ -> false)
                                    | _ -> false
                                let severityOk =
                                    match d["severity"] with
                                    | :? JsonValue as sv -> (try sv.GetValue<int>() = 2 with _ -> false)
                                    | _ -> false
                                codeOk && severityOk
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasUnusedWarning |> should equal false
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``DidOpen does not publish unused warning for exported binding`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/unused-exported-test.fss"
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create("[<export>]\nlet defaults = 42")

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv when (try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false) ->
                        match msg["params"] with
                        | :? JsonObject as p ->
                            match p["uri"] with
                            | :? JsonValue as u ->
                                try u.GetValue<string>() = uri with _ -> false
                            | _ -> false
                        | _ -> false
                    | _ -> false)

            let hasUnusedWarning =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as diagnosticsArray ->
                        diagnosticsArray
                        |> Seq.exists (fun d ->
                            match d with
                            | :? JsonObject as diag ->
                                let codeMatches =
                                    match diag["code"] with
                                    | :? JsonValue as codeValue ->
                                        try codeValue.GetValue<string>() = "unused" with _ -> false
                                    | _ -> false
                                let messageMatches =
                                    match diag["message"] with
                                    | :? JsonValue as messageValue ->
                                        try messageValue.GetValue<string>().Contains("defaults", StringComparison.Ordinal) with _ -> false
                                    | _ -> false
                                codeMatches && messageMatches
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasUnusedWarning |> should equal false
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``DidOpen does not publish unused warnings from included files`` () =
        let client = LspClient.start ()
        try
            initialize client

            let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-lsp-unused-include-{Guid.NewGuid():N}")
            Directory.CreateDirectory(tempRoot) |> ignore
            let includeFile = Path.Combine(tempRoot, "_helpers.fss")
            let mainFile = Path.Combine(tempRoot, "main.fss")
            File.WriteAllText(includeFile, "let with_flag x = x\n")
            File.WriteAllText(mainFile, "import \"_helpers.fss\"\nlet first_project_file x = x\nlet result = first_project_file 1\nresult\n")

            let uri = Uri(mainFile).AbsoluteUri
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(File.ReadAllText(mainFile))

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv when (try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false) ->
                        match msg["params"] with
                        | :? JsonObject as p ->
                            match p["uri"] with
                            | :? JsonValue as u ->
                                try u.GetValue<string>() = uri with _ -> false
                            | _ -> false
                        | _ -> false
                    | _ -> false)

            let hasIncludedUnusedWarning =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as diagnosticsArray ->
                        diagnosticsArray
                        |> Seq.exists (fun d ->
                            match d with
                            | :? JsonObject as diag ->
                                let codeOk =
                                    match diag["code"] with
                                    | :? JsonValue as cv -> (try cv.GetValue<string>() = "unused" with _ -> false)
                                    | _ -> false
                                let messageMentionsIncludedBinding =
                                    match diag["message"] with
                                    | :? JsonValue as mv ->
                                        try mv.GetValue<string>().Contains("with_flag", StringComparison.Ordinal) with _ -> false
                                    | _ -> false
                                codeOk && messageMentionsIncludedBinding
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasIncludedUnusedWarning |> should equal false
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``DidOpen does not publish unused warnings for underscore helper files`` () =
        let client = LspClient.start ()
        try
            initialize client

            let helperFile = Path.Combine(Path.GetTempPath(), $"_helpers-{Guid.NewGuid():N}.fss")
            File.WriteAllText(helperFile, "let append_part part acc = acc\n")

            let uri = Uri(helperFile).AbsoluteUri
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(File.ReadAllText(helperFile))

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv when (try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false) ->
                        match msg["params"] with
                        | :? JsonObject as p ->
                            match p["uri"] with
                            | :? JsonValue as u ->
                                try u.GetValue<string>() = uri with _ -> false
                            | _ -> false
                        | _ -> false
                    | _ -> false)

            let hasUnusedWarning =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as diagnosticsArray ->
                        diagnosticsArray
                        |> Seq.exists (fun d ->
                            match d with
                            | :? JsonObject as diag ->
                                match diag["code"] with
                                | :? JsonValue as cv -> (try cv.GetValue<string>() = "unused" with _ -> false)
                                | _ -> false
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasUnusedWarning |> should equal false
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``DidOpen does not report unbound variable for intrinsic print`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/print-intrinsic-test.fss"
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create("print \"hello\"")

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv ->
                        try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                    | _ -> false)

            let hasUnboundPrint =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as items ->
                        items
                        |> Seq.exists (fun item ->
                            match item with
                            | :? JsonObject as d ->
                                match d["message"] with
                                | :? JsonValue as m ->
                                    try m.GetValue<string>().Contains("Unbound variable 'print'", StringComparison.Ordinal) with _ -> false
                                | _ -> false
                            | _ -> false)
                    | _ -> false
                | _ -> false

            hasUnboundPrint |> should equal false
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Code action suggests quick fix for unbound variable typo`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/code-action-typo-test.fss"
            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create("let alpha = 1\nalph")

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv ->
                        try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                    | _ -> false)

            let diagnostics =
                match diagMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as items -> items
                    | _ -> JsonArray()
                | _ -> JsonArray()

            let req = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            req["textDocument"] <- textDocument
            let range = JsonObject()
            let startPos = JsonObject()
            startPos["line"] <- JsonValue.Create(1)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(1)
            endPos["character"] <- JsonValue.Create(4)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range
            let context = JsonObject()
            let diagnosticsCopy = JsonArray()
            for d in diagnostics do
                match d with
                | null -> ()
                | node ->
                    diagnosticsCopy.Add(node.DeepClone())
            context["diagnostics"] <- diagnosticsCopy
            req["context"] <- context

            LspClient.sendRequest client 18 "textDocument/codeAction" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 18 with _ -> false
                    | _ -> false)

            let hasAlphaSuggestion =
                match resp["result"] with
                | :? JsonArray as items ->
                    items
                    |> Seq.exists (fun item ->
                        match item with
                        | :? JsonObject as action ->
                            match action["title"] with
                            | :? JsonValue as title ->
                                try title.GetValue<string>().Contains("'alpha'", StringComparison.Ordinal) with _ -> false
                            | _ -> false
                        | _ -> false)
                | _ -> false

            hasAlphaSuggestion |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Document highlight returns local occurrences for selected symbol`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/document-highlight-test.fss"
            let source = "let alpha x = x + 1\nlet v = alpha 41\nalpha v"

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

            let hParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(9)
            hParams["textDocument"] <- textDocument
            hParams["position"] <- position

            LspClient.sendRequest client 14 "textDocument/documentHighlight" (Some hParams)
            let hResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 14 with _ -> false
                    | _ -> false)

            let count =
                match hResp["result"] with
                | :? JsonArray as items -> items |> Seq.length
                | _ -> 0

            count |> should be (greaterThanOrEqualTo 3)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Workspace symbol returns matches across opened documents`` () =
        let client = LspClient.start ()
        try
            initialize client

            let openDoc (uri: string) (source: string) =
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

            openDoc "file:///tmp/workspace-symbol-1.fss" "let alpha x = x + 1"
            openDoc "file:///tmp/workspace-symbol-2.fss" "let beta y = y + 2"

            let wsParams = JsonObject()
            wsParams["query"] <- JsonValue.Create("alpha")

            LspClient.sendRequest client 15 "workspace/symbol" (Some wsParams)
            let wsResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 15 with _ -> false
                    | _ -> false)

            let hasAlpha =
                match wsResp["result"] with
                | :? JsonArray as items ->
                    items
                    |> Seq.exists (fun item ->
                        match item with
                        | :? JsonObject as o ->
                            match o["name"] with
                            | :? JsonValue as v ->
                                try v.GetValue<string>() = "alpha" with _ -> false
                            | _ -> false
                        | _ -> false)
                | _ -> false

            hasAlpha |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Language server typing includes runtime externs`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/runtime-externs-typing-test.fss"
            let source = "let ok = Fs.exists \".\"\nok\n"

            let td = JsonObject()
            td["uri"] <- JsonValue.Create(uri)
            td["languageId"] <- JsonValue.Create("fscript")
            td["version"] <- JsonValue.Create(1)
            td["text"] <- JsonValue.Create(source)

            let didOpenParams = JsonObject()
            didOpenParams["textDocument"] <- td
            LspClient.sendNotification client "textDocument/didOpen" (Some didOpenParams)

            let diagnosticsMsg =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["method"] with
                    | :? JsonValue as mv ->
                        try mv.GetValue<string>() = "textDocument/publishDiagnostics" with _ -> false
                    | _ -> false)

            let diagnosticsCount =
                match diagnosticsMsg["params"] with
                | :? JsonObject as p ->
                    match p["diagnostics"] with
                    | :? JsonArray as arr -> arr.Count
                    | _ -> -1
                | _ -> -1

            Assert.That(diagnosticsCount, Is.EqualTo(0))
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Rename returns workspace edit for all symbol occurrences`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/rename-test.fss"
            let source = "let value = 1\nlet x = value + 2\nvalue"

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

            let renameParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(10)
            renameParams["textDocument"] <- textDocument
            renameParams["position"] <- position
            renameParams["newName"] <- JsonValue.Create("count")

            LspClient.sendRequest client 10 "textDocument/rename" (Some renameParams)
            let renameResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 10 with _ -> false
                    | _ -> false)

            let renameCount =
                match renameResp["result"] with
                | :? JsonObject as result ->
                    match result["changes"] with
                    | :? JsonObject as changes ->
                        match changes[uri] with
                        | :? JsonArray as edits ->
                            edits
                            |> Seq.filter (fun edit ->
                                match edit with
                                | :? JsonObject as o ->
                                    match o["newText"] with
                                    | :? JsonValue as v -> (try v.GetValue<string>() = "count" with _ -> false)
                                    | _ -> false
                                | _ -> false)
                            |> Seq.length
                        | _ -> 0
                    | _ -> 0
                | _ -> 0

            renameCount |> should be (greaterThanOrEqualTo 3)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Rename returns workspace edits across opened documents`` () =
        let client = LspClient.start ()
        try
            initialize client

            let openDoc (uri: string) (source: string) =
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

            let sourceUri = "file:///tmp/rename-source.fss"
            let usageUri = "file:///tmp/rename-usage.fss"
            openDoc sourceUri "let value = 1"
            openDoc usageUri "let a = value + 2\nvalue"

            let renameParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(usageUri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(9)
            renameParams["textDocument"] <- textDocument
            renameParams["position"] <- position
            renameParams["newName"] <- JsonValue.Create("count")

            LspClient.sendRequest client 26 "textDocument/rename" (Some renameParams)
            let renameResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 26 with _ -> false
                    | _ -> false)

            let changedUris =
                match renameResp["result"] with
                | :? JsonObject as result ->
                    match result["changes"] with
                    | :? JsonObject as changes ->
                        changes
                        |> Seq.map (fun kv -> kv.Key)
                        |> Set.ofSeq
                    | _ -> Set.empty
                | _ -> Set.empty

            changedUris.Contains(sourceUri) |> should equal true
            changedUris.Contains(usageUri) |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Rename does not rename record field labels when renaming variable`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/rename-field-label-test.fss"
            let source = "let value = 1\nlet recd = { value = value }\nvalue"

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

            let renameParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(2)
            renameParams["textDocument"] <- textDocument
            renameParams["position"] <- position
            renameParams["newName"] <- JsonValue.Create("count")

            LspClient.sendRequest client 13 "textDocument/rename" (Some renameParams)
            let renameResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 13 with _ -> false
                    | _ -> false)

            let editCount =
                match renameResp["result"] with
                | :? JsonObject as result ->
                    match result["changes"] with
                    | :? JsonObject as changes ->
                        match changes[uri] with
                        | :? JsonArray as edits -> edits |> Seq.length
                        | _ -> 0
                    | _ -> 0
                | _ -> 0

            // declaration + variable usage in record value + final usage
            editCount |> should equal 3
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Rename rejects invalid identifier target`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/rename-invalid-test.fss"
            let source = "let value = 1\nvalue"

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

            let renameParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(2)
            renameParams["textDocument"] <- textDocument
            renameParams["position"] <- position
            renameParams["newName"] <- JsonValue.Create("123bad")

            LspClient.sendRequest client 11 "textDocument/rename" (Some renameParams)
            let renameResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 11 with _ -> false
                    | _ -> false)

            let hasInvalidParamsError =
                match renameResp["error"] with
                | :? JsonObject as err ->
                    let codeOk =
                        match err["code"] with
                        | :? JsonValue as v -> (try v.GetValue<int>() = -32602 with _ -> false)
                        | _ -> false
                    let messageOk =
                        match err["message"] with
                        | :? JsonValue as v ->
                            let msg = v.GetValue<string>()
                            msg.Contains("Invalid rename target", StringComparison.Ordinal)
                        | _ -> false
                    codeOk && messageOk
                | _ -> false

            hasInvalidParamsError |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``PrepareRename returns placeholder and range for valid symbol`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/prepare-rename-test.fss"
            let source = "let total = 1\ntotal"

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

            let prepareParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(2)
            prepareParams["textDocument"] <- textDocument
            prepareParams["position"] <- position

            LspClient.sendRequest client 12 "textDocument/prepareRename" (Some prepareParams)
            let prepareResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 12 with _ -> false
                    | _ -> false)

            let hasPlaceholder =
                match prepareResp["result"] with
                | :? JsonObject as result ->
                    match result["placeholder"] with
                    | :? JsonValue as v -> (try v.GetValue<string>() = "total" with _ -> false)
                    | _ -> false
                | _ -> false

            hasPlaceholder |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
