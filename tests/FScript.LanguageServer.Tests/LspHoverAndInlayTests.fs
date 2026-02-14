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
type LspHoverAndInlayTests () =
    [<Test>]
    member _.``Inlay hints return parameter labels for function calls`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-params-test.fss"
            let source = "let add x y = x + y\nlet z = add(1, 2)"

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
            startPos["line"] <- JsonValue.Create(1)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(1)
            endPos["character"] <- JsonValue.Create(20)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 32 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 32 with _ -> false
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

            labels |> should contain "x:"
            labels |> should contain "y:"
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Inlay hints do not show parameter labels on typed function declarations`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-typed-declaration-test.fss"
            let source =
                "type ActionContext = { Name: string }\n"
                + "let defaults (context: ActionContext) = context.Name\n"

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
            startPos["line"] <- JsonValue.Create(1)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(1)
            endPos["character"] <- JsonValue.Create(50)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 45 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 45 with _ -> false
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

            labels |> should not' (contain "context:")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Inlay hints include inferred type for value bindings`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-types-test.fss"
            let source = "let answer = 42\nanswer"

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
            endPos["character"] <- JsonValue.Create(20)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 33 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 33 with _ -> false
                    | _ -> false)

            let hasTypeHint =
                match resp["result"] with
                | :? JsonArray as hints ->
                    hints
                    |> Seq.exists (fun hint ->
                        match hint with
                        | :? JsonObject as h ->
                            match h["label"] with
                            | :? JsonValue as v ->
                                try v.GetValue<string>() = ": int" with _ -> false
                            | _ -> false
                        | _ -> false)
                | _ -> false

            hasTypeHint |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Inlay hints include inferred type for lambda parameter`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-lambda-param-test.fss"
            let source = "let inc = fun x -> x + 1\ninc 2"

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

            LspClient.sendRequest client 34 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 34 with _ -> false
                    | _ -> false)

            let hasLambdaParamTypeHint =
                match resp["result"] with
                | :? JsonArray as hints ->
                    hints
                    |> Seq.exists (fun hint ->
                        match hint with
                        | :? JsonObject as h ->
                            match h["label"] with
                            | :? JsonValue as v ->
                                try v.GetValue<string>() = ": int" with _ -> false
                            | _ -> false
                        | _ -> false)
                | _ -> false

            hasLambdaParamTypeHint |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Inlay hints include inferred return type for function declarations`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-function-return-test.fss"
            let source = "let is_empty values = values = []\nis_empty []"

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
            endPos["character"] <- JsonValue.Create(40)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 48 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 48 with _ -> false
                    | _ -> false)

            let hasBoolReturnHint =
                match resp["result"] with
                | :? JsonArray as hints ->
                    hints
                    |> Seq.exists (fun hint ->
                        match hint with
                        | :? JsonObject as h ->
                            match h["label"] with
                            | :? JsonValue as v ->
                                try v.GetValue<string>() = ": bool" with _ -> false
                            | _ -> false
                        | _ -> false)
                | _ -> false

            hasBoolReturnHint |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Inlay hints include inferred type for option pattern variable`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/inlay-hints-option-pattern-var-test.fss"
            let source =
                "let firstEven = Some 2\n"
                + "match firstEven with\n"
                + "| Some x -> x\n"
                + "| None -> 0\n"

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
            startPos["line"] <- JsonValue.Create(2)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(2)
            endPos["character"] <- JsonValue.Create(20)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 44 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 44 with _ -> false
                    | _ -> false)

            let hasPatternVarTypeHint =
                match resp["result"] with
                | :? JsonArray as hints ->
                    hints
                    |> Seq.exists (fun hint ->
                        match hint with
                        | :? JsonObject as h ->
                            match h["label"] with
                            | :? JsonValue as v ->
                                try v.GetValue<string>() = ": int" with _ -> false
                            | _ -> false
                        | _ -> false)
                | _ -> false

            hasPatternVarTypeHint |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Inlay hints can be disabled through initialization options`` () =
        let client = LspClient.start ()
        try
            let options = JsonObject()
            options["inlayHintsEnabled"] <- JsonValue.Create(false)
            initializeWith client (Some options)

            let uri = "file:///tmp/inlay-hints-disabled-test.fss"
            let source = "let add x y = x + y\nlet z = add(1, 2)"

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
            startPos["line"] <- JsonValue.Create(1)
            startPos["character"] <- JsonValue.Create(0)
            let endPos = JsonObject()
            endPos["line"] <- JsonValue.Create(1)
            endPos["character"] <- JsonValue.Create(20)
            range["start"] <- startPos
            range["end"] <- endPos
            req["range"] <- range

            LspClient.sendRequest client 35 "textDocument/inlayHint" (Some req)
            let resp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 35 with _ -> false
                    | _ -> false)

            let isEmpty =
                match resp["result"] with
                | :? JsonArray as hints -> (hints |> Seq.length) = 0
                | _ -> false

            isEmpty |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover shows named arguments for injected stdlib function`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-injected-stdlib-test.fss"
            let source = "let value = Option.map (fun x -> x + 1) (Some 1)\nvalue"

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
            position["character"] <- JsonValue.Create(21)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 71 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 71 with _ -> false
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
                hoverValue.Contains("Option.map:", StringComparison.Ordinal)
                && hoverValue.Contains("(mapper:", StringComparison.Ordinal)
                && hoverValue.Contains("(value:", StringComparison.Ordinal)
                && hoverValue.Contains("injected-function", StringComparison.Ordinal)

            Assert.That(hasExpectedHover, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover shows function parameters when type inference is unavailable`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-function-params-fallback-test.fss"
            let source =
                "let bad = 1 + true\n"
                + "let with_batch_projects context create_command =\n"
                + "    create_command context\n"
                + "with_batch_projects\n"

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
            position["line"] <- JsonValue.Create(3)
            position["character"] <- JsonValue.Create(5)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 47 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 47 with _ -> false
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
                hoverValue.Contains("with_batch_projects:", StringComparison.Ordinal)
                && hoverValue.Contains("->", StringComparison.Ordinal)

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover returns record field information for dotted access`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-record-field-test.fss"
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

            let hoverParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(7)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 30 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 30 with _ -> false
                    | _ -> false)

            let hasFieldHover =
                match hoverResp["result"] with
                | :? JsonObject as result ->
                    match result["contents"] with
                    | :? JsonObject as contents ->
                        match contents["value"] with
                        | :? JsonValue as value ->
                            let text = value.GetValue<string>()
                            text.Contains("City : string", StringComparison.Ordinal)
                            && text.Contains("record-field", StringComparison.Ordinal)
                        | _ -> false
                    | _ -> false
                | _ -> false

            hasFieldHover |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover returns inferred type for local lambda variables`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-local-variables-test.fss"
            let source =
                "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)\n"
                + "let values =\n"
                + "    [0..9]\n"
                + "    |> List.map (fun i ->\n"
                + "        i |> fib |> fun x ->\n"
                + "            $\"{x}\")\n"

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

            let requestHover (requestId: int) (line: int) (character: int) =
                let hoverParams = JsonObject()
                let textDocument = JsonObject()
                textDocument["uri"] <- JsonValue.Create(uri)
                let position = JsonObject()
                position["line"] <- JsonValue.Create(line)
                position["character"] <- JsonValue.Create(character)
                hoverParams["textDocument"] <- textDocument
                hoverParams["position"] <- position
                LspClient.sendRequest client requestId "textDocument/hover" (Some hoverParams)
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = requestId with _ -> false
                    | _ -> false)

            let hoverText (resp: JsonNode) =
                match resp["result"] with
                | :? JsonObject as result ->
                    match result["contents"] with
                    | :? JsonObject as contents ->
                        match contents["value"] with
                        | :? JsonValue as value -> value.GetValue<string>()
                        | _ -> ""
                    | _ -> ""
                | _ -> ""

            let hasLocalHoverType (text: string) (name: string) (typeText: string) =
                text.Contains($"{name} : {typeText}", StringComparison.Ordinal)
                && text.Contains("local-variable", StringComparison.Ordinal)

            let hoverI = requestHover 41 3 21
            let hoverX = requestHover 42 4 24
            let hoverIText = hoverText hoverI
            let hoverXText = hoverText hoverX

            Assert.That(hasLocalHoverType hoverIText "i" "int", Is.True, $"Unexpected hover for i: {hoverIText}")
            Assert.That(hasLocalHoverType hoverXText "x" "int", Is.True, $"Unexpected hover for x: {hoverXText}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover returns local binding type even when another top-level binding has a type error`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-local-binding-best-effort-test.fss"
            let source =
                "let bad = 1 + true\n"
                + "let restore context =\n"
                + "    let locked = context = \"locked\"\n"
                + "    if locked then \"x\" else \"y\"\n"
                + "restore \"u\"\n"

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
            position["line"] <- JsonValue.Create(3)
            position["character"] <- JsonValue.Create(8)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 50 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 50 with _ -> false
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
                hoverValue.Contains("locked : bool", StringComparison.Ordinal)
                && hoverValue.Contains("local-variable", StringComparison.Ordinal)

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover on top-level function is not shadowed by same-name local binding`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-top-level-not-shadowed-by-local-test.fss"
            let source =
                "let restore (context: string option) =\n"
                + "    let restore = context\n"
                + "    restore\n"
                + "restore None\n"

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
            position["character"] <- JsonValue.Create(5)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 51 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 51 with _ -> false
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
                hoverValue.Contains("restore:", StringComparison.Ordinal)
                && hoverValue.Contains("local-variable", StringComparison.Ordinal) |> not

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover resolves nearest local binding when name is reused across functions`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-nearest-local-reused-name-test.fss"
            let source =
                "let first args =\n"
                + "    let flag = args = \"x\"\n"
                + "    flag\n"
                + "let second args =\n"
                + "    let flag = args = 42\n"
                + "    flag\n"
                + "second 42\n"

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
            position["line"] <- JsonValue.Create(5)
            position["character"] <- JsonValue.Create(6)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 52 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 52 with _ -> false
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
                hoverValue.Contains("flag : bool", StringComparison.Ordinal)
                && hoverValue.Contains("local-variable", StringComparison.Ordinal)

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover resolves local let declaration identifier`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-local-let-declaration-test.fss"
            let source =
                "let defaults context =\n"
                + "    let dependencies =\n"
                + "        if context then [] else []\n"
                + "    dependencies\n"
                + "defaults true\n"

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
            position["character"] <- JsonValue.Create(9)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 53 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 53 with _ -> false
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
                hoverValue.Contains("dependencies : unknown list", StringComparison.Ordinal)
                && hoverValue.Contains("local-variable", StringComparison.Ordinal)

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Hover infers local let type from returned record field when best-effort falls back`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-local-let-return-record-field-test.fss"
            let source =
                "type ProjectInfo = { Id: string option; Outputs: string list; Dependencies: string list }\n"
                + "let external_call x = missing_function x\n"
                + "let defaults context =\n"
                + "    let dependencies =\n"
                + "        context\n"
                + "        |> external_call\n"
                + "        |> Option.defaultValue []\n"
                + "    { Id = None; Outputs = []; Dependencies = dependencies }\n"
                + "defaults \"ok\"\n"

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
            position["line"] <- JsonValue.Create(3)
            position["character"] <- JsonValue.Create(9)
            hoverParams["textDocument"] <- textDocument
            hoverParams["position"] <- position

            LspClient.sendRequest client 54 "textDocument/hover" (Some hoverParams)
            let hoverResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 54 with _ -> false
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
                hoverValue.Contains("dependencies : string list", StringComparison.Ordinal)
                && hoverValue.Contains("local-variable", StringComparison.Ordinal)

            Assert.That(hasExpectedHoverValue, Is.True, $"Unexpected hover text: {hoverValue}")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
