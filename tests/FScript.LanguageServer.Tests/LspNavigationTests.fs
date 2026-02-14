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
type LspNavigationTests () =
    [<Test>]
    member _.``References returns all occurrences for a top-level binding`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/references-test.fss"
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

            let refsParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(9)
            refsParams["textDocument"] <- textDocument
            refsParams["position"] <- position
            let ctx = JsonObject()
            ctx["includeDeclaration"] <- JsonValue.Create(true)
            refsParams["context"] <- ctx

            LspClient.sendRequest client 6 "textDocument/references" (Some refsParams)
            let refsResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 6 with _ -> false
                    | _ -> false)

            let count =
                match refsResp["result"] with
                | :? JsonArray as items -> items |> Seq.length
                | _ -> 0

            count |> should be (greaterThanOrEqualTo 3)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``References returns occurrences across opened documents`` () =
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

            let sourceUri = "file:///tmp/references-source.fss"
            let usageUri = "file:///tmp/references-usage.fss"
            openDoc sourceUri "let alpha x = x + 1"
            openDoc usageUri "let one = alpha 1\nlet two = alpha 2"

            let refsParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(usageUri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(11)
            refsParams["textDocument"] <- textDocument
            refsParams["position"] <- position
            let ctx = JsonObject()
            ctx["includeDeclaration"] <- JsonValue.Create(true)
            refsParams["context"] <- ctx

            LspClient.sendRequest client 23 "textDocument/references" (Some refsParams)
            let refsResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 23 with _ -> false
                    | _ -> false)

            let uris =
                match refsResp["result"] with
                | :? JsonArray as items ->
                    items
                    |> Seq.choose (fun item ->
                        match item with
                        | :? JsonObject as o ->
                            match o["uri"] with
                            | :? JsonValue as v ->
                                try Some (v.GetValue<string>()) with _ -> None
                            | _ -> None
                        | _ -> None)
                    |> Set.ofSeq
                | _ -> Set.empty

            uris.Contains(sourceUri) |> should equal true
            uris.Contains(usageUri) |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``References honors includeDeclaration false across opened documents`` () =
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

            let sourceUri = "file:///tmp/references-nodecl-source.fss"
            let usageUri = "file:///tmp/references-nodecl-usage.fss"
            openDoc sourceUri "let alpha x = x + 1"
            openDoc usageUri "let value = alpha 41"

            let refsParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(usageUri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(12)
            refsParams["textDocument"] <- textDocument
            refsParams["position"] <- position
            let ctx = JsonObject()
            ctx["includeDeclaration"] <- JsonValue.Create(false)
            refsParams["context"] <- ctx

            LspClient.sendRequest client 27 "textDocument/references" (Some refsParams)
            let refsResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 27 with _ -> false
                    | _ -> false)

            let uris =
                match refsResp["result"] with
                | :? JsonArray as items ->
                    items
                    |> Seq.choose (fun item ->
                        match item with
                        | :? JsonObject as o ->
                            match o["uri"] with
                            | :? JsonValue as v ->
                                try Some (v.GetValue<string>()) with _ -> None
                            | _ -> None
                        | _ -> None)
                    |> Set.ofSeq
                | _ -> Set.empty

            uris.Contains(sourceUri) |> should equal false
            uris.Contains(usageUri) |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition resolves top-level binding usage`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/definition-test.fss"
            let source = "let inc x = x + 1\nlet y = inc 41"

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(11)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 8 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 8 with _ -> false
                    | _ -> false)

            let isDefinitionOnFirstLine =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false

                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false

            isDefinitionOnFirstLine |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition resolves injected stdlib function to virtual stdlib source`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/definition-injected-stdlib-test.fss"
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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(21)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 81 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 81 with _ -> false
                    | _ -> false)

            let resolvedUri =
                match defResp["result"] with
                | :? JsonObject as loc ->
                    match loc["uri"] with
                    | :? JsonValue as u ->
                        try Some (u.GetValue<string>()) with _ -> None
                    | _ -> None
                | _ -> None

            resolvedUri |> should equal (Some "fscript-stdlib:///Option.fss")
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition resolves symbol across opened documents`` () =
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

            let defUri = "file:///tmp/definition-source.fss"
            let useUri = "file:///tmp/definition-usage.fss"
            openDoc defUri "let alpha x = x + 1"
            openDoc useUri "let result = alpha 41"

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(useUri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(14)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 21 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 21 with _ -> false
                    | _ -> false)

            let resolvedUri =
                match defResp["result"] with
                | :? JsonObject as loc ->
                    match loc["uri"] with
                    | :? JsonValue as u ->
                        try Some (u.GetValue<string>()) with _ -> None
                    | _ -> None
                | _ -> None

            resolvedUri |> should equal (Some defUri)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition resolves include path to target file`` () =
        let client = LspClient.start ()
        try
            initialize client

            let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-lsp-include-{Guid.NewGuid():N}")
            Directory.CreateDirectory(tempRoot) |> ignore
            let includeFile = Path.Combine(tempRoot, "_helpers.fss")
            let mainFile = Path.Combine(tempRoot, "main.fss")
            File.WriteAllText(includeFile, "let helper = 42\n")
            File.WriteAllText(mainFile, "import \"_helpers.fss\"\nlet x = helper\n")

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(0)
            position["character"] <- JsonValue.Create(13)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 36 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 36 with _ -> false
                    | _ -> false)

            let expectedUri = Uri(includeFile).AbsoluteUri
            let resolvedUri =
                match defResp["result"] with
                | :? JsonObject as result ->
                    match result["uri"] with
                    | :? JsonValue as v ->
                        try Some (v.GetValue<string>()) with _ -> None
                    | _ -> None
                | _ -> None

            resolvedUri |> should equal (Some expectedUri)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition on included record field usage opens included file`` () =
        let client = LspClient.start ()
        try
            initialize client

            let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-lsp-include-field-{Guid.NewGuid():N}")
            Directory.CreateDirectory(tempRoot) |> ignore
            let includeFile = Path.Combine(tempRoot, "_protocol.fss")
            let mainFile = Path.Combine(tempRoot, "main.fss")

            File.WriteAllText(includeFile, "type ActionContext = { Command: string }\n")
            File.WriteAllText(mainFile, "import \"_protocol.fss\"\nlet dispatch (context: ActionContext) = context.Command\n")

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(52)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 360 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 360 with _ -> false
                    | _ -> false)

            let expectedUri = Uri(includeFile).AbsoluteUri
            let resolvedUri =
                match defResp["result"] with
                | :? JsonObject as result ->
                    match result["uri"] with
                    | :? JsonValue as v ->
                        try Some (v.GetValue<string>()) with _ -> None
                    | _ -> None
                | _ -> None

            resolvedUri |> should equal (Some expectedUri)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Type definition resolves record value to declared record type`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/type-definition-record-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet home = { City = \"Paris\"; Zip = 75000 }\nlet zip = home.Zip"

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(10)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 16 "textDocument/typeDefinition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 16 with _ -> false
                    | _ -> false)

            let pointsToTypeDecl =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false

                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false
            pointsToTypeDecl |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Type definition resolves inline nominal record annotation to declared type`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/type-definition-inline-annotation-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet format_address (address: { City: string; Zip: int }) = $\"{address.City} ({address.Zip})\""

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(30) // City in annotation
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 160 "textDocument/typeDefinition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 160 with _ -> false
                    | _ -> false)

            let pointsToTypeDecl =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false
                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false

            pointsToTypeDecl |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Type definition resolves annotated parameter usage to declared type`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/type-definition-parameter-usage-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet format_address (address: { City: string; Zip: int }) = address.City"

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(62) // address in address.City
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 161 "textDocument/typeDefinition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 161 with _ -> false
                    | _ -> false)

            let pointsToTypeDecl =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false
                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false

            pointsToTypeDecl |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Type definition resolves record literal call-argument field label to declared type`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/type-definition-record-call-arg-field-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet make_office_address (address: Address) = address\nlet officeAddress = make_office_address { City = \"London\"; Zip = 12345 }"

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(43) // City in literal argument
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 162 "textDocument/typeDefinition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 162 with _ -> false
                    | _ -> false)

            let pointsToTypeDecl =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false
                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false

            pointsToTypeDecl |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition resolves record literal call-argument field label to declared type`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/definition-record-call-arg-field-test.fss"
            let source = "type Address = { City: string; Zip: int }\nlet make_address (address: Address) = address\nlet office = make_address { City = \"London\"; Zip = 12345 }"

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(2)
            position["character"] <- JsonValue.Create(33) // City in literal argument
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 163 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 163 with _ -> false
                    | _ -> false)

            let pointsToTypeDecl =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false
                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false

            pointsToTypeDecl |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Type definition resolves record literal binding field label to declared type`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/type-definition-record-binding-field-test.fss"
            let source = "type Contact = { Name: string; City: string; Zip: int; Country: string }\nlet contact = { Name = \"Ada\"; City = \"Paris\"; Zip = 75000; Country = \"FR\" }"

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(1)
            position["character"] <- JsonValue.Create(30) // City in literal binding
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 164 "textDocument/typeDefinition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 164 with _ -> false
                    | _ -> false)

            let pointsToTypeDecl =
                match defResp["result"] with
                | :? JsonObject as result ->
                    let uriOk =
                        match result["uri"] with
                        | :? JsonValue as v -> (try v.GetValue<string>() = uri with _ -> false)
                        | _ -> false
                    let startLineOk =
                        match result["range"] with
                        | :? JsonObject as rangeObj ->
                            match rangeObj["start"] with
                            | :? JsonObject as startObj ->
                                match startObj["line"] with
                                | :? JsonValue as v -> (try v.GetValue<int>() = 0 with _ -> false)
                                | _ -> false
                            | _ -> false
                        | _ -> false
                    uriOk && startLineOk
                | _ -> false

            pointsToTypeDecl |> should equal true
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
    [<Test>]
    member _.``Definition resolves function return record field label to declared type in include file`` () =
        let client = LspClient.start ()
        try
            initialize client

            let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-lsp-type-definition-return-field-{Guid.NewGuid():N}")
            Directory.CreateDirectory(tempRoot) |> ignore
            let includeFile = Path.Combine(tempRoot, "_protocol.fss")
            let mainFile = Path.Combine(tempRoot, "main.fss")

            File.WriteAllText(includeFile, "type ActionContext = { Directory: string }\ntype ProjectInfo = { Id: string option; Outputs: string list; Dependencies: string list }\n")
            File.WriteAllText(mainFile, "import \"_protocol.fss\"\n[<export>] let defaults (context: ActionContext) =\n  let id = None\n  { Id = id; Outputs = [\"dist/**\"]; Dependencies = [] }\n")

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

            let defParams = JsonObject()
            let textDocument = JsonObject()
            textDocument["uri"] <- JsonValue.Create(uri)
            let position = JsonObject()
            position["line"] <- JsonValue.Create(3)
            position["character"] <- JsonValue.Create(4)
            defParams["textDocument"] <- textDocument
            defParams["position"] <- position

            LspClient.sendRequest client 364 "textDocument/definition" (Some defParams)
            let defResp =
                LspClient.readUntil client 10000 (fun msg ->
                    match msg["id"] with
                    | :? JsonValue as idv ->
                        try idv.GetValue<int>() = 364 with _ -> false
                    | _ -> false)

            let expectedUri = Uri(includeFile).AbsoluteUri
            let resolvedUri =
                match defResp["result"] with
                | :? JsonObject as result ->
                    match result["uri"] with
                    | :? JsonValue as v ->
                        try Some (v.GetValue<string>()) with _ -> None
                    | _ -> None
                | _ -> None

            resolvedUri |> should equal (Some expectedUri)
        finally
            try shutdown client with _ -> ()
            LspClient.stop client
