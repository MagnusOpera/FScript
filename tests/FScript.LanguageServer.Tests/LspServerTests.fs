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

module private LspWire =
    let private utf8 = UTF8Encoding(false)
    let mutable private pending = Array.empty<byte>

    let private readExactWithTimeout (stream: Stream) (buffer: byte[]) (offset: int) (count: int) (timeoutMs: int) =
        use cts = new CancellationTokenSource(timeoutMs)
        let mutable readTotal = 0
        while readTotal < count do
            let read =
                stream.ReadAsync(buffer.AsMemory(offset + readTotal, count - readTotal), cts.Token)
                |> fun t -> t.GetAwaiter().GetResult()

            if read <= 0 then
                failwith "Unexpected end of stream while reading LSP message."

            readTotal <- readTotal + read

    let readMessageWithTimeout (stream: Stream) (timeoutMs: int) : string =
        use cts = new CancellationTokenSource(timeoutMs)
        let headerBytes = ResizeArray<byte>()
        let one = Array.zeroCreate<byte> 1
        let marker = [| byte '\r'; byte '\n'; byte '\r'; byte '\n' |]
        let mutable matched = 0
        let mutable doneHeader = false

        if pending.Length > 0 then
            for b in pending do
                headerBytes.Add(b)
            pending <- Array.empty

        while not doneHeader do
            if headerBytes.Count >= marker.Length then
                let tail =
                    [| headerBytes[headerBytes.Count - 4]
                       headerBytes[headerBytes.Count - 3]
                       headerBytes[headerBytes.Count - 2]
                       headerBytes[headerBytes.Count - 1] |]
                if tail = marker then
                    doneHeader <- true
                else
                    let n = stream.ReadAsync(one.AsMemory(0, 1), cts.Token).GetAwaiter().GetResult()
                    if n <= 0 then failwith "Unexpected end of stream while reading LSP headers."
                    let b = one[0]
                    headerBytes.Add(b)
                    if b = marker[matched] then
                        matched <- matched + 1
                        if matched = marker.Length then doneHeader <- true
                    else
                        matched <- if b = marker[0] then 1 else 0
            else
                let n = stream.ReadAsync(one.AsMemory(0, 1), cts.Token).GetAwaiter().GetResult()
                if n <= 0 then failwith "Unexpected end of stream while reading LSP headers."
                let b = one[0]
                headerBytes.Add(b)
                if b = marker[matched] then
                    matched <- matched + 1
                    if matched = marker.Length then doneHeader <- true
                else
                    matched <- if b = marker[0] then 1 else 0

        let header = Encoding.ASCII.GetString(headerBytes.ToArray())
        let contentLength =
            header.Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryPick (fun line ->
                if line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase) then
                    Some (line.Substring("Content-Length:".Length).Trim() |> int)
                else
                    None)
            |> Option.defaultWith (fun () -> failwith "Missing Content-Length header")

        let payload = Array.zeroCreate<byte> contentLength
        readExactWithTimeout stream payload 0 contentLength timeoutMs
        utf8.GetString(payload)

    let writeMessage (stream: Stream) (payload: string) =
        let payloadBytes = utf8.GetBytes(payload)
        let header = $"Content-Length: {payloadBytes.Length}\r\n\r\n"
        let headerBytes = Encoding.ASCII.GetBytes(header)
        stream.Write(headerBytes, 0, headerBytes.Length)
        stream.Write(payloadBytes, 0, payloadBytes.Length)
        stream.Flush()

module private LspClient =
    type Client =
        { Process: Process
          Input: Stream
          Output: Stream }

    let private findRepoRoot () =
        let mutable current : DirectoryInfo option = Some (DirectoryInfo(AppContext.BaseDirectory))
        let mutable found: string option = None

        while current.IsSome && found.IsNone do
            let directory = current.Value
            let candidate = Path.Combine(directory.FullName, "FScript.sln")
            if File.Exists(candidate) then
                found <- Some directory.FullName
            else
                current <- Option.ofObj directory.Parent

        found |> Option.defaultWith (fun () -> failwith "Unable to locate repository root from test base directory")

    let start () =
        let root = findRepoRoot ()
        let serverProject = Path.Combine(root, "src", "FScript.LanguageServer", "FScript.LanguageServer.fsproj")
        let serverDll = Path.Combine(root, "src", "FScript.LanguageServer", "bin", "Release", "net10.0", "FScript.LanguageServer.dll")

        let buildPsi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = $"build \"{serverProject}\" -c Release -nologo -v q",
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true)

        use buildProc = new Process(StartInfo = buildPsi)
        if not (buildProc.Start()) then
            failwith "Unable to start dotnet build for language server test setup."
        buildProc.WaitForExit()
        if buildProc.ExitCode <> 0 || not (File.Exists(serverDll)) then
            let out = buildProc.StandardOutput.ReadToEnd()
            let err = buildProc.StandardError.ReadToEnd()
            failwith $"Failed to build language server test target. stdout: {out}\nstderr: {err}"

        let psi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = $"\"{serverDll}\"",
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true)

        let proc = new Process(StartInfo = psi)
        let started = proc.Start()
        if not started then failwith "Unable to start FScript language server process"

        { Process = proc
          Input = proc.StandardInput.BaseStream
          Output = proc.StandardOutput.BaseStream }

    let stop (client: Client) =
        if not client.Process.HasExited then
            try
                client.Process.Kill(true)
            with _ -> ()
        client.Process.Dispose()

    let sendRequest (client: Client) (id: int) (methodName: string) (parameters: JsonNode option) =
        let payload = JsonObject()
        payload["jsonrpc"] <- JsonValue.Create("2.0")
        payload["id"] <- JsonValue.Create(id)
        payload["method"] <- JsonValue.Create(methodName)
        payload["params"] <- (parameters |> Option.defaultValue (JsonObject()))
        LspWire.writeMessage client.Input (payload.ToJsonString())

    let sendNotification (client: Client) (methodName: string) (parameters: JsonNode option) =
        let payload = JsonObject()
        payload["jsonrpc"] <- JsonValue.Create("2.0")
        payload["method"] <- JsonValue.Create(methodName)
        payload["params"] <- (parameters |> Option.defaultValue (JsonObject()))
        LspWire.writeMessage client.Input (payload.ToJsonString())

    let readUntil (client: Client) (timeoutMs: int) (predicate: JsonObject -> bool) =
        let deadline = DateTime.UtcNow.AddMilliseconds(float timeoutMs)
        let mutable found: JsonObject option = None

        while found.IsNone && DateTime.UtcNow < deadline do
            let remaining = int (deadline - DateTime.UtcNow).TotalMilliseconds
            if remaining <= 0 then
                ()
            else
                let raw = LspWire.readMessageWithTimeout client.Output remaining
                let node = JsonNode.Parse(raw)
                match node with
                | :? JsonObject as obj when predicate obj ->
                    found <- Some obj
                | _ -> ()

        found |> Option.defaultWith (fun () -> failwith "Timed out waiting for expected LSP message")

[<TestFixture>]
type LspServerTests () =
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
    member _.``DidOpen publishes unused binding warning`` () =
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

            hasUnusedWarning |> should equal true
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
            let source = "let home = { City = \"Paris\"; Zip = 75000 }\nhome.City"

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
            let source = "let home = { City = \"Paris\"; Zip = 75000 }\nhome.City"

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
            File.WriteAllText(mainFile, "#include \"_helpers.fss\"\nlet x = helper\n")

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
    member _.``Hover returns record field information for dotted access`` () =
        let client = LspClient.start ()
        try
            initialize client

            let uri = "file:///tmp/hover-record-field-test.fss"
            let source = "let home = { City = \"Paris\"; Zip = 75000 }\nhome.City"

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
