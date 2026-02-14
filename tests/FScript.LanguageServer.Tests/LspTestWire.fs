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
module internal LspWire =
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
