namespace FScript.LanguageServer

open System
open System.IO
open System.Text
open System.Text.Json.Nodes

module LspProtocol =
    let private utf8 = UTF8Encoding(false)
    let private stdin = Console.OpenStandardInput()
    let private stdout = Console.OpenStandardOutput()

    let private jsonNull () = JsonNode.Parse("null")

    let sendMessage (payload: string) =
        let bytes = utf8.GetBytes(payload)
        let header = $"Content-Length: {bytes.Length}\r\n\r\n"
        let headerBytes = Encoding.ASCII.GetBytes(header)
        stdout.Write(headerBytes, 0, headerBytes.Length)
        stdout.Write(bytes, 0, bytes.Length)
        stdout.Flush()

    let sendResponse (idNode: JsonNode) (resultNode: JsonNode option) =
        let obj = JsonObject()
        obj["jsonrpc"] <- JsonValue.Create("2.0")
        obj["id"] <- idNode.DeepClone()
        obj["result"] <-
            match resultNode with
            | Some node -> node
            | None -> jsonNull ()
        sendMessage (obj.ToJsonString())

    let sendError (idNode: JsonNode) (code: int) (message: string) =
        let err = JsonObject()
        err["code"] <- JsonValue.Create(code)
        err["message"] <- JsonValue.Create(message)

        let obj = JsonObject()
        obj["jsonrpc"] <- JsonValue.Create("2.0")
        obj["id"] <- idNode.DeepClone()
        obj["error"] <- err
        sendMessage (obj.ToJsonString())

    let sendNotification (methodName: string) (paramsNode: JsonNode option) =
        let obj = JsonObject()
        obj["jsonrpc"] <- JsonValue.Create("2.0")
        obj["method"] <- JsonValue.Create(methodName)
        obj["params"] <-
            match paramsNode with
            | Some node -> node
            | None -> jsonNull ()
        sendMessage (obj.ToJsonString())

    let rec private readExact (stream: Stream) (buffer: byte[]) (offset: int) (count: int) =
        if count > 0 then
            let read = stream.Read(buffer, offset, count)
            if read <= 0 then
                raise (EndOfStreamException("Unexpected end of stream while reading LSP payload."))
            readExact stream buffer (offset + read) (count - read)

    let tryReadMessage () : string option =
        let headerBytes = ResizeArray<byte>()
        let mutable matched = 0
        let marker = [| byte '\r'; byte '\n'; byte '\r'; byte '\n' |]
        let mutable ended = false

        while not ended do
            let b = stdin.ReadByte()
            if b = -1 then
                if headerBytes.Count = 0 then
                    ended <- true
                else
                    raise (EndOfStreamException("Unexpected end of stream while reading LSP headers."))
            else
                let bb = byte b
                headerBytes.Add(bb)
                if bb = marker[matched] then
                    matched <- matched + 1
                    if matched = marker.Length then
                        ended <- true
                else
                    matched <- if bb = marker[0] then 1 else 0

        if headerBytes.Count = 0 then
            None
        else
            let headerText = Encoding.ASCII.GetString(headerBytes.ToArray())
            let contentLength =
                headerText.Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.tryPick (fun line ->
                    if line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase) then
                        line.Substring("Content-Length:".Length).Trim() |> int |> Some
                    else
                        None)
                |> Option.defaultWith (fun () -> failwith "Missing Content-Length header")

            let payload = Array.zeroCreate<byte> contentLength
            readExact stdin payload 0 contentLength
            Some (utf8.GetString(payload))
