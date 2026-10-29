namespace FScript.Runtime

open System
open System.Text
open System.Security.Cryptography
open FScript.Language

/// Content transformations only: no filesystem, network, environment, or clock access.
module PureExterns =
    let private utf8 = UTF8Encoding(false,true)
    let private bytes = function
        | VList values -> values |> List.map (function VInt n when n >= 0L && n <= 255L -> byte n | _ -> invalidArg "bytes" "Bytes must be integers between 0 and 255.") |> List.toArray
        | _ -> invalidArg "bytes" "Expected a byte list."
    let private byteList (values: byte array) = values |> Array.toList |> List.map (int64 >> VInt) |> VList
    let private hex (values: byte array) = BitConverter.ToString(values).Replace("-", "").ToLowerInvariant()
    let private hexDigit c =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else invalidArg "text" "Invalid hexadecimal digit."
    let private decodeHex (text: string) =
        if text.Length % 2 <> 0 then invalidArg "text" "Hexadecimal input must have even length."
        Array.init (text.Length/2) (fun i -> byte (16 * hexDigit text[2*i] + hexDigit text[2*i+1]))
    let private decodeUri (text: string) =
        let source = utf8.GetBytes text
        let result = ResizeArray<byte>()
        let mutable index = 0
        while index < source.Length do
            if source[index] = byte '%' then
                if index + 2 >= source.Length then invalidArg "text" "Incomplete URI escape."
                result.Add(byte (16 * hexDigit (char source[index+1]) + hexDigit (char source[index+2])))
                index <- index + 3
            else
                result.Add source[index]
                index <- index + 1
        utf8.GetString(result.ToArray())

    let private invoke name = function
        | [VString text] ->
            match name with
            | "Encoding.utf8Encode" -> byteList(utf8.GetBytes text)
            | "Encoding.base64Decode" -> byteList(Convert.FromBase64String text)
            | "Encoding.hexDecode" -> byteList(decodeHex text)
            | "Encoding.uriEncode" -> utf8.GetBytes text |> ignore; VString(Uri.EscapeDataString text)
            | "Encoding.uriDecode" -> VString(decodeUri text)
            | "Hash.sha256" ->
                use algorithm = SHA256.Create()
                VString(hex(algorithm.ComputeHash(utf8.GetBytes text)))
            | _ -> invalidArg "arguments" $"Invalid arguments to {name}."
        | [VList _ as value] ->
            let data = bytes value
            match name with
            | "Encoding.utf8Decode" -> VString(utf8.GetString data)
            | "Encoding.base64Encode" -> VString(Convert.ToBase64String data)
            | "Encoding.hexEncode" -> VString(hex data)
            | "Hash.sha256Bytes" ->
                use algorithm = SHA256.Create()
                VString(hex(algorithm.ComputeHash data))
            | _ -> invalidArg "arguments" $"Invalid arguments to {name}."
        | _ -> invalidArg "arguments" $"Invalid arguments to {name}."

    let functions : ExternalFunction list =
        SupportLibrary.runtimeSignatures |> List.map (fun (name,scheme,parameters) ->
            { Name=name; Scheme=scheme; Arity=parameters.Length
              Impl=fun _ arguments ->
                  try VOption(Some(invoke name arguments))
                  with
                  | :? ArgumentException | :? FormatException -> VOption None })
