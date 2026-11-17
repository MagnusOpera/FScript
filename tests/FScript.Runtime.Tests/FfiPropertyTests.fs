namespace FScript.Runtime.Tests

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Security.Cryptography
open FsCheck.FSharp
open FScript.Language
open FScript.Runtime
open NUnit.Framework
open RuntimePropertySupport

[<TestFixture; NonParallelizable>]
type FfiPropertyTests() =
    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.RegexCapturesAndFailures(seed: uint64) =
        verify "FfiPropertyTests.RegexCapturesAndFailures" 100 inputs seed (fun input ->
            let first = input.Text
            let second = string input.Number
            let escaped = Regex.Escape first
            let capture = $"\\A({escaped})({Regex.Escape second})\\z"
            value (some(VList [VString first; VString second])) (invoke RegexExterns.match_groups [VString capture; VString(first + second)])
            value (some(VList [])) (invoke RegexExterns.match_groups [VString($"\\A(?:{escaped})\\z"); VString first])
            value (some(VList [VString ""; VString("b" + first)])) (invoke RegexExterns.match_groups [VString($"\\A(a)?(b{escaped})\\z"); VString("b" + first)])
            value (VOption None) (invoke RegexExterns.match_groups [VString($"\\A{escaped}\\z"); VString(first + "\000")])
            value (VOption None) (invoke RegexExterns.match_groups [VString("(" + escaped); VString first]))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.EncodingRoundTrips(seed: uint64) =
        verify "FfiPropertyTests.EncodingRoundTrips" 100 inputs seed (fun input ->
            let bytes = input.Bytes |> List.map byte |> List.toArray
            let values = integers input.Bytes
            let utf8 = Encoding.UTF8.GetBytes input.Text
            let utf8Values = utf8 |> Array.toList |> List.map (int >> int64 >> VInt) |> VList
            let base64 = Convert.ToBase64String bytes
            let hex = Convert.ToHexString(bytes).ToLowerInvariant()
            let uri = Uri.EscapeDataString input.Text
            for name, argument, expected in
                ["Encoding.utf8Encode", VString input.Text, utf8Values
                 "Encoding.utf8Decode", utf8Values, VString input.Text
                 "Encoding.base64Encode", values, VString base64
                 "Encoding.base64Decode", VString base64, values
                 "Encoding.hexEncode", values, VString hex
                 "Encoding.hexDecode", VString(hex.ToUpperInvariant()), values
                 "Encoding.uriEncode", VString input.Text, VString uri
                 "Encoding.uriDecode", VString uri, VString input.Text
                 "Encoding.uriDecode", VString(uri + "+"), VString(input.Text + "+")] do
                value (some expected) (invokePure name [argument]))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.InvalidEncodingsReturnNone(seed: uint64) =
        verify "FfiPropertyTests.InvalidEncodingsReturnNone" 100 inputs seed (fun input ->
            let bytes = input.Bytes |> List.map byte |> List.toArray
            let unpairedSurrogate = System.String(char 0xD800, 1) + input.Text
            for name, invalid in
                ["Encoding.utf8Decode", integers (255 :: input.Bytes)
                 "Encoding.utf8Encode", VString unpairedSurrogate
                 "Encoding.uriEncode", VString unpairedSurrogate
                 "Hash.sha256", VString unpairedSurrogate
                 "Encoding.base64Decode", VString("!" + Convert.ToBase64String bytes)
                 "Encoding.hexDecode", VString(Convert.ToHexString bytes + "z")
                 "Encoding.uriDecode", VString(Uri.EscapeDataString input.Text + "%")
                 "Encoding.uriDecode", VString "%ff"] do value (VOption None) (invokePure name [invalid])
            for name in ["Encoding.utf8Decode"; "Encoding.base64Encode"; "Encoding.hexEncode"; "Hash.sha256Bytes"] do
                for bad in [-1; 256 + abs input.Number] do value (VOption None) (invokePure name [integers [bad]]))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.HashesMatchStandardByteDigests(seed: uint64) =
        verify "FfiPropertyTests.HashesMatchStandardByteDigests" 100 inputs seed (fun input ->
            let bytes = Encoding.UTF8.GetBytes input.Text
            let binary = input.Bytes |> List.map byte |> List.toArray
            let hex (bytes: byte array) = Convert.ToHexString(bytes).ToLowerInvariant()
            value (some(VString(hex(MD5.HashData bytes)))) (invoke HashExterns.md5 [VString input.Text])
            value (some(VString(hex(SHA256.HashData bytes)))) (invokePure "Hash.sha256" [VString input.Text])
            value (some(VString(hex(SHA256.HashData binary)))) (invokePure "Hash.sha256Bytes" [integers input.Bytes]))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.GuidValuesHaveCanonicalVersionFourFormat(seed: uint64) =
        verify "FfiPropertyTests.GuidValuesHaveCanonicalVersionFourFormat" 50 inputs seed (fun input ->
            // Entropy itself is deliberately not the oracle; exact GUID values
            // and collision-free random batches are not deterministic promises.
            for argument in [VUnit; VInt(int64 input.Number); VString input.Text; integers input.Numbers] do
                match invoke GuidExterns.new_guid [argument] with
                | VOption(Some(VString text)) ->
                    let guid = Guid.ParseExact(text, "D")
                    equal (guid.ToString "D") text
                    equal '4' text[14]
                    equal true ("89ab".Contains text[19])
                | actual -> failwith ($"Guid.new returned {actual}"))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.ConsolePreservesTextLinesAndEof(seed: uint64) =
        verify "FfiPropertyTests.ConsolePreservesTextLinesAndEof" 50 inputs seed (fun input ->
            let lines = input.Numbers |> List.map (fun n -> input.Text.Replace("\r", " ").Replace("\n", " ") + string n)
            let content = if lines.IsEmpty then "" else String.concat "\n" lines + "\n"
            let originalInput, originalOutput = Console.In, Console.Out
            use reader = new StringReader(content)
            use writer = new StringWriter()
            try
                Console.SetIn reader
                Console.SetOut writer
                value VUnit (invoke ConsoleExterns.write_line [VString input.Text])
                equal (input.Text + Environment.NewLine) (writer.ToString())
                for line in lines do value (some(VString line)) (invoke ConsoleExterns.read_line [VUnit])
                for _ in 1 .. 2 do value (VOption None) (invoke ConsoleExterns.read_line [VUnit])
            finally Console.SetIn originalInput; Console.SetOut originalOutput)

    static member ExpectedCatalog =
        let io, threading, read = HostPermissions.IO, HostPermissions.Threading, HostPermissions.IO ||| HostPermissions.Interactive
        [ for name in ["Fs.readText"; "Fs.exists"; "Fs.kind"; "Fs.createDirectory"; "Fs.writeText"; "Fs.glob"; "Fs.enumerateFiles"; "Guid.new"; "Console.writeLine"] -> name, io ]
        @ ["Console.readLine", read]
        @ [for name in ["Task.spawn"; "Task.await"; "Task.delay"; "Task.fromResult"; "Task.continueWith"; "Task.whenAll"; "Task.whenAny"; "Task.awaitAll"; "Mailbox.start"; "Mailbox.post"; "Mailbox.finish"] -> name, threading]
        @ [for name in ["Fs.combinePath"; "Fs.parentDirectory"; "Fs.extension"; "Fs.fileNameWithoutExtension"; "Json.serialize"; "Json.deserialize"; "Xml.queryValues"; "Regex.matchGroups"; "Hash.md5"; "Hash.sha256"; "Hash.sha256Bytes"; "Encoding.utf8Encode"; "Encoding.utf8Decode"; "Encoding.base64Encode"; "Encoding.base64Decode"; "Encoding.hexEncode"; "Encoding.hexDecode"; "Encoding.uriEncode"; "Encoding.uriDecode"] -> name, HostPermissions.None]

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.RegistryRespectsCapabilityMatrix(seed: uint64) =
        verify "FfiPropertyTests.RegistryRespectsCapabilityMatrix" 100 inputs seed (fun input ->
            let permissions = enum<HostPermissions>(abs input.Number % 16)
            let context = { HostContext.create "." with Permissions = permissions }
            let actual = Registry.registration context
            let expected = FfiPropertyTests.ExpectedCatalog |> List.filter (fun (_, required) -> (permissions &&& required) = required) |> List.map fst |> Set.ofList
            equal expected (actual.Functions |> List.map _.Name |> Set.ofList)
            equal actual.Functions.Length expected.Count
            equal permissions actual.Permissions
            equal ((permissions &&& HostPermissions.Threading) <> HostPermissions.None) (actual.Types |> List.exists (fun t -> t.Name = "Mailbox"))
            equal ((permissions &&& HostPermissions.IO) <> HostPermissions.None) (actual.Unions |> List.exists (fun t -> t.Name = "FsKind")))

    [<Test>]
    member _.EveryRegisteredFfiHasAnExplicitPropertyCoverageEntry() =
        let all = Registry.all { HostContext.create "." with Permissions = HostPermissions.All }
        equal (FfiPropertyTests.ExpectedCatalog |> Map.ofList) (all |> List.map (fun fn -> fn.Name, fn.RequiredPermissions) |> Map.ofList)
