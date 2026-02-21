namespace FScript.Language.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type HostExternTests () =
    let host = { RootDirectory = System.IO.Directory.GetCurrentDirectory() }
    let externs = Registry.all host

    [<Test>]
    member _.``Map externs support CRUD`` () =
        let script = "Map.empty |> Map.add \"a\" 1 |> Map.tryGet \"a\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

    [<Test>]
    member _.``Map indexer is the primitive and Map.tryGet matches it`` () =
        let script = "let m = { [\"a\"] = 1 }\n(m[\"a\"], Map.tryGet \"a\" m)"
        match Helpers.evalWithExterns externs script with
        | VTuple [ VOption (Some (VInt 1L)); VOption (Some (VInt 1L)) ] -> ()
        | _ -> Assert.Fail("Expected indexer and Map.tryGet to both return Some 1")

    [<Test>]
    member _.``Map.empty behaves as a value and cannot be invoked`` () =
        match Helpers.evalWithExterns externs "Map.empty |> Map.count" with
        | VInt 0L -> ()
        | _ -> Assert.Fail("Expected empty map count 0")

        let act () = Helpers.evalWithExterns externs "Map.empty ()" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``Map.ofList builds map from tuple list`` () =
        let script = "Map.ofList [(\"a\", 1); (\"b\", 2)] |> Map.tryGet \"b\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 2L)) -> ()
        | _ -> Assert.Fail("Expected Some 2")

    [<Test>]
    member _.``Map.ofList supports indented next-line argument`` () =
        let script = "Map.ofList\n    [(\"a\", 1); (\"b\", 2)]\n|> Map.tryGet \"b\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 2L)) -> ()
        | _ -> Assert.Fail("Expected Some 2 from multiline call")

    [<Test>]
    member _.``Map.ofList keeps last duplicate key`` () =
        let script = "Map.ofList [(\"a\", 1); (\"a\", 2)] |> Map.tryGet \"a\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VInt 2L)) -> ()
        | _ -> Assert.Fail("Expected last duplicate value")

    [<Test>]
    member _.``Native map literal composes with Map externs`` () =
        match Helpers.evalWithExterns externs "{ [\"a\"] = 1; [\"b\"] = 2 } |> Map.count" with
        | VInt 2L -> ()
        | _ -> Assert.Fail("Expected map count 2")

        match Helpers.evalWithExterns externs "{ [\"a\"] = 1; [\"b\"] = 2 } |> Map.tryGet \"b\"" with
        | VOption (Some (VInt 2L)) -> ()
        | _ -> Assert.Fail("Expected Some 2")

    [<Test>]
    member _.``Map externs support count filter fold and choose`` () =
        match Helpers.evalWithExterns externs "Map.ofList [(\"a\", 1); (\"b\", 2); (\"c\", 3)] |> Map.count" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected map count 3")

        match Helpers.evalWithExterns externs "Map.ofList [(\"a\", 1); (\"b\", 2); (\"c\", 3)] |> Map.filter (fun k -> fun v -> v % 2 = 1) |> Map.count" with
        | VInt 2L -> ()
        | _ -> Assert.Fail("Expected filtered map count 2")

        match Helpers.evalWithExterns externs "Map.ofList [(\"a\", 1); (\"b\", 2); (\"c\", 3)] |> Map.fold (fun s -> fun _ -> fun v -> s + v) 0" with
        | VInt 6L -> ()
        | _ -> Assert.Fail("Expected folded sum 6")

        match Helpers.evalWithExterns externs "Map.ofList [(\"a\", 1); (\"b\", 2); (\"c\", 3)] |> Map.choose (fun _ -> fun v -> if v % 2 = 0 then Some (v * 10) else None) |> Map.tryGet \"b\"" with
        | VOption (Some (VInt 20L)) -> ()
        | _ -> Assert.Fail("Expected chosen map value 20 for key b")

    [<Test>]
    member _.``Map.map maps values and Map.iter applies key-value iterator`` () =
        match Helpers.evalWithExterns externs "Map.ofList [(\"a\", 1); (\"b\", 2)] |> Map.map (fun v -> v + 10) |> Map.tryGet \"b\"" with
        | VOption (Some (VInt 12L)) -> ()
        | _ -> Assert.Fail("Expected mapped map value 12 for key b")

        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result =
                Helpers.evalWithExterns externs
                    "Map.ofList [(\"a\", 1); (\"b\", 2)] |> Map.iter (fun key -> fun value -> print $\"{key}:{value}\")"
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().Replace("\r\n", "\n").TrimEnd() |> should equal "a:1\nb:2"
        finally
            Console.SetOut(original)

    [<Test>]
    member _.``Fs write-side externs operate under root confinement`` () =
        let root = Path.Combine(Path.GetTempPath(), "fscript-host-extern-tests", Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let localExterns = Registry.all { RootDirectory = root }
            let script =
                "let created = Fs.createDirectory \"tmp\"\n" +
                "let written = Fs.writeText \"tmp/file.txt\" \"hello\"\n" +
                "let exists = Fs.exists \"tmp/file.txt\"\n" +
                "let file = Fs.isFile \"tmp/file.txt\"\n" +
                "let dir = Fs.isDirectory \"tmp\"\n" +
                "(created, written, exists, file, dir)"

            match Helpers.evalWithExterns localExterns script with
            | VTuple [ VBool true; VBool true; VBool true; VBool true; VBool true ] -> ()
            | _ -> Assert.Fail("Expected Fs write-side externs to succeed")
        finally
            if Directory.Exists(root) then
                Directory.Delete(root, true)

    [<Test>]
    member _.``List.map external maps values`` () =
        let script = "[0..3] |> List.map (fun n -> n + 1)"
        match Helpers.evalWithExterns externs script with
        | VList [ VInt 1L; VInt 2L; VInt 3L; VInt 4L ] -> ()
        | _ -> Assert.Fail("Expected mapped int list")

    [<Test>]
    member _.``List.empty behaves as a value and cannot be invoked`` () =
        match Helpers.evalWithExterns externs "List.empty |> List.length" with
        | VInt 0L -> ()
        | _ -> Assert.Fail("Expected empty list length 0")

        let act () = Helpers.evalWithExterns externs "List.empty ()" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``List.iter external applies function and returns unit`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let script = "[1;2] |> List.iter (fun n -> n |> fun x -> $\"{x}\" |> print)"
            let result = Helpers.evalWithExterns externs script
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().Replace("\r\n", "\n").TrimEnd() |> should equal "1\n2"
        finally
            Console.SetOut(original)

    [<Test>]
    member _.``List.tryHead returns option`` () =
        match Helpers.evalWithExterns externs "[1;2] |> List.tryHead" with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected Some 1")

        match Helpers.evalWithExterns externs "[] |> List.tryHead" with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")

    [<Test>]
    member _.``List.tail returns tail and errors on empty list`` () =
        match Helpers.evalWithExterns externs "[1;2;3] |> List.tail" with
        | VList [ VInt 2L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected [2;3]")

        let act () = Helpers.evalWithExterns externs "[] |> List.tail" |> ignore
        act |> should throw typeof<EvalException>

    [<Test>]
    member _.``List.append concatenates lists`` () =
        match Helpers.evalWithExterns externs "List.append [1;2] [3]" with
        | VList [ VInt 1L; VInt 2L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected concatenated list")

    [<Test>]
    member _.``List externs support choose collect contains distinct exists fold rev tryFind tryGet and filter`` () =
        match Helpers.evalWithExterns externs "[1;2;3;4] |> List.choose (fun x -> if x % 2 = 0 then Some x else None)" with
        | VList [ VInt 2L; VInt 4L ] -> ()
        | _ -> Assert.Fail("Expected choose of even elements")

        match Helpers.evalWithExterns externs "[1;2;3] |> List.collect (fun x -> [x; x + 10])" with
        | VList [ VInt 1L; VInt 11L; VInt 2L; VInt 12L; VInt 3L; VInt 13L ] -> ()
        | _ -> Assert.Fail("Expected collected list")

        match Helpers.evalWithExterns externs "List.contains 2 [1;2;3]" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected contains true")

        match Helpers.evalWithExterns externs "[1;2;1;3;2] |> List.distinct" with
        | VList [ VInt 1L; VInt 2L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected distinct values preserving order")

        match Helpers.evalWithExterns externs "List.exists (fun x -> x > 2) [1;2;3]" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected exists true")

        match Helpers.evalWithExterns externs "List.fold (fun s -> fun x -> s + x) 0 [1;2;3]" with
        | VInt 6L -> ()
        | _ -> Assert.Fail("Expected fold sum")

        match Helpers.evalWithExterns externs "List.rev [1;2;3]" with
        | VList [ VInt 3L; VInt 2L; VInt 1L ] -> ()
        | _ -> Assert.Fail("Expected reversed list")

        match Helpers.evalWithExterns externs "List.length [1;2;3]" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected list length 3")

        match Helpers.evalWithExterns externs "List.tryFind (fun x -> x > 2) [1;2;3]" with
        | VOption (Some (VInt 3L)) -> ()
        | _ -> Assert.Fail("Expected tryFind Some 3")

        match Helpers.evalWithExterns externs "List.tryGet (fun x -> x = 2) [1;2;3]" with
        | VOption (Some (VInt 1L)) -> ()
        | _ -> Assert.Fail("Expected tryGet Some 1")

        match Helpers.evalWithExterns externs "List.filter (fun x -> x % 2 = 1) [1;2;3;4]" with
        | VList [ VInt 1L; VInt 3L ] -> ()
        | _ -> Assert.Fail("Expected filtered odd list")

    [<Test>]
    member _.``Option externs support mapping and defaults`` () =
        match Helpers.evalWithExterns externs "Option.defaultValue 9 None" with
        | VInt 9L -> ()
        | _ -> Assert.Fail("Expected default on None")

        match Helpers.evalWithExterns externs "Option.defaultValue 9 (Some 3)" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected Some value over default")

        match Helpers.evalWithExterns externs "Option.defaultWith (fun _ -> 11) None" with
        | VInt 11L -> ()
        | _ -> Assert.Fail("Expected computed default on None")

        match Helpers.evalWithExterns externs "Option.defaultWith (fun _ -> raise \"boom\") (Some 3)" with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected Option.defaultWith to skip fallback for Some")

        match Helpers.evalWithExterns externs "Option.isNone None" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected Option.isNone true")

        match Helpers.evalWithExterns externs "Option.isSome (Some 1)" with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected Option.isSome true")

        match Helpers.evalWithExterns externs "Option.map (fun x -> x + 1) (Some 1)" with
        | VOption (Some (VInt 2L)) -> ()
        | _ -> Assert.Fail("Expected mapped Some value")

        match Helpers.evalWithExterns externs "Option.map (fun x -> x + 1) None" with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None unchanged")

    [<Test>]
    member _.``Option.map infers unannotated lambda record field access`` () =
        let script =
            "type Package = { name: string; version: string }\n" +
            "let package = Some { name = \"fscript\"; version = \"0.7.0\" }\n" +
            "package |> Option.map (fun value -> value.name)"
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VString "fscript")) -> ()
        | _ -> Assert.Fail("Expected Some \"fscript\"")

    [<Test>]
    member _.``Json deserialize uses typeof record`` () =
        let script =
            "type Package = { Name: string; Version: string option; Deps: int map }\n" +
            "let json = \"{\\\"Name\\\":\\\"pkg\\\",\\\"Version\\\":null,\\\"Deps\\\":{\\\"a\\\":1}}\"\n" +
            "Json.deserialize (typeof Package) json"

        match Helpers.evalWithExterns externs script with
        | VOption (Some (VRecord fields)) ->
            fields.ContainsKey "Name" |> should equal true
        | _ -> Assert.Fail("Expected Some record")

    [<Test>]
    member _.``Json serialize returns string payload`` () =
        let script =
            "type Payload = { Name: string; Deps: string list }\n" +
            "Json.serialize { Name = \"pkg\"; Deps = [\"core\"; \"cli\"] }"
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VString json)) ->
            json.Contains("\"Name\":\"pkg\"") |> should equal true
            json.Contains("\"Deps\":[\"core\",\"cli\"]") |> should equal true
        | _ -> Assert.Fail("Expected Some serialized json")

    [<Test>]
    member _.``Xml deserialize and serialize work with record payloads`` () =
        let script =
            "type Item = { Name: string }\n" +
            "type Payload = { Item: Item }\n" +
            "let payload = Xml.serialize { Item = { Name = \"x\" } }\n" +
            "match payload with\n" +
            "| Some xml -> Xml.deserialize (typeof Item) xml \"Item\"\n" +
            "| None -> None"

        match Helpers.evalWithExterns externs script with
        | VOption (Some (VList [ VRecord fields ])) ->
            match fields.TryFind "Name" with
            | Some (VString "x") -> ()
            | _ -> Assert.Fail("Expected Name field with value x")
        | _ -> Assert.Fail("Expected XML roundtrip result")

    [<Test>]
    member _.``Regex groups external returns captures`` () =
        let script = "Regex.matchGroups \"^file:(.*)$\" \"file:foo\""
        match Helpers.evalWithExterns externs script with
        | VOption (Some (VList [ VString "foo" ])) -> ()
        | _ -> Assert.Fail("Expected group capture")

    [<Test>]
    member _.``Old flat extern names are no longer available`` () =
        let act () = Helpers.evalWithExterns externs "regex_match_groups \"^file:(.*)$\" \"file:foo\"" |> ignore
        act |> should throw typeof<TypeException>

    [<Test>]
    member _.``print external writes to console`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result = Helpers.evalWithExterns externs "print \"hello-core-test\""
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().TrimEnd() |> should equal "hello-core-test"
        finally
            Console.SetOut(original)

    [<Test>]
    member _.``for loop can call print external`` () =
        let original = Console.Out
        use writer = new StringWriter()
        Console.SetOut(writer)
        try
            let result = Helpers.evalWithExterns externs "for msg in [\"a\";\"b\"] do print msg"
            match result with
            | VUnit -> ()
            | _ -> Assert.Fail("Expected unit")
            writer.ToString().Replace("\r\n", "\n").TrimEnd() |> should equal "a\nb"
        finally
            Console.SetOut(original)
