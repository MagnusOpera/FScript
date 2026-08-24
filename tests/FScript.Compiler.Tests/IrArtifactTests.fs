namespace FScript.Compiler.Tests

open System
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type IrArtifactTests() =
    let compile source =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Ok value -> value
        | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))

    let parseObject (bytes: byte array) : JsonObject =
        match JsonNode.Parse(Encoding.UTF8.GetString(bytes)) with
        | null -> failwith "Expected JSON object"
        | node -> node.AsObject()

    [<Test>]
    member _.``IR artifact is deterministic and equivalent after round trip`` () =
        let compiled = compile "let add x y = x + y\n(Some (add 1 2), [1] @ [2])"
        let first = IrArtifact.serialize compiled.Lir
        let second = IrArtifact.serialize compiled.Lir
        Assert.That((first = second), Is.True)
        match IrArtifact.deserialize first with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok loaded -> Assert.That(loaded, Is.EqualTo(compiled.Lir))

    [<Test>]
    member _.``IR artifact round trips explicit tail calls`` () =
        let compiled = compile "let rec loop n = if n <= 0 then n else loop (n - 1)\nloop 10"
        let bytes = IrArtifact.serialize compiled.Lir
        Assert.That(Encoding.UTF8.GetString(bytes), Does.Contain("\"kind\": \"tail-call\""))
        match IrArtifact.deserialize bytes with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok loaded -> Assert.That(loaded, Is.EqualTo(compiled.Lir))

    [<Test>]
    member _.``IR artifact rejects incompatible versions`` () =
        let bytes = compile "1" |> _.Lir |> IrArtifact.serialize
        let invalid = Encoding.UTF8.GetString(bytes).Replace("\"irVersion\": 1", "\"irVersion\": 999") |> Encoding.UTF8.GetBytes
        match IrArtifact.deserialize invalid with
        | Ok _ -> Assert.Fail("Expected incompatible-version diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5002"))

    [<Test>]
    member _.``IR artifact rejects corrupt JSON`` () =
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes("{")) with
        | Ok _ -> Assert.Fail("Expected corrupt-artifact diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5003"))

    [<Test>]
    member _.``IR artifact rejects missing and non-canonical metadata`` () =
        let bytes = compile "1" |> _.Lir |> IrArtifact.serialize
        let missing = parseObject bytes
        missing.Remove("compilerVersion") |> ignore
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes(missing.ToJsonString())) with
        | Ok _ -> Assert.Fail("Expected missing-metadata diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5004"))

        let invalid = parseObject bytes
        let dependencies = JsonArray()
        dependencies.Add("z")
        dependencies.Add("a")
        invalid["dependencies"] <- dependencies
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes(invalid.ToJsonString())) with
        | Ok _ -> Assert.Fail("Expected non-canonical-metadata diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5005"))

        let invalidSource = parseObject bytes
        let sourceMetadata = JsonObject()
        sourceMetadata["path"] <- "z\\program.fss"
        sourceMetadata["sha256"] <- String.replicate 64 "A"
        invalidSource["sources"] <- JsonArray(sourceMetadata)
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes(invalidSource.ToJsonString())) with
        | Ok _ -> Assert.Fail("Expected invalid source-metadata diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5005"))

        let invalidVersion = parseObject bytes
        invalidVersion["compilerVersion"] <- ""
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes(invalidVersion.ToJsonString())) with
        | Ok _ -> Assert.Fail("Expected invalid version-metadata diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5005"))

    [<Test>]
    member _.``IR artifact post-load validation rejects unknown semantic types`` () =
        let artifact = compile "1" |> _.Lir |> IrArtifact.serialize |> parseObject
        let required (name: string) (value: JsonObject) : JsonNode =
            match value[name] with
            | null -> failwith $"Expected '{name}'"
            | node -> node
        let program = (required "program" artifact).AsObject()
        let entry = (required "entry" program).AsObject()
        let unknownType = JsonObject()
        unknownType["kind"] <- "named"
        unknownType["name"] <- "Missing"
        entry["type"] <- unknownType
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes(artifact.ToJsonString())) with
        | Ok _ -> Assert.Fail("Expected post-load semantic-type diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2017"))

    [<Test>]
    member _.``IR artifact records normalized source paths and SHA256 hashes`` () =
        let path = IO.Path.Combine(IO.Path.GetTempPath(), $"fscript-source-{Guid.NewGuid():N}.fss")
        try
            IO.File.WriteAllText(path, "1")
            let input = { Program = Parser.parseProgramWithSourceName (Some path) "1"; Externs = [] }
            let compiled = match Compiler.compile input with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
            use document = JsonDocument.Parse(IrArtifact.serialize compiled.Lir)
            let source = document.RootElement.GetProperty("sources")[0]
            Assert.That(source.GetProperty("path").GetString(), Is.EqualTo(path.Replace('\\', '/')))
            let expected = Security.Cryptography.SHA256.HashData(IO.File.ReadAllBytes(path)) |> Convert.ToHexString |> _.ToLowerInvariant()
            Assert.That(source.GetProperty("sha256").GetString(), Is.EqualTo(expected))
        finally
            if IO.File.Exists path then IO.File.Delete path

    [<Test>]
    member _.``IR artifact round trips lexical let and fail nodes`` () =
        for source in
            [ "let f x = (let y = x + 1\n    y\n)\nf 2"
              "raise \"failure\""
              "match Some 2 with\n    | Some x when x > 0 -> x\n    | None -> 0"
              "([1..3][0], $\"answer {42}\")"
              "let value = {| Count = 1 |}\n{| value with Count = 2 |}"
              "type Shape = | Point | Circle of int\nmatch Circle 2 with\n    | Point -> 0\n    | Circle radius -> radius"
              "{ [\"answer\"] = 42 }[\"answer\"]"
              "let sum pair = (let (left, right) = pair\n    left + right\n)\nsum (1, 2)"
              "for value in [1; 2; 3] do ignore value"
              "type Package = { Name: string }\ntypeof Package"
              "(let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 10\n)"
              "match { [\"a\"] = 1 } with\n    | { [key] = value; ..tail } -> value\n    | {} -> 0"
              "match {| Name = \"Ada\" |} with\n    | {| Name: string |} -> true\n    | _ -> false"
              "List.length Env.Arguments"
              "[\"a\"; \"b\"] |> String.concat \",\""
              "[<Import(\"strings\", \"ToUpper\")>] let upper : string -> string\nupper \"hello\"" ] do
            let bytes = compile source |> _.Lir |> IrArtifact.serialize
            match IrArtifact.deserialize bytes with
            | Ok _ -> ()
            | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``IR artifact serializes lowered pattern plans rather than source patterns`` () =
        let bytes = compile "match Some (1, 2) with | Some (left, right) -> left + right | None -> 0" |> _.Lir |> IrArtifact.serialize
        let json = Encoding.UTF8.GetString(bytes)
        Assert.That(json, Does.Contain("\"plan\""))
        Assert.That(json, Does.Contain("\"projection\""))
        Assert.That(json, Does.Not.Contain("\"pattern\""))

    [<Test>]
    member _.``IR artifact preserves explicit structural record conversions`` () =
        let source = "type Person = { Name: string; Age: int }\nlet age (person: Person) = person.Age\nage {| Name = \"Ada\"; Age = 42 |}"
        let lir = (compile source).Lir
        let bytes = IrArtifact.serialize lir
        Assert.That(Encoding.UTF8.GetString(bytes), Does.Contain("\"kind\": \"record-convert\""))
        match IrArtifact.deserialize bytes with
        | Ok loaded -> Assert.That(loaded, Is.EqualTo(lir))
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``IR artifact preserves named type declarations`` () =
        let lir = (compile "type Person = { Name: string; Age: int }\n{ Name = \"Ada\"; Age = 42 }").Lir
        let bytes = IrArtifact.serialize lir
        Assert.That(Encoding.UTF8.GetString(bytes), Does.Contain("\"types\""))
        match IrArtifact.deserialize bytes with
        | Ok loaded -> Assert.That(loaded.Types = lir.Types, Is.True)
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``LIR closures serialize deterministic explicit captures`` () =
        let source = "let outer x = (let inner y = x + y\n    inner\n)\nouter 40 2"
        let lir = (compile source).Lir
        Assert.That((lir.Functions |> List.map (fun fn -> Lir.functionNumber fn.Id)) = [ 0; 1 ], Is.True)
        Assert.That(lir.Functions |> List.exists (fun fn -> not fn.Captures.IsEmpty), Is.True)
        let bytes = lir |> IrArtifact.serialize
        let json = Encoding.UTF8.GetString(bytes)
        Assert.That(json, Does.Match("\\\"captures\\\": \\[\\s+[0-9]+"))
        match IrArtifact.deserialize bytes with
        | Ok _ -> ()
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``IR artifact validation rejects forged closure captures`` () =
        let source = "let outer x = (let inner y = x + y\n    inner\n)\nouter 40 2"
        let json = (compile source).Lir |> IrArtifact.serialize |> Encoding.UTF8.GetString
        let capturePattern = System.Text.RegularExpressions.Regex("\\\"captures\\\": \\[\\s+[0-9]+\\s+\\]")
        let invalid = capturePattern.Replace(json, "\"captures\": [ ]", 1) |> Encoding.UTF8.GetBytes
        match IrArtifact.deserialize invalid with
        | Ok _ -> Assert.Fail("Expected invalid closure-capture diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSL2013"))

    [<Test>]
    member _.``IR artifact preserves structured export metadata`` () =
        let lir = (compile "[<export>] let answer = 42").Lir
        let exported = lir.Declarations |> List.find _.IsExported
        Assert.That(exported.Export |> Option.map _.Name, Is.EqualTo(Some "answer"))
        match lir |> IrArtifact.serialize |> IrArtifact.deserialize with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok loaded ->
            let roundTripped = loaded.Declarations |> List.find _.IsExported
            Assert.That(roundTripped.Export, Is.EqualTo(exported.Export))
