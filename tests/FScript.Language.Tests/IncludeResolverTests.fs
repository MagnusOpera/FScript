namespace FScript.Language.Tests

open System
open System.IO
open NUnit.Framework
open FsUnit
open FScript.Language

[<TestFixture>]
type IncludeResolverTests () =
    let withTempDir (test: string -> unit) =
        let path = Path.Combine(Path.GetTempPath(), $"fscript-include-tests-{Guid.NewGuid():N}")
        Directory.CreateDirectory(path) |> ignore
        try
            test path
        finally
            if Directory.Exists(path) then
                Directory.Delete(path, true)

    [<Test>]
    member _.``Import resolver expands imports once`` () =
        withTempDir (fun dir ->
            let commonPath = Path.Combine(dir, "common.fss")
            let aPath = Path.Combine(dir, "a.fss")
            let bPath = Path.Combine(dir, "b.fss")
            let mainPath = Path.Combine(dir, "main.fss")

            File.WriteAllText(commonPath, "let common = 40")
            File.WriteAllText(aPath, "import \"common.fss\" as Common\nlet a = 1")
            File.WriteAllText(bPath, "import \"common.fss\" as Common\nlet b = 2")
            File.WriteAllText(mainPath, "import \"a.fss\" as A\nimport \"b.fss\" as B\nlet total = A.a + B.b")

            let program = IncludeResolver.parseProgramFromFile dir mainPath
            let lets =
                program
                |> List.choose (function | SLet(name, _, _, _, _, _) -> Some name | _ -> None)
            lets |> should contain "total"
            lets |> List.filter (fun name -> name.EndsWith(".common", StringComparison.Ordinal)) |> List.length |> should equal 1)

    [<Test>]
    member _.``Import resolver detects import cycles`` () =
        withTempDir (fun dir ->
            let aPath = Path.Combine(dir, "a.fss")
            let bPath = Path.Combine(dir, "b.fss")
            let mainPath = Path.Combine(dir, "main.fss")

            File.WriteAllText(aPath, "import \"b.fss\" as B\nlet a = 1")
            File.WriteAllText(bPath, "import \"a.fss\" as A\nlet b = 1")
            File.WriteAllText(mainPath, "import \"a.fss\" as A\nlet x = 1")

            let mutable err : ParseError option = None
            try
                IncludeResolver.parseProgramFromFile dir mainPath |> ignore
                Assert.Fail("Expected import cycle parse failure")
            with
            | ParseException parseError ->
                err <- Some parseError
            match err with
            | Some parseError -> parseError.Message |> should contain "Import cycle detected"
            | None -> Assert.Fail("Expected ParseException"))

    [<Test>]
    member _.``Import resolver blocks paths outside root`` () =
        withTempDir (fun dir ->
            let externalFile = Path.Combine(Path.GetTempPath(), $"fscript-include-external-{Guid.NewGuid():N}.fss")
            try
                File.WriteAllText(externalFile, "let outside = 1")
                let mainPath = Path.Combine(dir, "main.fss")
                File.WriteAllText(mainPath, "import \"../" + Path.GetFileName(externalFile) + "\" as Outside\nlet x = 1")
                let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
                act |> should throw typeof<ParseException>
            finally
                if File.Exists(externalFile) then
                    File.Delete(externalFile))

    [<Test>]
    member _.``Import resolver rejects non fss extension`` () =
        withTempDir (fun dir ->
            let txtPath = Path.Combine(dir, "shared.txt")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(txtPath, "let x = 1")
            File.WriteAllText(mainPath, "import \"shared.txt\" as Shared\nlet y = 1")
            let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
            act |> should throw typeof<ParseException>)

    [<Test>]
    member _.``Import parse errors report offending file`` () =
        withTempDir (fun dir ->
            let badPath = Path.Combine(dir, "bad.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(badPath, "let = 1")
            File.WriteAllText(mainPath, "import \"bad.fss\" as Bad\nlet x = 1")

            let mutable err : ParseError option = None
            try
                IncludeResolver.parseProgramFromFile dir mainPath |> ignore
                Assert.Fail("Expected parse failure from included file")
            with
            | ParseException parseError ->
                err <- Some parseError
            match err with
            | Some parseError ->
                parseError.Span.Start.File |> should equal (Some (Path.GetFullPath badPath))
            | None -> Assert.Fail("Expected ParseException"))

    [<Test>]
    member _.``Import resolver rejects module declarations in imported files`` () =
        withTempDir (fun dir ->
            let helperPath = Path.Combine(dir, "helper.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(helperPath, "module Root\nlet x = 1")
            File.WriteAllText(mainPath, "import \"helper.fss\" as Helper\nlet ok = 1")
            let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
            act |> should throw typeof<ParseException>)

    [<Test>]
    member _.``Import resolver uses explicit alias namespace`` () =
        withTempDir (fun dir ->
            let helperPath = Path.Combine(dir, "helper.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(helperPath, "let add1 x = x + 1")
            File.WriteAllText(mainPath, "import \"helper.fss\" as H\nlet value = H.add1 41")
            let program = IncludeResolver.parseProgramFromFile dir mainPath
            let names =
                program
                |> List.choose (function
                    | SLet(name, _, _, _, _, _) -> Some name
                    | _ -> None)
            names |> List.exists (fun name -> name.EndsWith(".add1", StringComparison.Ordinal)) |> should equal true
            names |> should contain "value")

    [<Test>]
    member _.``Import resolver enforces import then code ordering`` () =
        withTempDir (fun dir ->
            let helperPath = Path.Combine(dir, "helper.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(helperPath, "let x = 1\nimport \"other.fss\" as Other")
            File.WriteAllText(mainPath, "import \"helper.fss\" as Helper\nlet ok = 1")
            let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
            act |> should throw typeof<ParseException>)

    [<Test>]
    member _.``Import resolver can parse in-memory main source with on-disk imports`` () =
        withTempDir (fun dir ->
            let helperPath = Path.Combine(dir, "_protocol.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(helperPath, "type ActionContext = { Name: string }\n")

            let source =
                "import \"_protocol.fss\" as Proto\n"
                + "let defaults (context: Proto.ActionContext) = context.Name\n"

            let program = IncludeResolver.parseProgramFromSourceWithIncludes dir mainPath source
            let letNames =
                program
                |> List.choose (function
                    | SLet(name, _, _, _, _, _) -> Some name
                    | _ -> None)

            letNames |> should contain "defaults")

    [<Test>]
    member _.``Import resolver rejects invalid alias`` () =
        withTempDir (fun dir ->
            let helperPath = Path.Combine(dir, "bad-name.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(helperPath, "let add1 x = x + 1")
            File.WriteAllText(mainPath, "import \"bad-name.fss\" as 1Bad\nlet value = 1")
            let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
            act |> should throw typeof<ParseException>)

    [<Test>]
    member _.``Import resolver rejects duplicate aliases`` () =
        withTempDir (fun dir ->
            let leftDir = Path.Combine(dir, "left")
            let rightDir = Path.Combine(dir, "right")
            Directory.CreateDirectory(leftDir) |> ignore
            Directory.CreateDirectory(rightDir) |> ignore

            let helperLeft = Path.Combine(leftDir, "helper.fss")
            let helperRight = Path.Combine(rightDir, "helper.fss")
            let mainPath = Path.Combine(dir, "main.fss")

            File.WriteAllText(helperLeft, "let x = 1")
            File.WriteAllText(helperRight, "let y = 2")
            File.WriteAllText(mainPath, "import \"left/helper.fss\" as Helper\nimport \"right/helper.fss\" as Helper\nlet total = Helper.x")

            let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
            act |> should throw typeof<ParseException>)

    [<Test>]
    member _.``Import inference resolves qualified type annotation to imported type`` () =
        withTempDir (fun dir ->
            let commonPath = Path.Combine(dir, "common.fss")
            let mainPath = Path.Combine(dir, "main.fss")

            File.WriteAllText(commonPath, "type ProjectInfo = { Name: string; Language: string }\nlet describe_project (project: ProjectInfo) = project.Name")
            File.WriteAllText(mainPath, "import \"common.fss\" as Common\nlet summary (project: Common.ProjectInfo) = Common.describe_project project\nsummary { Name = \"Terrabuild\"; Language = \"F#\" }")

            let program = IncludeResolver.parseProgramFromFile dir mainPath
            let typed = TypeInfer.inferProgram program
            match typed |> List.last with
            | TypeInfer.TSExpr expr -> expr.Type |> should equal TString
            | _ -> Assert.Fail("Expected expression"))
