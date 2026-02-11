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
    member _.``Include resolver expands includes once`` () =
        withTempDir (fun dir ->
            let commonPath = Path.Combine(dir, "common.fss")
            let aPath = Path.Combine(dir, "a.fss")
            let bPath = Path.Combine(dir, "b.fss")
            let mainPath = Path.Combine(dir, "main.fss")

            File.WriteAllText(commonPath, "let common = 40")
            File.WriteAllText(aPath, "#include \"common.fss\"\nlet a = 1")
            File.WriteAllText(bPath, "#include \"common.fss\"\nlet b = 2")
            File.WriteAllText(mainPath, "#include \"a.fss\"\n#include \"b.fss\"\nlet total = common + a + b")

            let program = IncludeResolver.parseProgramFromFile dir mainPath
            let lets =
                program
                |> List.choose (function | SLet(name, _, _, _, _, _) -> Some name | _ -> None)
            lets |> should equal [ "common"; "a"; "b"; "total" ])

    [<Test>]
    member _.``Include resolver detects include cycles`` () =
        withTempDir (fun dir ->
            let aPath = Path.Combine(dir, "a.fss")
            let bPath = Path.Combine(dir, "b.fss")
            let mainPath = Path.Combine(dir, "main.fss")

            File.WriteAllText(aPath, "#include \"b.fss\"\nlet a = 1")
            File.WriteAllText(bPath, "#include \"a.fss\"\nlet b = 1")
            File.WriteAllText(mainPath, "#include \"a.fss\"\nlet x = 1")

            let mutable err : ParseError option = None
            try
                IncludeResolver.parseProgramFromFile dir mainPath |> ignore
                Assert.Fail("Expected include cycle parse failure")
            with
            | ParseException parseError ->
                err <- Some parseError
            match err with
            | Some parseError -> parseError.Message |> should contain "Include cycle detected"
            | None -> Assert.Fail("Expected ParseException"))

    [<Test>]
    member _.``Include resolver blocks paths outside root`` () =
        withTempDir (fun dir ->
            let externalFile = Path.Combine(Path.GetTempPath(), $"fscript-include-external-{Guid.NewGuid():N}.fss")
            try
                File.WriteAllText(externalFile, "let outside = 1")
                let mainPath = Path.Combine(dir, "main.fss")
                File.WriteAllText(mainPath, "#include \"../" + Path.GetFileName(externalFile) + "\"\nlet x = 1")
                let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
                act |> should throw typeof<ParseException>
            finally
                if File.Exists(externalFile) then
                    File.Delete(externalFile))

    [<Test>]
    member _.``Include resolver rejects non fss extension`` () =
        withTempDir (fun dir ->
            let txtPath = Path.Combine(dir, "shared.txt")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(txtPath, "let x = 1")
            File.WriteAllText(mainPath, "#include \"shared.txt\"\nlet y = 1")
            let act () = IncludeResolver.parseProgramFromFile dir mainPath |> ignore
            act |> should throw typeof<ParseException>)

    [<Test>]
    member _.``Include parse errors report offending file`` () =
        withTempDir (fun dir ->
            let badPath = Path.Combine(dir, "bad.fss")
            let mainPath = Path.Combine(dir, "main.fss")
            File.WriteAllText(badPath, "let = 1")
            File.WriteAllText(mainPath, "#include \"bad.fss\"\nlet x = 1")

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
