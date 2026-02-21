namespace FScript.Runtime.Tests

open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type FsExternsTests () =
    [<Test>]
    member _.``fs_read_text reads files under root`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let file = Path.Combine(root, "a.txt")
            File.WriteAllText(file, "hello")
            let ext = FsExterns.read_text { RootDirectory = root; ExcludedPaths = [] }
            match invoke ext [ VString "a.txt" ] with
            | VOption (Some (VString "hello")) -> ()
            | _ -> Assert.Fail("Expected Some \"hello\""))

    [<Test>]
    member _.``fs_read_text rejects parent path escape`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let ext = FsExterns.read_text { RootDirectory = root; ExcludedPaths = [] }
            match invoke ext [ VString "../outside.txt" ] with
            | VOption None -> ()
            | _ -> Assert.Fail("Expected None for escaped path"))

    [<Test>]
    member _.``fs_exists and fs_kind respect root boundary`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let dir = Path.Combine(root, "sub")
            let file = Path.Combine(dir, "a.txt")
            Directory.CreateDirectory(dir) |> ignore
            File.WriteAllText(file, "a")
            let exists = FsExterns.exists { RootDirectory = root; ExcludedPaths = [] }
            let kind = FsExterns.entry_kind { RootDirectory = root; ExcludedPaths = [] }

            match invoke exists [ VString "sub/a.txt" ] with
            | VBool true -> ()
            | _ -> Assert.Fail("Expected Fs.exists true")
            match invoke kind [ VString "sub/a.txt" ] with
            | VUnionCase("FsKind", "File", Some (VString "sub/a.txt")) -> ()
            | _ -> Assert.Fail("Expected Fs.kind File")
            match invoke kind [ VString "sub" ] with
            | VUnionCase("FsKind", "Directory", Some (VString "sub")) -> ()
            | _ -> Assert.Fail("Expected Fs.kind Directory")
            match invoke exists [ VString "../outside.txt" ] with
            | VBool false -> ()
            | _ -> Assert.Fail("Expected Fs.exists false for escaped path"))

    [<Test>]
    member _.``fs_kind returns Missing for out of root and missing path`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let kind = FsExterns.entry_kind { RootDirectory = root; ExcludedPaths = [] }
            match invoke kind [ VString "../outside.txt" ] with
            | VUnionCase("FsKind", "Missing", None) -> ()
            | _ -> Assert.Fail("Expected Fs.kind Missing for escaped path")
            match invoke kind [ VString "does-not-exist.txt" ] with
            | VUnionCase("FsKind", "Missing", None) -> ()
            | _ -> Assert.Fail("Expected Fs.kind Missing for missing path"))

    [<Test>]
    member _.``fs_create_directory and fs_write_text create files in root only`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let createDirectory = FsExterns.create_directory { RootDirectory = root; ExcludedPaths = [] }
            let writeText = FsExterns.write_text { RootDirectory = root; ExcludedPaths = [] }
            let readText = FsExterns.read_text { RootDirectory = root; ExcludedPaths = [] }

            match invoke createDirectory [ VString "new/sub" ] with
            | VBool true -> ()
            | _ -> Assert.Fail("Expected Fs.createDirectory true")

            match invoke writeText [ VString "new/sub/file.txt"; VString "hello" ] with
            | VBool true -> ()
            | _ -> Assert.Fail("Expected Fs.writeText true")

            match invoke readText [ VString "new/sub/file.txt" ] with
            | VOption (Some (VString "hello")) -> ()
            | _ -> Assert.Fail("Expected written file content")

            match invoke writeText [ VString "../outside.txt"; VString "nope" ] with
            | VBool false -> ()
            | _ -> Assert.Fail("Expected Fs.writeText false for escaped path"))

    [<Test>]
    member _.``excluded read write and create_directory fail immediately`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let blocked = Path.Combine(root, ".git")
            Directory.CreateDirectory(blocked) |> ignore
            File.WriteAllText(Path.Combine(blocked, "config"), "x")
            let context = { RootDirectory = root; ExcludedPaths = [ blocked ] }

            let readText = FsExterns.read_text context
            let writeText = FsExterns.write_text context
            let createDirectory = FsExterns.create_directory context

            let readAct () = invoke readText [ VString ".git/config" ] |> ignore
            let writeAct () = invoke writeText [ VString ".git/config"; VString "y" ] |> ignore
            let createAct () = invoke createDirectory [ VString ".git/new" ] |> ignore

            Assert.Throws<EvalException>(TestDelegate readAct) |> ignore
            Assert.Throws<EvalException>(TestDelegate writeAct) |> ignore
            Assert.Throws<EvalException>(TestDelegate createAct) |> ignore)

    [<Test>]
    member _.``excluded entries are hidden from probes and enumeration`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let blocked = Path.Combine(root, ".git")
            let visible = Path.Combine(root, "src")
            Directory.CreateDirectory(blocked) |> ignore
            Directory.CreateDirectory(visible) |> ignore
            File.WriteAllText(Path.Combine(blocked, "HEAD"), "ref: heads/main")
            File.WriteAllText(Path.Combine(visible, "app.fs"), "let x = 1")

            let context = { RootDirectory = root; ExcludedPaths = [ blocked ] }
            let exists = FsExterns.exists context
            let kind = FsExterns.entry_kind context
            let glob = FsExterns.glob context
            let enumerateFiles = FsExterns.enumerate_files context

            match invoke exists [ VString ".git/HEAD" ] with
            | VBool false -> ()
            | _ -> Assert.Fail("Expected Fs.exists false for excluded file")

            match invoke kind [ VString ".git" ] with
            | VUnionCase("FsKind", "Missing", None) -> ()
            | _ -> Assert.Fail("Expected Fs.kind Missing for excluded directory")

            match invoke glob [ VString "**/*" ] with
            | VOption (Some (VList values)) ->
                let asStrings =
                    values
                    |> List.choose (function | VString value -> Some value | _ -> None)
                Assert.That(asStrings, Does.Contain("src/app.fs"))
                Assert.That(asStrings, Does.Not.Contain(".git/HEAD"))
            | _ -> Assert.Fail("Expected Fs.glob list")

            match invoke enumerateFiles [ VString "."; VString "**/*" ] with
            | VOption (Some (VList values)) ->
                let asStrings =
                    values
                    |> List.choose (function | VString value -> Some value | _ -> None)
                Assert.That(asStrings, Does.Contain("src/app.fs"))
                Assert.That(asStrings, Does.Not.Contain(".git/HEAD"))
            | _ -> Assert.Fail("Expected Fs.enumerateFiles list"))

    [<Test>]
    member _.``excluded paths outside root are ignored`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let file = Path.Combine(root, "a.txt")
            File.WriteAllText(file, "hello")
            let outside = Path.Combine(Path.GetTempPath(), "outside-fscript-blacklist")
            let readText = FsExterns.read_text { RootDirectory = root; ExcludedPaths = [ outside ] }
            match invoke readText [ VString "a.txt" ] with
            | VOption (Some (VString "hello")) -> ()
            | _ -> Assert.Fail("Expected outside exclusion to be ignored"))

    [<Test>]
    member _.``fs_glob filters by pattern`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            Directory.CreateDirectory(Path.Combine(root, "sub")) |> ignore
            File.WriteAllText(Path.Combine(root, "a.txt"), "a")
            File.WriteAllText(Path.Combine(root, "sub", "b.txt"), "b")
            File.WriteAllText(Path.Combine(root, "sub", "c.md"), "c")
            let ext = FsExterns.glob { RootDirectory = root; ExcludedPaths = [] }
            match invoke ext [ VString "sub/*.txt" ] with
            | VOption (Some (VList [ VString "sub/b.txt" ])) -> ()
            | _ -> Assert.Fail("Expected only sub/b.txt"))

    [<Test>]
    member _.``fs_combine_path combines path segments`` () =
        let ext = FsExterns.combine_path
        match invoke ext [ VString "a"; VString "b.txt" ] with
        | VString "a/b.txt" -> ()
        | _ -> Assert.Fail("Expected combined path")

    [<Test>]
    member _.``fs_parent_directory returns parent when present`` () =
        let ext = FsExterns.parent_directory
        match invoke ext [ VString "dir/file.txt" ] with
        | VOption (Some (VString "dir")) -> ()
        | _ -> Assert.Fail("Expected Some parent directory")

    [<Test>]
    member _.``fs_extension and fs_file_name_without_extension extract path parts`` () =
        let extension = FsExterns.extension
        let filename = FsExterns.file_name_without_extension
        match invoke extension [ VString "dir/file.txt" ] with
        | VOption (Some (VString ".txt")) -> ()
        | _ -> Assert.Fail("Expected Some extension")
        match invoke filename [ VString "dir/file.txt" ] with
        | VString "file" -> ()
        | _ -> Assert.Fail("Expected file name without extension")

    [<Test>]
    member _.``fs_enumerate_files respects root and pattern`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            Directory.CreateDirectory(Path.Combine(root, "sub")) |> ignore
            File.WriteAllText(Path.Combine(root, "sub", "a.txt"), "a")
            File.WriteAllText(Path.Combine(root, "sub", "b.md"), "b")
            let ext = FsExterns.enumerate_files { RootDirectory = root; ExcludedPaths = [] }
            match invoke ext [ VString "sub"; VString "*.txt" ] with
            | VOption (Some (VList [ VString "sub/a.txt" ])) -> ()
            | _ -> Assert.Fail("Expected only sub/a.txt"))
