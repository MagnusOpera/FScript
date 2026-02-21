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
            let ext = FsExterns.read_text { RootDirectory = root }
            match invoke ext [ VString "a.txt" ] with
            | VOption (Some (VString "hello")) -> ()
            | _ -> Assert.Fail("Expected Some \"hello\""))

    [<Test>]
    member _.``fs_read_text rejects parent path escape`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let ext = FsExterns.read_text { RootDirectory = root }
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
            let exists = FsExterns.exists { RootDirectory = root }
            let kind = FsExterns.entry_kind { RootDirectory = root }

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
            let kind = FsExterns.entry_kind { RootDirectory = root }
            match invoke kind [ VString "../outside.txt" ] with
            | VUnionCase("FsKind", "Missing", None) -> ()
            | _ -> Assert.Fail("Expected Fs.kind Missing for escaped path")
            match invoke kind [ VString "does-not-exist.txt" ] with
            | VUnionCase("FsKind", "Missing", None) -> ()
            | _ -> Assert.Fail("Expected Fs.kind Missing for missing path"))

    [<Test>]
    member _.``fs_create_directory and fs_write_text create files in root only`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            let createDirectory = FsExterns.create_directory { RootDirectory = root }
            let writeText = FsExterns.write_text { RootDirectory = root }
            let readText = FsExterns.read_text { RootDirectory = root }

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
    member _.``fs_glob filters by pattern`` () =
        withTempRoot "fscript-host-tests" (fun root ->
            Directory.CreateDirectory(Path.Combine(root, "sub")) |> ignore
            File.WriteAllText(Path.Combine(root, "a.txt"), "a")
            File.WriteAllText(Path.Combine(root, "sub", "b.txt"), "b")
            File.WriteAllText(Path.Combine(root, "sub", "c.md"), "c")
            let ext = FsExterns.glob { RootDirectory = root }
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
            let ext = FsExterns.enumerate_files { RootDirectory = root }
            match invoke ext [ VString "sub"; VString "*.txt" ] with
            | VOption (Some (VList [ VString "sub/a.txt" ])) -> ()
            | _ -> Assert.Fail("Expected only sub/a.txt"))
