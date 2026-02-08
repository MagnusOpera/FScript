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
