namespace FScript.Runtime.Tests

open System
open System.IO
open FsCheck
open FsCheck.FSharp
open FScript.Language
open FScript.Runtime
open NUnit.Framework
open RuntimePropertySupport

module private FsModel =
    type Operation = Write of int * int * string | Read of int * int | Probe of int * int | Create of int | Scan of int * bool
    let folder = function 0 -> "." | n -> $"dir{n}"
    let file directory index =
        let name = $"file{index}" + (if index % 2 = 0 then ".txt" else ".md")
        if directory = 0 then name else folder directory + "/" + name
    let generator = Gen.sized (fun size -> gen {
        let! count = Gen.choose(0, min 16 size)
        let operation = gen {
            let! directory = Gen.choose(0, 2)
            let! index = Gen.choose(0, 3)
            let! text = text
            let! kind = Gen.choose(0, 4)
            let! recursive = Gen.elements [false; true]
            return match kind with 0 -> Write(directory, index, text) | 1 -> Read(directory, index) | 2 -> Probe(directory, index) | 3 -> Create directory | _ -> Scan(directory, recursive)
        }
        return! Gen.listOfLength count operation
    })
    let shrinkOperation = function Write(d, i, text) -> shrinkText text |> Seq.map (fun text -> Write(d, i, text)) | _ -> Seq.empty
    let arbitrary = Arb.fromGenShrink(generator, shrinkList shrinkOperation)

[<TestFixture>]
type FsPropertyTests() =
    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.OperationSequencesMatchFileModel(seed: uint64) =
        verify "FsPropertyTests.OperationSequencesMatchFileModel" 75 FsModel.arbitrary seed (fun operations ->
            HostTestHelpers.withTempRoot "fscript-fs-properties" (fun root ->
                let context = { HostContext.create root with Permissions = HostPermissions.IO }
                let mutable files: Map<string, string> = Map.empty
                let mutable directories = Set.singleton "."
                let strings = function
                    | VOption(Some(VList values)) ->
                        let values = values |> List.map (function VString s -> s | _ -> failwith "Expected a file path")
                        equal values.Length (Set.ofList values).Count
                        Set.ofList values
                    | actual -> failwithf "Expected an enumeration: %A" actual
                // Ensure even the size-zero case exercises actual read/write IO.
                for operation in FsModel.Write(0, 0, "initial") :: operations do
                    match operation with
                    | FsModel.Write(directory, index, content) ->
                        let path = FsModel.file directory index
                        value (VBool true) (invoke (FsExterns.write_text context) [VString path; VString content])
                        files <- files.Add(path, content)
                        directories <- directories.Add(FsModel.folder directory)
                    | FsModel.Read(directory, index) ->
                        let path = FsModel.file directory index
                        value (VOption(files.TryFind path |> Option.map VString)) (invoke (FsExterns.read_text context) [VString path])
                    | FsModel.Probe(directory, index) ->
                        let path = FsModel.file directory index
                        value (VBool(files.ContainsKey path)) (invoke (FsExterns.exists context) [VString path])
                        let kind = if files.ContainsKey path then VUnionCase("FsKind", "File", Some(VString path)) else VUnionCase("FsKind", "Missing", None)
                        value kind (invoke (FsExterns.entry_kind context) [VString path])
                    | FsModel.Create directory ->
                        let path = FsModel.folder directory
                        value (VBool true) (invoke (FsExterns.create_directory context) [VString path])
                        value (VUnionCase("FsKind", "Directory", Some(VString path))) (invoke (FsExterns.entry_kind context) [VString path])
                        directories <- directories.Add path
                    | FsModel.Scan(directory, recursive) ->
                        let path = FsModel.folder directory
                        let pattern = if recursive then "**/*.txt" else "*.txt"
                        let result = invoke (FsExterns.enumerate_files context) [VString path; VString pattern]
                        if not (directories.Contains path) then value (VOption None) result
                        else
                            let expected =
                                files |> Map.keys |> Seq.filter (fun name ->
                                    let location =
                                        if directory = 0 then recursive || not (name.Contains '/')
                                        else name.StartsWith(path + "/", StringComparison.Ordinal)
                                    name.EndsWith(".txt", StringComparison.Ordinal) && location)
                                |> Set.ofSeq
                            equal expected (strings result)
                equal (files |> Map.keys |> Set.ofSeq) (invoke (FsExterns.glob context) [VString "**/*"] |> strings)
                for KeyValue(path, content) in files do value (some(VString content)) (invoke (FsExterns.read_text context) [VString path])))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.SandboxAndDeniedPathsStayOutsideTheFileModel(seed: uint64) =
        verify "FsPropertyTests.SandboxAndDeniedPathsStayOutsideTheFileModel" 75 inputs seed (fun input ->
            HostTestHelpers.withTempRoot "fscript-fs-boundaries" (fun parent ->
                let root = Path.Combine(parent, "root")
                Directory.CreateDirectory(Path.Combine(root, "blocked")) |> ignore
                let outside = Path.Combine(parent, "outside.txt")
                File.WriteAllText(outside, "outside-sentinel")
                File.WriteAllText(Path.Combine(root, "blocked", "secret.txt"), "denied-sentinel")
                let context = { HostContext.create root with Permissions = HostPermissions.IO; DeniedPathGlobs = ["blocked"] }
                for path in ["../outside.txt"; "child/../../outside.txt"; outside] do
                    value (VOption None) (invoke (FsExterns.read_text context) [VString path])
                    value (VBool false) (invoke (FsExterns.exists context) [VString path])
                    value (VBool false) (invoke (FsExterns.write_text context) [VString path; VString input.Text])
                    value (VUnionCase("FsKind", "Missing", None)) (invoke (FsExterns.entry_kind context) [VString path])
                value (VBool false) (invoke (FsExterns.create_directory context) [VString "../new-dir"])
                equal false (Directory.Exists(Path.Combine(parent, "new-dir")))
                equal "outside-sentinel" (File.ReadAllText outside)
                for fn, args in [FsExterns.read_text context, [VString "blocked/secret.txt"]; FsExterns.write_text context, [VString "blocked/secret.txt"; VString input.Text]; FsExterns.create_directory context, [VString "blocked/new"]] do
                    raises<EvalException> (fun () -> invoke fn args |> ignore) |> ignore
                equal "denied-sentinel" (File.ReadAllText(Path.Combine(root, "blocked", "secret.txt")))
                value (VBool false) (invoke (FsExterns.exists context) [VString "blocked/secret.txt"])
                value (VUnionCase("FsKind", "Missing", None)) (invoke (FsExterns.entry_kind context) [VString "blocked"])
                value (some(VList [])) (invoke (FsExterns.glob context) [VString "**/*"])))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.LexicalPathHelpersPreserveComponents(seed: uint64) =
        verify "FsPropertyTests.LexicalPathHelpersPreserveComponents" 100 inputs seed (fun input ->
            let directory = $"dir{abs input.Number}"
            let stem = $"item{input.Bytes.Length}"
            let extension = if input.Number % 2 = 0 then ".txt" else ".data"
            let file = stem + extension
            let path = directory + "/" + file
            value (VString path) (invoke FsExterns.combine_path [VString directory; VString file])
            value (some(VString directory)) (invoke FsExterns.parent_directory [VString path])
            value (some(VString extension)) (invoke FsExterns.extension [VString path])
            value (VString stem) (invoke FsExterns.file_name_without_extension [VString path]))
