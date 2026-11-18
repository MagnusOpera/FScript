namespace FScript.Runtime

open System.IO
open System.Text.RegularExpressions
open FScript.Language

module FsExterns =
    let internal kindType : HostUnionDescriptor =
        { Name = "FsKind"
          Cases = Map.ofList ["File", Some TString; "Directory", Some TString; "Missing", None] }

    let private normalizeSeparators (path: string) =
        path.Replace("\\", "/")

    let private throwBlockedPathAccess (operationName: string) (fullPath: string) =
        raise (HostCommon.evalError $"Access denied for operation '{operationName}' on denied path '{fullPath}'")

    let private tryEnumerateFiles (directory: string) =
        try
            Directory.EnumerateFiles(directory) |> Seq.toList
        with _ ->
            []

    let private tryEnumerateDirectories (directory: string) =
        try
            Directory.EnumerateDirectories(directory) |> Seq.toList
        with _ ->
            []

    let private enumerateReachableFiles (ctx: FileSystemPolicy) (startDirectory: string) =
        let rec loop directory =
            seq {
                for file in tryEnumerateFiles directory do
                    let full = HostCommon.normalizeFullPath file
                    if FileSystemPolicy.isSafePath ctx full && not (FileSystemPolicy.isDeniedPath ctx full) then
                        yield full

                for childDirectory in tryEnumerateDirectories directory do
                    let full = HostCommon.normalizeFullPath childDirectory
                    if FileSystemPolicy.isSafePath ctx full && not (FileSystemPolicy.isDeniedPath ctx full) then
                        yield! loop full
            }

        loop startDirectory

    let private enumerateCandidateFiles (ctx: FileSystemPolicy) (startDirectory: string) (pattern: string) =
        let normalizedPattern = normalizeSeparators pattern
        if normalizedPattern.Contains("/") then
            enumerateReachableFiles ctx startDirectory
        else
            tryEnumerateFiles startDirectory
            |> Seq.choose (fun file ->
                let full = HostCommon.normalizeFullPath file
                if FileSystemPolicy.isSafePath ctx full && not (FileSystemPolicy.isDeniedPath ctx full) then
                    Some full
                else
                    None)

    let internal read_textWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.readText"
          Scheme = Forall([], TFun(TString, TOption TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match FileSystemPolicy.tryResolvePath ctx path with
                  | Some full when FileSystemPolicy.isDeniedPath ctx full -> throwBlockedPathAccess "Fs.readText" full
                  | Some full when File.Exists(full) -> HostCommon.some (VString (File.ReadAllText(full)))
                  | _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Fs.readText expects (string)") }

    let internal existsWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.exists"
          Scheme = Forall([], TFun(TString, TBool))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match FileSystemPolicy.tryResolvePath ctx path with
                  | Some full when FileSystemPolicy.isDeniedPath ctx full -> VBool false
                  | Some full -> VBool (File.Exists(full) || Directory.Exists(full))
                  | None -> VBool false
              | _ -> raise (HostCommon.evalError "Fs.exists expects (string)") }

    let internal entry_kindWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.kind"
          Scheme = Forall([], TFun(TString, TNamed "FsKind"))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match FileSystemPolicy.tryResolvePath ctx path with
                  | Some full ->
                      let root = FileSystemPolicy.normalizeRoot ctx
                      let relative = Path.GetRelativePath(root, full) |> normalizeSeparators
                      if FileSystemPolicy.isDeniedPath ctx full then
                          VUnionCase("FsKind", "Missing", None)
                      elif File.Exists(full) then
                          VUnionCase("FsKind", "File", Some (VString relative))
                      elif Directory.Exists(full) then
                          VUnionCase("FsKind", "Directory", Some (VString relative))
                      else
                          VUnionCase("FsKind", "Missing", None)
                  | None -> VUnionCase("FsKind", "Missing", None)
              | _ -> raise (HostCommon.evalError "Fs.kind expects (string)") }

    let internal create_directoryWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.createDirectory"
          Scheme = Forall([], TFun(TString, TBool))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match FileSystemPolicy.tryResolvePath ctx path with
                  | Some full when FileSystemPolicy.isDeniedPath ctx full -> throwBlockedPathAccess "Fs.createDirectory" full
                  | Some full ->
                      try
                          Directory.CreateDirectory(full) |> ignore
                          VBool (Directory.Exists(full))
                      with _ -> VBool false
                  | None -> VBool false
              | _ -> raise (HostCommon.evalError "Fs.createDirectory expects (string)") }

    let internal write_textWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.writeText"
          Scheme = Forall([], TFun(TString, TFun(TString, TBool)))
          Arity = 2
          Impl = fun _ -> function
              | [ VString path; VString content ] ->
                  match FileSystemPolicy.tryResolvePath ctx path with
                  | Some full when FileSystemPolicy.isDeniedPath ctx full -> throwBlockedPathAccess "Fs.writeText" full
                  | Some full ->
                      try
                          match Path.GetDirectoryName(full) with
                          | null
                          | "" -> ()
                          | directory -> Directory.CreateDirectory(directory) |> ignore
                          File.WriteAllText(full, content)
                          VBool true
                      with _ -> VBool false
                  | None -> VBool false
              | _ -> raise (HostCommon.evalError "Fs.writeText expects (string, string)") }

    let combine_path : ExternalFunction =
        { RequiredPermissions = FScript.Language.HostPermissions.None; Name = "Fs.combinePath"
          Scheme = Forall([], TFun(TString, TFun(TString, TString)))
          Arity = 2
          Impl = fun _ -> function
              | [ VString left; VString right ] ->
                  Path.Combine(left, right)
                  |> normalizeSeparators
                  |> VString
              | _ -> raise (HostCommon.evalError "Fs.combinePath expects (string, string)") }

    let parent_directory : ExternalFunction =
        { RequiredPermissions = FScript.Language.HostPermissions.None; Name = "Fs.parentDirectory"
          Scheme = Forall([], TFun(TString, TOption TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match Path.GetDirectoryName(path) with
                  | null -> HostCommon.none
                  | value when value = "" -> HostCommon.none
                  | value -> HostCommon.some (VString (normalizeSeparators value))
              | _ -> raise (HostCommon.evalError "Fs.parentDirectory expects (string)") }

    let extension : ExternalFunction =
        { RequiredPermissions = FScript.Language.HostPermissions.None; Name = "Fs.extension"
          Scheme = Forall([], TFun(TString, TOption TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match Path.GetExtension(path) with
                  | null -> HostCommon.none
                  | value when value = "" -> HostCommon.none
                  | value -> HostCommon.some (VString value)
              | _ -> raise (HostCommon.evalError "Fs.extension expects (string)") }

    let file_name_without_extension : ExternalFunction =
        { RequiredPermissions = FScript.Language.HostPermissions.None; Name = "Fs.fileNameWithoutExtension"
          Scheme = Forall([], TFun(TString, TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match Path.GetFileNameWithoutExtension(path) with
                  | null -> VString ""
                  | value -> VString value
              | _ -> raise (HostCommon.evalError "Fs.fileNameWithoutExtension expects (string)") }

    let internal globWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.glob"
          Scheme = Forall([], TFun(TString, TOption (TList TString)))
          Arity = 1
          Impl = fun _ -> function
              | [ VString pattern ] ->
                  let root = FileSystemPolicy.normalizeRoot ctx
                  let regex = Regex(HostCommon.globToRegex pattern, RegexOptions.Compiled)
                  try
                      if not (Directory.Exists(root)) || not (FileSystemPolicy.isSafePath ctx root) || FileSystemPolicy.isDeniedPath ctx root then
                          HostCommon.none
                      else
                          enumerateReachableFiles ctx root
                          |> Seq.map (fun full -> Path.GetRelativePath(root, full).Replace("\\", "/"))
                          |> Seq.filter regex.IsMatch
                          |> Seq.map VString
                          |> Seq.toList
                          |> VList
                          |> HostCommon.some
                  with _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Fs.glob expects (string)") }

    let internal enumerate_filesWithPolicy (ctx: FileSystemPolicy) : ExternalFunction =
        { RequiredPermissions = HostPermissions.IO; Name = "Fs.enumerateFiles"
          Scheme = Forall([], TFun(TString, TFun(TString, TOption (TList TString))))
          Arity = 2
          Impl = fun _ -> function
              | [ VString directory; VString pattern ] ->
                  match FileSystemPolicy.tryResolvePath ctx directory with
                  | Some fullDirectory when Directory.Exists(fullDirectory) && not (FileSystemPolicy.isDeniedPath ctx fullDirectory) ->
                      let root = FileSystemPolicy.normalizeRoot ctx
                      let regex = Regex(HostCommon.globToRegex pattern, RegexOptions.Compiled)
                      try
                          enumerateCandidateFiles ctx fullDirectory pattern
                          |> Seq.filter (fun full ->
                              let relativeToDirectory = Path.GetRelativePath(fullDirectory, full) |> normalizeSeparators
                              regex.IsMatch relativeToDirectory)
                          |> Seq.map (fun full -> Path.GetRelativePath(root, full) |> normalizeSeparators |> VString)
                          |> Seq.toList
                          |> VList
                          |> HostCommon.some
                      with _ -> HostCommon.none
                  | _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Fs.enumerateFiles expects (string, string)") }

    let read_text context = read_textWithPolicy (FileSystemPolicy.create context)

    let exists context = existsWithPolicy (FileSystemPolicy.create context)

    let entry_kind context = entry_kindWithPolicy (FileSystemPolicy.create context)

    let create_directory context = create_directoryWithPolicy (FileSystemPolicy.create context)

    let write_text context = write_textWithPolicy (FileSystemPolicy.create context)

    let glob context = globWithPolicy (FileSystemPolicy.create context)

    let enumerate_files context = enumerate_filesWithPolicy (FileSystemPolicy.create context)
