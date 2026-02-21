namespace FScript.Runtime

open System.IO
open System.Text.RegularExpressions
open FScript.Language

module FsExterns =
    let private normalizeSeparators (path: string) =
        path.Replace("\\", "/")

    let read_text (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.readText"
          Scheme = Forall([], TFun(TString, TOption TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match HostCommon.tryResolvePath ctx path with
                  | Some full when File.Exists(full) -> HostCommon.some (VString (File.ReadAllText(full)))
                  | _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Fs.readText expects (string)") }

    let exists (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.exists"
          Scheme = Forall([], TFun(TString, TBool))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match HostCommon.tryResolvePath ctx path with
                  | Some full -> VBool (File.Exists(full) || Directory.Exists(full))
                  | None -> VBool false
              | _ -> raise (HostCommon.evalError "Fs.exists expects (string)") }

    let entry_kind (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.kind"
          Scheme = Forall([], TFun(TString, TNamed "FsKind"))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match HostCommon.tryResolvePath ctx path with
                  | Some full ->
                      let root = HostCommon.normalizeRoot ctx
                      let relative = Path.GetRelativePath(root, full) |> normalizeSeparators
                      if File.Exists(full) then
                          VUnionCase("FsKind", "File", Some (VString relative))
                      elif Directory.Exists(full) then
                          VUnionCase("FsKind", "Directory", Some (VString relative))
                      else
                          VUnionCase("FsKind", "Missing", None)
                  | None -> VUnionCase("FsKind", "Missing", None)
              | _ -> raise (HostCommon.evalError "Fs.kind expects (string)") }

    let create_directory (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.createDirectory"
          Scheme = Forall([], TFun(TString, TBool))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match HostCommon.tryResolvePath ctx path with
                  | Some full ->
                      try
                          Directory.CreateDirectory(full) |> ignore
                          VBool (Directory.Exists(full))
                      with _ -> VBool false
                  | None -> VBool false
              | _ -> raise (HostCommon.evalError "Fs.createDirectory expects (string)") }

    let write_text (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.writeText"
          Scheme = Forall([], TFun(TString, TFun(TString, TBool)))
          Arity = 2
          Impl = fun _ -> function
              | [ VString path; VString content ] ->
                  match HostCommon.tryResolvePath ctx path with
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
        { Name = "Fs.combinePath"
          Scheme = Forall([], TFun(TString, TFun(TString, TString)))
          Arity = 2
          Impl = fun _ -> function
              | [ VString left; VString right ] ->
                  Path.Combine(left, right)
                  |> normalizeSeparators
                  |> VString
              | _ -> raise (HostCommon.evalError "Fs.combinePath expects (string, string)") }

    let parent_directory : ExternalFunction =
        { Name = "Fs.parentDirectory"
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
        { Name = "Fs.extension"
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
        { Name = "Fs.fileNameWithoutExtension"
          Scheme = Forall([], TFun(TString, TString))
          Arity = 1
          Impl = fun _ -> function
              | [ VString path ] ->
                  match Path.GetFileNameWithoutExtension(path) with
                  | null -> VString ""
                  | value -> VString value
              | _ -> raise (HostCommon.evalError "Fs.fileNameWithoutExtension expects (string)") }

    let glob (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.glob"
          Scheme = Forall([], TFun(TString, TOption (TList TString)))
          Arity = 1
          Impl = fun _ -> function
              | [ VString pattern ] ->
                  let root = HostCommon.normalizeRoot ctx
                  let regex = Regex(HostCommon.globToRegex pattern, RegexOptions.Compiled)
                  try
                      Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
                      |> Seq.map (fun full -> Path.GetRelativePath(root, full).Replace("\\", "/"))
                      |> Seq.filter regex.IsMatch
                      |> Seq.map VString
                      |> Seq.toList
                      |> VList
                      |> HostCommon.some
                  with _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Fs.glob expects (string)") }

    let enumerate_files (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.enumerateFiles"
          Scheme = Forall([], TFun(TString, TFun(TString, TOption (TList TString))))
          Arity = 2
          Impl = fun _ -> function
              | [ VString directory; VString pattern ] ->
                  match HostCommon.tryResolvePath ctx directory with
                  | Some fullDirectory when Directory.Exists(fullDirectory) ->
                      let root = HostCommon.normalizeRoot ctx
                      let regex = Regex(HostCommon.globToRegex pattern, RegexOptions.Compiled)
                      try
                          Directory.EnumerateFiles(fullDirectory, "*", SearchOption.AllDirectories)
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
