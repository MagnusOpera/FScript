namespace FScript.Host

open System.IO
open System.Text.RegularExpressions
open FScript.Core

module FsExterns =
    let read_text (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.readText"
          Scheme = Forall([], TFun(TString, TOption TString))
          Arity = 1
          Impl = function
              | [ VString path ] ->
                  match HostCommon.tryResolvePath ctx path with
                  | Some full when File.Exists(full) -> HostCommon.some (VString (File.ReadAllText(full)))
                  | _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Fs.readText expects (string)") }

    let glob (ctx: HostContext) : ExternalFunction =
        { Name = "Fs.glob"
          Scheme = Forall([], TFun(TString, TOption (TList TString)))
          Arity = 1
          Impl = function
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
