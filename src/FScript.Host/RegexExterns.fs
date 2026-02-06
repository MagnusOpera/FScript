namespace FScript.Host

open System.Text.RegularExpressions
open FScript.Core

module RegexExterns =
    let match_groups : ExternalFunction =
        { Name = "regex_match_groups"
          Scheme = Forall([], TFun(TString, TFun(TString, TOption (TList TString))))
          Arity = 2
          Impl = function
              | [ VString pattern; VString input ] ->
                  try
                      let m = Regex.Match(input, pattern)
                      if not m.Success then HostCommon.none
                      else
                          [ 1 .. m.Groups.Count - 1 ]
                          |> List.map (fun i -> VString m.Groups.[i].Value)
                          |> VList
                          |> HostCommon.some
                  with _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "regex_match_groups expects (string, string)") }
