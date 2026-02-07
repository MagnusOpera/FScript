namespace FScript.Runtime

open System.Text.Json
open FScript.Language

module JsonExterns =
    let deserialize : ExternalFunction =
        { Name = "Json.deserialize"
          Scheme = Forall([ 0 ], TFun(TTypeToken, TFun(TString, TOption (TVar 0))))
          Arity = 2
          Impl = function
              | [ VTypeToken t; VString json ] ->
                  try
                      use doc = JsonDocument.Parse(json)
                      match HostDecode.decodeJson t doc.RootElement with
                      | Some value -> HostCommon.some value
                      | None -> HostCommon.none
                  with _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Json.deserialize expects (type, string)") }
