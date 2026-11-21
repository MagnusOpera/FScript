namespace FScript.Runtime

open System.Text.Json
open FScript.Language

module JsonExterns =
    let private decode definitions target json =
        try
            use doc = JsonDocument.Parse(json: string)
            match HostDecode.decodeJsonWithDefinitions definitions target doc.RootElement with
            | Some value -> HostCommon.some value
            | None -> HostCommon.none
        with _ -> HostCommon.none

    let deserialize : ExternalFunction =
        { RequiredPermissions = FScript.Language.HostPermissions.None; Name = JsonDeserialization.name
          Scheme = JsonDeserialization.scheme
          Arity = 1
          Impl = fun _ -> function
              | [ VTypeToken target; VTypeToken(TRecord definitions); VString json ] -> decode definitions target json
              | [ VTypeToken target; VString json ] -> decode Map.empty target json
              | _ -> raise (HostCommon.evalError "Json.deserialize requires a concrete target type") }

    let serialize : ExternalFunction =
        { RequiredPermissions = FScript.Language.HostPermissions.None; Name = "Json.serialize"
          Scheme = Forall([ 0 ], TFun(TVar 0, TOption TString))
          Arity = 1
          Impl = fun _ -> function
              | [ value ] ->
                  match HostEncode.encodeJson value with
                  | Some encoded -> HostCommon.some (VString encoded)
                  | None -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Json.serialize expects (value)") }
