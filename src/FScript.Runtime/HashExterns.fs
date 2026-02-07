namespace FScript.Runtime

open System
open System.Security.Cryptography
open System.Text
open FScript.Language

module HashExterns =
    let md5 : ExternalFunction =
        { Name = "Hash.md5"
          Scheme = Forall([], TFun(TString, TOption TString))
          Arity = 1
          Impl = function
              | [ VString input ] ->
                  try
                      use h = MD5.Create()
                      let bytes = Encoding.UTF8.GetBytes(input)
                      let hash = h.ComputeHash(bytes)
                      let hex = Convert.ToHexString(hash).ToLowerInvariant()
                      HostCommon.some (VString hex)
                  with _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Hash.md5 expects (string)") }
