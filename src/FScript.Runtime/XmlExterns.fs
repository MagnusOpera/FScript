namespace FScript.Runtime

open System
open System.Xml.Linq
open FScript.Language

module XmlExterns =
    let values : ExternalFunction =
        { Name = "Xml.values"
          Scheme = Forall([ 0 ], TFun(TTypeToken, TFun(TString, TFun(TString, TOption (TList (TVar 0))))))
          Arity = 3
          Impl = fun _ -> function
              | [ VTypeToken t; VString xml; VString query ] ->
                  try
                      let doc = XDocument.Parse(xml)
                      let segments =
                          query.Split('/', StringSplitOptions.RemoveEmptyEntries)
                          |> Array.toList

                      let start =
                          match doc.Root with
                          | null -> Seq.empty
                          | root -> seq { root }

                      let nodes =
                          (start, segments)
                          ||> List.fold (fun cur seg -> cur |> Seq.collect (fun e -> e.Elements(XName.Get seg)))
                          |> Seq.toList

                      nodes
                      |> List.map (HostDecode.decodeXmlValue t)
                      |> List.fold (fun acc next ->
                          match acc, next with
                          | Some xs, Some x -> Some (x :: xs)
                          | _ -> None) (Some [])
                      |> Option.map (List.rev >> VList >> HostCommon.some)
                      |> Option.defaultValue HostCommon.none
                  with _ -> HostCommon.none
              | _ -> raise (HostCommon.evalError "Xml.values expects (type, xml, query)") }
