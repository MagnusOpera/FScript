namespace FScript.Runtime

open System
open System.Xml.Linq
open FScript.Language

module XmlExterns =
    type private ValueSelector =
        | Attribute of string
        | Text

    type private QuerySpec =
        { DescendantFirst: bool
          Steps: string list
          Selector: ValueSelector }

    let private isValidName (value: string) =
        not (String.IsNullOrWhiteSpace(value))
        && value.IndexOf(':') < 0
        && value <> "."
        && value <> ".."
        && value <> "*"

    let private splitPath (path: string) =
        path.Split('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    let private parseQuery (rawQuery: string) : QuerySpec option =
        if String.IsNullOrWhiteSpace(rawQuery) then
            None
        else
            let query = rawQuery.Trim()
            let descendantFirst, body =
                if query.StartsWith("//", StringComparison.Ordinal) then
                    true, query.Substring(2)
                else
                    false, query

            let slash = body.LastIndexOf('/')
            if slash <= 0 || slash = body.Length - 1 then
                None
            else
                let path = body.Substring(0, slash)
                let selectorRaw = body.Substring(slash + 1)

                let selector =
                    if selectorRaw.StartsWith("@", StringComparison.Ordinal) then
                        let name = selectorRaw.Substring(1)
                        if isValidName name then Some (Attribute name) else None
                    elif selectorRaw = "text()" then
                        Some Text
                    else
                        None

                let steps =
                    splitPath path
                    |> List.filter isValidName

                if steps.IsEmpty || steps.Length <> (splitPath path).Length then
                    None
                else
                    selector |> Option.map (fun sel -> { DescendantFirst = descendantFirst; Steps = steps; Selector = sel })

    let private traverseElements (doc: XDocument) (spec: QuerySpec) : XElement list =
        match doc.Root with
        | null -> []
        | root ->
            let first = spec.Steps.Head
            let tail = spec.Steps.Tail

            let start =
                if spec.DescendantFirst then
                    root.DescendantsAndSelf(XName.Get first)
                    |> Seq.toList
                elif root.Name.LocalName = first then
                    [ root ]
                else
                    root.Elements(XName.Get first)
                    |> Seq.toList

            (start, tail)
            ||> List.fold (fun current step ->
                current
                |> Seq.collect (fun e -> e.Elements(XName.Get step))
                |> Seq.toList)

    let private extractValues (elements: XElement list) (selector: ValueSelector) =
        match selector with
        | Attribute name ->
            elements
            |> List.choose (fun e ->
                match e.Attribute(XName.Get name) with
                | null -> None
                | attr -> Some attr.Value)
        | Text ->
            elements
            |> List.choose (fun e ->
                let value = e.Value.Trim()
                if String.IsNullOrEmpty(value) then None else Some value)

    let query_values : ExternalFunction =
        { Name = "Xml.queryValues"
          Scheme = Forall([], TFun(TString, TFun(TString, TOption (TList TString))))
          Arity = 2
          Impl = fun _ -> function
              | [ VString query; VString xml ] ->
                  match parseQuery query with
                  | None -> HostCommon.none
                  | Some spec ->
                      try
                          let doc = XDocument.Parse(xml)
                          let values =
                              traverseElements doc spec
                              |> extractValues <| spec.Selector
                              |> List.map VString
                              |> VList
                          HostCommon.some values
                      with _ ->
                          HostCommon.none
              | _ -> raise (HostCommon.evalError "Xml.queryValues expects (query, xmldoc)") }
