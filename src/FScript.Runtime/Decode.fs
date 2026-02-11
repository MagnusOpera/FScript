namespace FScript.Runtime

open System
open System.Text.Json
open System.Xml.Linq
open FScript.Language

module internal HostDecode =
    let private decodeMapKey (target: Type) (raw: string) : MapKey option =
        match target with
        | TString -> Some (MKString raw)
        | TInt ->
            match Int64.TryParse(raw) with
            | true, value -> Some (MKInt value)
            | _ -> None
        | _ -> None

    let rec decodeJson (target: Type) (el: JsonElement) : Value option =
        match target with
        | TUnit -> Some VUnit
        | TString when el.ValueKind = JsonValueKind.String ->
            match el.GetString() with
            | null -> None
            | value -> Some (VString value)
        | TInt -> HostCommon.jsonInt el |> Option.map VInt
        | TFloat when el.ValueKind = JsonValueKind.Number -> Some (VFloat (el.GetDouble()))
        | TBool when el.ValueKind = JsonValueKind.True || el.ValueKind = JsonValueKind.False -> Some (VBool (el.GetBoolean()))
        | TList inner when el.ValueKind = JsonValueKind.Array ->
            el.EnumerateArray()
            |> Seq.map (decodeJson inner)
            |> Seq.fold (fun acc next ->
                match acc, next with
                | Some xs, Some x -> Some (x :: xs)
                | _ -> None) (Some [])
            |> Option.map (List.rev >> VList)
        | TOption _ when el.ValueKind = JsonValueKind.Null -> Some (VOption None)
        | TOption inner -> decodeJson inner el |> Option.map (Some >> VOption)
        | TTuple elements when el.ValueKind = JsonValueKind.Array ->
            let arr = el.EnumerateArray() |> Seq.toList
            if arr.Length <> elements.Length then None
            else
                (arr, elements)
                ||> List.zip
                |> List.map (fun (json, t) -> decodeJson t json)
                |> List.fold (fun acc next ->
                    match acc, next with
                    | Some xs, Some x -> Some (x :: xs)
                    | _ -> None) (Some [])
                |> Option.map (List.rev >> VTuple)
        | TMap (keyType, inner) when el.ValueKind = JsonValueKind.Object ->
            let mutable ok = true
            let mutable map = Map.empty<MapKey, Value>
            for prop in el.EnumerateObject() do
                match decodeMapKey keyType prop.Name, decodeJson inner prop.Value with
                | Some key, Some v -> map <- map.Add(key, v)
                | _ -> ok <- false
            if ok then Some (VMap map) else None
        | TRecord fields when el.ValueKind = JsonValueKind.Object ->
            let mutable ok = true
            let mutable map = Map.empty<string, Value>
            for KeyValue(name, ft) in fields do
                let mutable prop = Unchecked.defaultof<JsonElement>
                if el.TryGetProperty(name, &prop) then
                    match decodeJson ft prop with
                    | Some v -> map <- map.Add(name, v)
                    | None -> ok <- false
                else
                    ok <- false
            if ok then Some (VRecord map) else None
        | TNamed _
        | TUnion _
        | TTypeToken
        | TFun _
        | TVar _ -> None
        | _ -> None

    let rec decodeXmlValue (target: Type) (el: XElement) : Value option =
        let parseInt (s: string) =
            match Int64.TryParse(s) with
            | true, v -> Some v
            | _ -> None
        let parseFloat (s: string) =
            match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | _ -> None
        let parseBool (s: string) =
            match Boolean.TryParse(s) with
            | true, v -> Some v
            | _ -> None

        match target with
        | TUnit -> Some VUnit
        | TString -> Some (VString el.Value)
        | TInt -> parseInt el.Value |> Option.map VInt
        | TFloat -> parseFloat el.Value |> Option.map VFloat
        | TBool -> parseBool el.Value |> Option.map VBool
        | TOption inner -> decodeXmlValue inner el |> Option.map (Some >> VOption)
        | TList inner ->
            el.Elements()
            |> Seq.map (decodeXmlValue inner)
            |> Seq.fold (fun acc next ->
                match acc, next with
                | Some xs, Some x -> Some (x :: xs)
                | _ -> None) (Some [])
            |> Option.map (List.rev >> VList)
        | TMap (keyType, inner) ->
            let mutable ok = true
            let mutable map = Map.empty<MapKey, Value>
            for child in el.Elements() do
                match decodeMapKey keyType child.Name.LocalName, decodeXmlValue inner child with
                | Some key, Some v -> map <- map.Add(key, v)
                | _ -> ok <- false
            if ok then Some (VMap map) else None
        | TRecord fields ->
            let mutable ok = true
            let mutable map = Map.empty<string, Value>
            for KeyValue(name, ft) in fields do
                match el.Element(XName.Get(name)) with
                | null -> ok <- false
                | child ->
                    match decodeXmlValue ft child with
                    | Some v -> map <- map.Add(name, v)
                    | None -> ok <- false
            if ok then Some (VRecord map) else None
        | TTuple _
        | TNamed _
        | TUnion _
        | TTypeToken
        | TFun _
        | TVar _ -> None
