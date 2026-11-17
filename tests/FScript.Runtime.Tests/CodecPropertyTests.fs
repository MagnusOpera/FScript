namespace FScript.Runtime.Tests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Xml.Linq
open FsCheck
open FsCheck.FSharp
open FScript.Language
open FScript.Runtime
open NUnit.Framework
open RuntimePropertySupport

module private CodecData =
    type Kind = Integer | Decimal | Boolean | Text
    type Atom = Number of int64 | Fraction of int | Flag of bool | String of string
    type KeyKind = StringKeys | IntKeys | BoolKeys
    type Data =
        | Scalar of Atom | Items of Kind * Atom list | Maybe of Kind * Atom option
        | Pair of Data * Data | Fields of (string * Data) list
        | Mapping of KeyKind * Kind * (int * Atom) list

    let typ = function Integer -> TInt | Decimal -> TFloat | Boolean -> TBool | Text -> TString
    let atomValue = function Number n -> VInt n | Fraction n -> VFloat(float n / 4.0) | Flag b -> VBool b | String s -> VString s
    let atomType = function Number _ -> TInt | Fraction _ -> TFloat | Flag _ -> TBool | String _ -> TString
    let key kind n = match kind with StringKeys -> MKString($"k{n}") | IntKeys -> MKInt(int64 n) | BoolKeys -> MKBool(n % 2 = 0)
    let keyText = function MKString s -> s | MKInt n -> string n | MKBool b -> (if b then "true" else "false") | _ -> failwith "Unexpected key"

    let atom stringGenerator = function
        | Integer -> Gen.oneof [Gen.choose(-100000, 100000) |> Gen.map (int64 >> Number); Gen.elements [Number Int64.MinValue; Number Int64.MaxValue; Number 0L]]
        | Decimal -> Gen.choose(-10000, 10000) |> Gen.map Fraction
        | Boolean -> Gen.elements [Flag false; Flag true]
        | Text -> stringGenerator |> Gen.map String

    let rec generate xml depth = gen {
        let! kind = Gen.elements [Integer; Decimal; Boolean; Text]
        let atom = atom (if xml then xmlText else text) kind
        let! choice = Gen.choose(0, if depth = 0 then 2 elif xml then 3 else 5)
        match choice with
        | 0 -> let! item = atom in return Scalar item
        | 1 ->
            let! count = Gen.choose(0, 4)
            let! items = Gen.listOfLength count atom
            return Items(kind, items)
        | 2 ->
            let! present = Gen.elements [true; false]
            let! item = atom
            return Maybe(kind, if present then Some item else None)
        | 3 ->
            let! count = Gen.choose(0, 3)
            let! fields = Gen.listOfLength count (generate xml (depth - 1))
            return Fields(List.mapi (fun i item -> $"Field{i}", item) fields)
        | 4 ->
            let! left = generate xml (depth - 1)
            let! right = generate xml (depth - 1)
            return Pair(left, right)
        | _ ->
            let! keys = Gen.elements [StringKeys; IntKeys; BoolKeys]
            let! count = Gen.choose(0, 4)
            let! entries = Gen.listOfLength count (Gen.map2 (fun key value -> key, value) (Gen.choose(-3, 3)) atom)
            return Mapping(keys, kind, entries)
    }

    let shrinkAtom = function
        | Number n -> seq { if n <> 0L then yield Number 0L }
        | Fraction n -> shrinkInt n |> Seq.map Fraction
        | Flag true -> Seq.singleton (Flag false)
        | Flag false -> Seq.empty
        | String s -> shrinkText s |> Seq.map String

    let rec shrink = function
        | Scalar a -> shrinkAtom a |> Seq.map Scalar
        | Items(kind, items) -> shrinkList shrinkAtom items |> Seq.map (fun items -> Items(kind, items))
        | Maybe(kind, Some a) -> seq { yield Maybe(kind, None); yield! shrinkAtom a |> Seq.map (fun a -> Maybe(kind, Some a)) }
        | Maybe(_, None) -> Seq.empty
        | Pair(left, right) -> seq { yield left; yield right; yield! shrink left |> Seq.map (fun l -> Pair(l, right)); yield! shrink right |> Seq.map (fun r -> Pair(left, r)) }
        | Fields fields ->
            shrinkList (fun (name, item) -> shrink item |> Seq.map (fun item -> name, item)) fields |> Seq.map Fields
        | Mapping(keys, kind, entries) ->
            shrinkList (fun (key, a) -> shrinkAtom a |> Seq.map (fun a -> key, a)) entries |> Seq.map (fun entries -> Mapping(keys, kind, entries))

    let arbitrary xml =
        let generated = generate xml 2
        // XML's encoder accepts a record root. Preserve that root when shrinking.
        if xml then Arb.fromGenShrink(Gen.map (fun data -> Fields ["Payload", data]) generated,
                                     function Fields [name, data] -> shrink data |> Seq.map (fun data -> Fields [name, data]) | _ -> Seq.empty)
        else Arb.fromGenShrink(generated, shrink)

    let rec materialize = function
        | Scalar a -> atomType a, atomValue a
        | Items(kind, items) -> TList(typ kind), VList(List.map atomValue items)
        | Maybe(kind, item) -> TOption(typ kind), VOption(Option.map atomValue item)
        | Pair(left, right) -> let lt, lv = materialize left in let rt, rv = materialize right in TTuple [lt; rt], VTuple [lv; rv]
        | Fields fields ->
            let fields = fields |> List.map (fun (name, data) -> name, materialize data)
            TRecord(Map.ofList [for name, (t, _) in fields -> name, t]), VRecord(Map.ofList [for name, (_, v) in fields -> name, v])
        | Mapping(keys, kind, entries) ->
            let kt = match keys with StringKeys -> TString | IntKeys -> TInt | BoolKeys -> TBool
            TMap(kt, typ kind), VMap(entries |> List.map (fun (n, a) -> key keys n, atomValue a) |> Map.ofList)

    let json data =
        use stream = new MemoryStream()
        use writer = new Utf8JsonWriter(stream)
        let writeAtom = function
            | Number n -> writer.WriteNumberValue n
            | Fraction n -> writer.WriteNumberValue(float n / 4.0)
            | Flag b -> writer.WriteBooleanValue b
            | String s -> writer.WriteStringValue s
        let rec write = function
            | Scalar a -> writeAtom a
            | Items(_, items) -> writer.WriteStartArray(); items |> List.iter writeAtom; writer.WriteEndArray()
            | Maybe(_, None) -> writer.WriteNullValue()
            | Maybe(_, Some a) -> writeAtom a
            | Pair(left, right) -> writer.WriteStartArray(); write left; write right; writer.WriteEndArray()
            | Fields fields ->
                writer.WriteStartObject()
                for name, data in fields do writer.WritePropertyName name; write data
                writer.WriteEndObject()
            | Mapping(keys, _, entries) ->
                writer.WriteStartObject()
                for KeyValue(k, a) in (entries |> List.map (fun (k, a) -> key keys k, a) |> Map.ofList) do
                    writer.WritePropertyName(keyText k); writeAtom a
                writer.WriteEndObject()
        write data
        writer.Flush()
        Encoding.UTF8.GetString(stream.ToArray())

    let xml data =
        let atomText = function
            | Number n -> string n
            | Fraction n -> (float n / 4.0).ToString(Globalization.CultureInfo.InvariantCulture)
            | Flag b -> if b then "true" else "false"
            | String s -> s
        let rec element name = function
            | Scalar a -> XElement(XName.Get name, atomText a)
            | Items(_, items) -> XElement(XName.Get name, [for a in items -> element "item" (Scalar a)] :> obj)
            | Maybe(_, Some a) -> element name (Scalar a)
            | Maybe(_, None) -> XElement(XName.Get name)
            | Fields fields ->
                XElement(XName.Get name, [for name, data in fields do match data with Maybe(_, None) -> () | _ -> yield element name data] :> obj)
            | _ -> failwith "Unsupported XML generator shape"
        element "root" data

[<TestFixture>]
type CodecPropertyTests() =
    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.JsonTypedCodecs(seed: uint64) =
        verify "CodecPropertyTests.JsonTypedCodecs" 100 (CodecData.arbitrary false) seed (fun data ->
            let target, expected = CodecData.materialize data
            let json = CodecData.json data
            use doc = JsonDocument.Parse json
            value (some expected) (invoke JsonExterns.deserialize [VTypeToken target; VString json])
            match HostDecode.decodeJson target doc.RootElement with Some actual -> value expected actual | None -> failwith ("Typed JSON decoder rejected a generated value")
            match invoke JsonExterns.serialize [expected] with
            | VOption(Some(VString encoded)) ->
                use actual = JsonDocument.Parse encoded
                equal true (JsonElement.DeepEquals(doc.RootElement, actual.RootElement))
                value (some expected) (invoke JsonExterns.deserialize [VTypeToken target; VString encoded])
            | actual -> failwith ($"JSON encoding failed: {actual}"))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.JsonSchemaFailuresAndOptionalFields(seed: uint64) =
        verify "CodecPropertyTests.JsonSchemaFailuresAndOptionalFields" 100 inputs seed (fun input ->
            let n = int64 input.Number
            value (some VUnit) (invoke JsonExterns.deserialize [VTypeToken TUnit; VString (quote input.Text)])
            value (some(VString "null")) (invoke JsonExterns.serialize [VUnit])
            let record = TRecord(Map.ofList ["Required", TInt; "Optional", TOption TString])
            let json = $"{{\"Required\":{n},\"Extra\":true}}"
            value (some(VRecord(Map.ofList ["Required", VInt n; "Optional", VOption None]))) (invoke JsonExterns.deserialize [VTypeToken record; VString json])
            for target, document in [record, "{}"; TInt, "true"; TInt, "1.25"; TTuple [TInt; TInt], $"[{n}]"; TList TInt, $"[{n},false]"; TMap(TInt, TInt), "{\"bad\":1}"] do
                value (VOption None) (invoke JsonExterns.deserialize [VTypeToken target; VString document])
            value (VOption None) (invoke JsonExterns.deserialize [VTypeToken TString; VString ("{" + quote input.Text)])
            for unsupported in [THostManaged("Opaque", Map.empty); TApplied("Opaque", [TInt]); TFun(TInt, TInt); TTask TInt; TNamed "Unknown"; TUnion("U", Map.ofList ["Case", None]); TTypeToken] do
                value (VOption None) (invoke JsonExterns.deserialize [VTypeToken unsupported; VString "{}"])
                value (VOption None) (invoke JsonExterns.deserialize [VTypeToken(TList unsupported); VString "[{}]"])
            for unsupported in [VUnionCtor("U", "Case"); VUnionCase("U", "Case", None); VTypeToken TInt] do
                value (VOption None) (invoke JsonExterns.serialize [VRecord(Map.ofList ["Nested", VList [unsupported]])]))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.JsonRejectsNonFiniteNumbers(seed: uint64) =
        verify "CodecPropertyTests.JsonRejectsNonFiniteNumbers" 100 inputs seed (fun input ->
            for invalid in [Double.NaN; Double.PositiveInfinity; Double.NegativeInfinity] do
                for payload in [VFloat invalid; VList [VInt(int64 input.Number); VFloat invalid]; VRecord(Map.ofList ["Text", VString input.Text; "Invalid", VFloat invalid])] do
                    value (VOption None) (invoke JsonExterns.serialize [payload]))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.XmlInternalCodecs(seed: uint64) =
        verify "CodecPropertyTests.XmlInternalCodecs" 100 (CodecData.arbitrary true) seed (fun data ->
            let target, expected = CodecData.materialize data
            let independentlyEncoded = CodecData.xml data
            match HostDecode.decodeXmlValue target independentlyEncoded with Some decoded -> value expected decoded | None -> failwith ("XML decoder rejected a generated value")
            match HostEncode.encodeXml expected with
            | Some encoded ->
                let parsed = XDocument.Parse(encoded, LoadOptions.PreserveWhitespace)
                match HostDecode.decodeXmlValue target (nonNull parsed.Root) with Some decoded -> value expected decoded | None -> failwith ("XML roundtrip failed")
            | None -> failwith ("XML encoder rejected its supported subset"))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.DecodeXmlSchemaRules(seed: uint64) =
        verify "CodecPropertyTests.DecodeXmlSchemaRules" 100 inputs seed (fun input ->
            let root = XElement(XName.Get "root", XElement(XName.Get "Required", input.Number), XElement(XName.Get "Extra", "ignored"))
            let target = TRecord(Map.ofList ["Required", TInt; "Optional", TOption TString])
            match HostDecode.decodeXmlValue TUnit root with Some actual -> value VUnit actual | None -> failwith "Unit XML decoding failed"
            match HostDecode.decodeXmlValue target root with
            | Some actual -> value (VRecord(Map.ofList ["Required", VInt(int64 input.Number); "Optional", VOption None])) actual
            | None -> failwith ("XML record defaults failed")
            equal true (HostDecode.decodeXmlValue target (XElement(XName.Get "root"))).IsNone
            let mapXml = XElement(XName.Get "root", XElement(XName.Get "true", input.Number), XElement(XName.Get "false", 1))
            match HostDecode.decodeXmlValue (TMap(TBool, TInt)) mapXml with
            | Some actual -> value (VMap(Map.ofList [MKBool true, VInt(int64 input.Number); MKBool false, VInt 1L])) actual
            | None -> failwith ("XML bool-keyed map failed")
            let invalid = XElement(XName.Get "root", XElement(XName.Get "item", "not-an-integer"))
            equal true (HostDecode.decodeXmlValue (TList TInt) invalid).IsNone
            for unsupported in [TTuple [TInt; TInt]; THostManaged("Opaque", Map.empty); TApplied("Opaque", [TInt]); TFun(TInt, TInt); TTask TInt; TNamed "Unknown"; TTypeToken] do
                equal true (HostDecode.decodeXmlValue unsupported root).IsNone
            for unsupported in [VTuple [VInt 1L; VInt 2L]; VMap(Map.ofList [MKString "a", VInt 1L]); VUnionCtor("U", "Case"); VTypeToken TInt] do
                equal None (HostEncode.encodeXml (VRecord(Map.ofList ["Nested", unsupported]))))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.XmlQueriesFollowDocumentModel(seed: uint64) =
        let rows = Arb.fromGenShrink(Gen.listOfLength 6 xmlText, shrinkList shrinkText)
        verify "CodecPropertyTests.XmlQueriesFollowDocumentModel" 100 rows seed (fun rows ->
            let direct, nested = rows |> List.indexed |> List.partition (fun (i, _) -> i % 2 = 0)
            let item (i: int, text: string) =
                let el = XElement(XName.Get "item", text)
                if i % 3 <> 0 then el.SetAttributeValue(XName.Get "key", text)
                el
            let root = XElement(XName.Get "root")
            for row in direct do root.Add(item row)
            root.Add(XElement(XName.Get "group", List.map item nested :> obj))
            let document = root.ToString(SaveOptions.DisableFormatting)
            let textValues (values: (int * string) list) = values |> List.map (fun (_, text) -> text.Trim()) |> List.filter ((<>) "") |> List.map VString |> VList |> some
            let attributes values = values |> List.choose (fun (i, text) -> if i % 3 <> 0 then Some(VString text) else None) |> VList |> some
            for query, expected in ["root/item/text()", textValues direct; "//item/text()", textValues (direct @ nested); "//item/@key", attributes (direct @ nested); "group/item/@key", attributes nested; "//missing/text()", some(VList [])] do
                value expected (invoke XmlExterns.query_values [VString query; VString document])
            for query in [""; "//item"; "//*/text()"; "//item/@"; "//item/text()/extra"] do
                value (VOption None) (invoke XmlExterns.query_values [VString query; VString document])
            value (VOption None) (invoke XmlExterns.query_values [VString "//item/text()"; VString "<root>"]))
