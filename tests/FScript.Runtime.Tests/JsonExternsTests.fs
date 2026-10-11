namespace FScript.Runtime.Tests

open System.Text.Json
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type JsonExternsTests () =
    [<Test>]
    member _.``json maps support bool keys`` () =
        let target = TMap(TBool, TInt)
        match invoke JsonExterns.deserialize [ VTypeToken target; VString "{\"true\":1,\"false\":2}" ] with
        | VOption(Some(VMap values)) ->
            Assert.That(values.ContainsKey(MKBool true), Is.True)
            Assert.That(values.ContainsKey(MKBool false), Is.True)
            match invoke JsonExterns.serialize [ VMap values ] with
            | VOption(Some(VString json)) ->
                use doc = JsonDocument.Parse(json)
                Assert.That(doc.RootElement.GetProperty("true").GetInt32(), Is.EqualTo(1))
                Assert.That(doc.RootElement.GetProperty("false").GetInt32(), Is.EqualTo(2))
            | _ -> Assert.Fail("Expected serialized bool-keyed map")
        | _ -> Assert.Fail("Expected bool-keyed map")

    [<Test>]
    member _.``json rejects union map keys`` () =
        let value = VMap(Map.ofList [ MKUnionCase("Color", "Red"), VInt 1L ])
        match invoke JsonExterns.serialize [ value ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected unsupported union map keys to return None")

    [<Test>]
    member _.``json_deserialize returns None for type mismatch`` () =
        let target = TRecord (Map.ofList [ "Age", TInt ])
        match invoke JsonExterns.deserialize [ VTypeToken target; VString "{\"Age\":\"x\"}" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")

    [<Test>]
    member _.``json_deserialize fills missing optional record fields with None`` () =
        let target =
            TRecord (
                Map.ofList [
                    "name", TString
                    "dependencies", TOption (TMap (TString, TString))
                    "devDependencies", TOption (TMap (TString, TString))
                ]
            )

        match invoke JsonExterns.deserialize [ VTypeToken target; VString "{\"name\":\"pkg\"}" ] with
        | VOption (Some (VRecord fields)) ->
            match fields["name"] with
            | VString "pkg" -> ()
            | _ -> Assert.Fail("Expected name field to decode as VString \"pkg\"")

            match fields["dependencies"] with
            | VOption None -> ()
            | _ -> Assert.Fail("Expected missing dependencies field to decode as None")

            match fields["devDependencies"] with
            | VOption None -> ()
            | _ -> Assert.Fail("Expected missing devDependencies field to decode as None")
        | _ -> Assert.Fail("Expected Some record with missing optional fields defaulted to None")

    [<Test>]
    member _.``json_serialize encodes records lists and options`` () =
        let value =
            VRecord (
                Map.ofList [
                    "Name", VString "pkg"
                    "Version", VOption None
                    "Deps", VList [ VString "core"; VString "cli" ]
                ]
            )

        match invoke JsonExterns.serialize [ value ] with
        | VOption (Some (VString json)) ->
            use doc = JsonDocument.Parse(json)
            let root = doc.RootElement
            Assert.That(root.GetProperty("Name").GetString(), Is.EqualTo("pkg"))
            Assert.That(root.GetProperty("Version").ValueKind, Is.EqualTo(JsonValueKind.Null))
            Assert.That(root.GetProperty("Deps").GetArrayLength(), Is.EqualTo(2))
        | _ -> Assert.Fail("Expected serialized JSON")

    [<Test>]
    member _.``json_serialize returns None for unsupported values`` () =
        match invoke JsonExterns.serialize [ VUnionCtor("Result", "Ok") ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")
