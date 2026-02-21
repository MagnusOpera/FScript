namespace FScript.Runtime.Tests

open System.Text.Json
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type JsonExternsTests () =
    [<Test>]
    member _.``json_deserialize returns None for type mismatch`` () =
        let target = TRecord (Map.ofList [ "Age", TInt ])
        match invoke JsonExterns.deserialize [ VTypeToken target; VString "{\"Age\":\"x\"}" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")

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
