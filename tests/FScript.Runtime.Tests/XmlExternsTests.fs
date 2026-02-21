namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type XmlExternsTests () =
    [<Test>]
    member _.``xml_deserialize decodes list of ints`` () =
        let xml = "<root><items><item>1</item><item>2</item></items></root>"
        match invoke XmlExterns.deserialize [ VTypeToken TInt; VString xml; VString "items/item" ] with
        | VOption (Some (VList [ VInt 1L; VInt 2L ])) -> ()
        | _ -> Assert.Fail("Expected two ints")

    [<Test>]
    member _.``xml_serialize encodes record values`` () =
        let value = VRecord (Map.ofList [ "Name", VString "pkg"; "Enabled", VBool true ])
        match invoke XmlExterns.serialize [ value ] with
        | VOption (Some (VString xml)) ->
            Assert.That(xml, Does.Contain("<root>"))
            Assert.That(xml, Does.Contain("<Name>pkg</Name>"))
            Assert.That(xml, Does.Contain("<Enabled>true</Enabled>"))
        | _ -> Assert.Fail("Expected serialized XML")

    [<Test>]
    member _.``xml_serialize returns None for non-record top-level`` () =
        match invoke XmlExterns.serialize [ VList [ VInt 1L; VInt 2L ] ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None")
