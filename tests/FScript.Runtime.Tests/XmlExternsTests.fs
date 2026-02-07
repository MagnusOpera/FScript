namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type XmlExternsTests () =
    [<Test>]
    member _.``xml_values decodes list of ints`` () =
        let xml = "<root><items><item>1</item><item>2</item></items></root>"
        match invoke XmlExterns.values [ VTypeToken TInt; VString xml; VString "items/item" ] with
        | VOption (Some (VList [ VInt 1L; VInt 2L ])) -> ()
        | _ -> Assert.Fail("Expected two ints")
