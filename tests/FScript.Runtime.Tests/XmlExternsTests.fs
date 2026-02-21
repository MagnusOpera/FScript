namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Runtime.Tests.HostTestHelpers

[<TestFixture>]
type XmlExternsTests () =
    [<Test>]
    member _.``xml_query_values extracts attribute values in document order`` () =
        let xml = "<root><ProjectReference Include=\"A.csproj\" /><ProjectReference Include=\"B.csproj\" /></root>"
        match invoke XmlExterns.query_values [ VString "//ProjectReference/@Include"; VString xml ] with
        | VOption (Some (VList [ VString "A.csproj"; VString "B.csproj" ])) -> ()
        | _ -> Assert.Fail("Expected attribute values in document order")

    [<Test>]
    member _.``xml_query_values returns Some empty list for valid query with no match`` () =
        let xml = "<root><ProjectReference Include=\"A.csproj\" /></root>"
        match invoke XmlExterns.query_values [ VString "//ProjectReference/@Missing"; VString xml ] with
        | VOption (Some (VList [])) -> ()
        | _ -> Assert.Fail("Expected Some [] for valid query with no match")

    [<Test>]
    member _.``xml_query_values extracts trimmed text values and drops empty text`` () =
        let xml = "<root><Name>  A  </Name><Name>   </Name><Name>B</Name></root>"
        match invoke XmlExterns.query_values [ VString "Name/text()"; VString xml ] with
        | VOption (Some (VList [ VString "A"; VString "B" ])) -> ()
        | _ -> Assert.Fail("Expected trimmed non-empty text values")

    [<Test>]
    member _.``xml_query_values keeps duplicate values`` () =
        let xml = "<root><ProjectReference Include=\"A.csproj\" /><ProjectReference Include=\"A.csproj\" /></root>"
        match invoke XmlExterns.query_values [ VString "//ProjectReference/@Include"; VString xml ] with
        | VOption (Some (VList [ VString "A.csproj"; VString "A.csproj" ])) -> ()
        | _ -> Assert.Fail("Expected duplicate values to be preserved")

    [<Test>]
    member _.``xml_query_values returns None for malformed xml`` () =
        match invoke XmlExterns.query_values [ VString "//ProjectReference/@Include"; VString "<root>" ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None for malformed xml")

    [<Test>]
    member _.``xml_query_values returns None for invalid query`` () =
        let xml = "<root><ProjectReference Include=\"A.csproj\" /></root>"
        match invoke XmlExterns.query_values [ VString "//ProjectReference"; VString xml ] with
        | VOption None -> ()
        | _ -> Assert.Fail("Expected None for invalid query")
