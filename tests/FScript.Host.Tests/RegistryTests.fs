namespace FScript.Host.Tests

open System.IO
open NUnit.Framework
open FsUnit
open FScript.Host

[<TestFixture>]
type RegistryTests () =
    [<Test>]
    member _.``Registry exposes expected extern names`` () =
        let host = { RootDirectory = Directory.GetCurrentDirectory() }
        let names = Registry.all host |> List.map (fun e -> e.Name) |> Set.ofList
        names.Contains "fs_read_text" |> should equal true
        names.Contains "fs_glob" |> should equal true
        names.Contains "regex_match_groups" |> should equal true
        names.Contains "hash_md5" |> should equal true
        names.Contains "guid_new" |> should equal true
        names.Contains "print" |> should equal true
        names.Contains "map_empty" |> should equal true
        names.Contains "json_deserialize" |> should equal true
        names.Contains "xml_values" |> should equal true
