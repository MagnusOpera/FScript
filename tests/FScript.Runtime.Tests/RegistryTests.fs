namespace FScript.Runtime.Tests

open System.IO
open NUnit.Framework
open FsUnit
open FScript.Runtime

[<TestFixture>]
type RegistryTests () =
    [<Test>]
    member _.``Registry exposes expected extern names`` () =
        let host = { RootDirectory = Directory.GetCurrentDirectory() }
        let names = Registry.all host |> List.map (fun e -> e.Name) |> Set.ofList
        names.Contains "Fs.readText" |> should equal true
        names.Contains "Fs.exists" |> should equal true
        names.Contains "Fs.isFile" |> should equal true
        names.Contains "Fs.isDirectory" |> should equal true
        names.Contains "Fs.createDirectory" |> should equal true
        names.Contains "Fs.writeText" |> should equal true
        names.Contains "Fs.combinePath" |> should equal true
        names.Contains "Fs.parentDirectory" |> should equal true
        names.Contains "Fs.extension" |> should equal true
        names.Contains "Fs.fileNameWithoutExtension" |> should equal true
        names.Contains "Fs.glob" |> should equal true
        names.Contains "Fs.enumerateFiles" |> should equal true
        names.Contains "Regex.matchGroups" |> should equal true
        names.Contains "Hash.md5" |> should equal true
        names.Contains "Guid.new" |> should equal true
        names.Contains "print" |> should equal true
        names.Contains "Json.deserialize" |> should equal true
        names.Contains "Json.serialize" |> should equal true
        names.Contains "Xml.deserialize" |> should equal true
        names.Contains "Xml.serialize" |> should equal true
