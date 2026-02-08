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
        names.Contains "Map.empty" |> should equal true
        names.Contains "Map.add" |> should equal true
        names.Contains "Map.ofList" |> should equal true
        names.Contains "Map.tryGet" |> should equal true
        names.Contains "Map.count" |> should equal true
        names.Contains "Map.filter" |> should equal true
        names.Contains "Map.fold" |> should equal true
        names.Contains "Map.choose" |> should equal true
        names.Contains "Map.map" |> should equal true
        names.Contains "Map.iter" |> should equal true
        names.Contains "Map.remove" |> should equal true
        names.Contains "List.empty" |> should equal true
        names.Contains "List.map" |> should equal true
        names.Contains "List.choose" |> should equal true
        names.Contains "List.collect" |> should equal true
        names.Contains "List.contains" |> should equal true
        names.Contains "List.distinct" |> should equal true
        names.Contains "List.exists" |> should equal true
        names.Contains "List.fold" |> should equal true
        names.Contains "List.filter" |> should equal true
        names.Contains "List.iter" |> should equal true
        names.Contains "List.rev" |> should equal true
        names.Contains "List.length" |> should equal true
        names.Contains "List.tryFind" |> should equal true
        names.Contains "List.tryGet" |> should equal true
        names.Contains "List.tryHead" |> should equal true
        names.Contains "List.tail" |> should equal true
        names.Contains "List.append" |> should equal true
        names.Contains "Option.defaultValue" |> should equal true
        names.Contains "Option.defaultWith" |> should equal true
        names.Contains "Option.isNone" |> should equal true
        names.Contains "Option.isSome" |> should equal true
        names.Contains "Option.map" |> should equal true
        names.Contains "Json.deserialize" |> should equal true
        names.Contains "Xml.values" |> should equal true
