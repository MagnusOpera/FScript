namespace FScript.Runtime.Tests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language
open FScript.Runtime

module internal ScriptTestFiles =
    let quote (value: string) = System.Text.Json.JsonSerializer.Serialize value
    let write root path (source: string) =
        let full = Path.Combine(root,path)
        Directory.CreateDirectory(Path.GetDirectoryName full |> nonNull) |> ignore
        File.WriteAllText(full,source)
    let withRoot run =
        let root = Path.Combine(Path.GetTempPath(),"fscript-child",Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory root |> ignore
        try run root
        finally Directory.Delete(root,true)
    let registration root grants denied = Registry.registration {Permissions=grants;RootDirectory=root;DeniedPathGlobs=denied}
    let run registration source = (ScriptHost.loadSourceWithRegistration registration source).LastValue
    let call path args options = $"FScript.exec {quote path} {args} {options}"
    let completed expected code = function
        | VUnionCase("FScriptResult","Completed",Some(VRecord fields)) ->
            Assert.That(fields.["value"] |> Pretty.valueToString, Is.EqualTo(Pretty.valueToString(VString expected)))
            Assert.That(fields.["exitCode"] |> Pretty.valueToString, Is.EqualTo(string code))
        | other -> Assert.Fail(Pretty.valueToString other)
    let failure prefix = function
        | VUnionCase("FScriptResult","Failed",Some(VRecord fields)) ->
            match fields.["message"] with
            | VString message -> Assert.That(message, Does.StartWith(prefix+":")); message
            | other -> failwith (string other)
        | other -> failwith (Pretty.valueToString other)
