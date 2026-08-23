namespace FScript.Compiler.Tests

open System
open System.Diagnostics
open System.IO
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type GoBackendTests() =
    [<Test>]
    member _.``Generated add project runs with Go`` () =
        let input = { Program = Parser.parseProgram "let add x y = x + y\nadd 1 2"; Externs = [] }
        let compiled =
            match Compiler.compile input with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let files =
            match GoBackend.generate { ModuleName = "fscript.generated/testprogram" } compiled.Lir with
            | Ok value -> value
            | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        let directory = Path.Combine(Path.GetTempPath(), $"fscript-go-{Guid.NewGuid():N}")
        try
            for file in files do
                let path = Path.Combine(directory, file.RelativePath)
                match Path.GetDirectoryName(path) with
                | null -> ()
                | parent -> Directory.CreateDirectory(parent) |> ignore
                File.WriteAllText(path, file.Content)
            let start = ProcessStartInfo("go")
            start.WorkingDirectory <- directory
            start.ArgumentList.Add("run")
            start.ArgumentList.Add(".")
            start.RedirectStandardOutput <- true
            start.RedirectStandardError <- true
            use proc =
                match Process.Start(start) with
                | null -> failwith "Failed to start Go"
                | value -> value
            let output = proc.StandardOutput.ReadToEnd()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Assert.That(proc.ExitCode, Is.EqualTo(0), error)
            Assert.That(output.Trim(), Is.EqualTo("3"))
        finally
            if Directory.Exists directory then Directory.Delete(directory, true)
