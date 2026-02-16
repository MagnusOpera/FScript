namespace FScript.Cli.Tests

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework

[<TestFixture>]
type CliTests() =
    let findRepoRoot () =
        let rec loop (dir: string) =
            let sln = Path.Combine(dir, "FScript.sln")
            if File.Exists(sln) then dir
            else
                let parent: DirectoryInfo | null = Directory.GetParent(dir)
                match parent with
                | null -> failwith "Could not locate repository root from test directory"
                | p -> loop p.FullName

        loop TestContext.CurrentContext.TestDirectory

    let runCli (repoRoot: string) (workingDir: string) (args: string list) (stdinText: string option) =
        let projectPath = Path.Combine(repoRoot, "src", "FScript")
        let allArgs = [ "run"; "--project"; projectPath; "-c"; "Release"; "--no-build"; "--" ] @ args

        let psi = ProcessStartInfo()
        psi.FileName <- "dotnet"
        psi.WorkingDirectory <- workingDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.RedirectStandardInput <- stdinText.IsSome
        psi.UseShellExecute <- false

        for a in allArgs do
            psi.ArgumentList.Add(a)

        use proc = new Process()
        proc.StartInfo <- psi
        proc.Start() |> ignore

        match stdinText with
        | Some text ->
            proc.StandardInput.Write(text)
            proc.StandardInput.Close()
        | None -> ()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        proc.ExitCode, stdout, stderr

    [<Test>]
    member _.``Runs script file from positional path`` () =
        let repoRoot = findRepoRoot ()
        let tempScript = Path.Combine(Path.GetTempPath(), $"fscript-cli-{Guid.NewGuid():N}.fss")

        try
            File.WriteAllText(tempScript, "1 + 2")
            let code, stdout, stderr = runCli repoRoot repoRoot [ tempScript ] None
            Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
            Assert.That(stdout.Trim(), Is.EqualTo("3"))
        finally
            if File.Exists(tempScript) then File.Delete(tempScript)

    [<Test>]
    member _.``Runs piped stdin script with root override`` () =
        let repoRoot = findRepoRoot ()
        let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-cli-root-{Guid.NewGuid():N}")
        Directory.CreateDirectory(tempRoot) |> ignore

        try
            let filePath = Path.Combine(tempRoot, "a.txt")
            File.WriteAllText(filePath, "hello")

            let code, stdout, stderr =
                runCli repoRoot tempRoot [ "-r"; "." ] (Some "Fs.exists \"a.txt\"")

            Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
            Assert.That(stdout.Trim(), Is.EqualTo("true"))
        finally
            if Directory.Exists(tempRoot) then Directory.Delete(tempRoot, true)

    [<Test>]
    member _.``Prints version with version command`` () =
        let repoRoot = findRepoRoot ()
        let code, stdout, stderr = runCli repoRoot repoRoot [ "version" ] None

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(Regex.IsMatch(stdout.Trim(), "^\\d+\\.\\d+\\.\\d+"), Is.True)

    [<Test>]
    member _.``No args with redirected stdin executes stdin mode`` () =
        let repoRoot = findRepoRoot ()
        let code, stdout, stderr = runCli repoRoot repoRoot [] (Some "40 + 2")

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(stdout.Trim(), Is.EqualTo("42"))

    [<Test>]
    member _.``File mode exposes Environment script name and arguments`` () =
        let repoRoot = findRepoRoot ()
        let tempScript = Path.Combine(Path.GetTempPath(), $"fscript-cli-env-{Guid.NewGuid():N}.fss")
        let scriptSource =
            "match Env.ScriptName with\n"
            + "| Some name -> print name\n"
            + "| None -> print \"none\"\n"
            + "type Wrapper = { Value: Environment }\n"
            + "let wrapped = { Value = Env }\n"
            + "print $\"{wrapped.Value.Arguments |> List.length}\"\n"
            + "for arg in Env.Arguments do\n"
            + "  print arg\n"
            + "()\n"

        try
            File.WriteAllText(tempScript, scriptSource)

            let code, stdout, stderr = runCli repoRoot repoRoot [ tempScript; "--"; "10"; "abc" ] None
            let lines = stdout.Replace("\r\n", "\n").Trim().Split('\n')

            Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
            Assert.That(lines.[0], Is.EqualTo(Path.GetFileName(tempScript)))
            Assert.That(lines.[1], Is.EqualTo("2"))
            Assert.That(lines.[2], Is.EqualTo("10"))
            Assert.That(lines.[3], Is.EqualTo("abc"))
            Assert.That(lines.[4], Is.EqualTo("()"))
        finally
            if File.Exists(tempScript) then File.Delete(tempScript)

    [<Test>]
    member _.``Stdin mode exposes Environment arguments with separator`` () =
        let repoRoot = findRepoRoot ()
        let source =
            "match Env.ScriptName with\n"
            + "| Some _ -> print \"unexpected\"\n"
            + "| None -> print \"none\"\n"
            + "for arg in Env.Arguments do\n"
            + "  print arg\n"
            + "()\n"

        let code, stdout, stderr = runCli repoRoot repoRoot [ "--"; "x"; "y" ] (Some source)
        let lines = stdout.Replace("\r\n", "\n").Trim().Split('\n')

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(lines.[0], Is.EqualTo("none"))
        Assert.That(lines.[1], Is.EqualTo("x"))
        Assert.That(lines.[2], Is.EqualTo("y"))
        Assert.That(lines.[3], Is.EqualTo("()"))

    [<Test>]
    member _.``Script arguments require separator`` () =
        let repoRoot = findRepoRoot ()
        let tempScript = Path.Combine(Path.GetTempPath(), $"fscript-cli-separator-{Guid.NewGuid():N}.fss")

        try
            File.WriteAllText(tempScript, "1")
            let code, _, stderr = runCli repoRoot repoRoot [ tempScript; "10" ] None

            Assert.That(code, Is.Not.EqualTo(0))
            Assert.That(stderr, Does.Contain("parameter '<path>' should appear after all other arguments"))
        finally
            if File.Exists(tempScript) then File.Delete(tempScript)
