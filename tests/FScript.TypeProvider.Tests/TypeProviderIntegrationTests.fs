namespace FScript.TypeProvider.Tests

open System
open System.Diagnostics
open System.IO
open NUnit.Framework

[<TestFixture>]
type TypeProviderIntegrationTests () =
    let findRepoRoot () =
        let rec loop (path: string) =
            if String.IsNullOrWhiteSpace(path) then
                invalidOp "Unable to find repository root."
            elif File.Exists(Path.Combine(path, "FScript.sln")) then
                path
            else
                let parent = Directory.GetParent(path)
                match parent with
                | null -> invalidOp "Unable to find repository root."
                | value -> loop value.FullName

        loop TestContext.CurrentContext.TestDirectory

    let runProcess (workingDirectory: string) (fileName: string) (arguments: string) =
        let startInfo =
            ProcessStartInfo(
                FileName = fileName,
                Arguments = arguments,
                WorkingDirectory = workingDirectory,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false)

        use proc = new Process(StartInfo = startInfo)
        proc.Start() |> ignore
        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()
        proc.ExitCode, stdout, stderr

    [<Test>]
    member _.``type provider builds valid script fixture`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.Valid")
        let exitCode, stdout, stderr = runProcess repoRoot "dotnet" $"build \"{fixturePath}\" -c Release"
        let output = stdout + "\n" + stderr
        Assert.That(exitCode, Is.EqualTo(0), $"Expected build success. Output:\n{output}")

    [<Test>]
    member _.``type provider fails compilation on script type error`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.Invalid")
        let exitCode, stdout, stderr = runProcess repoRoot "dotnet" $"build \"{fixturePath}\" -c Release"
        let output = stdout + "\n" + stderr
        Assert.That(exitCode, Is.Not.EqualTo(0), "Expected build failure for invalid script.")
        Assert.That(output, Does.Contain("Failed to type-check FScript"))

    [<Test>]
    member _.``type provider fails unsupported exported signature`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.Unsupported")
        let exitCode, stdout, stderr = runProcess repoRoot "dotnet" $"build \"{fixturePath}\" -c Release"
        let output = stdout + "\n" + stderr
        Assert.That(exitCode, Is.Not.EqualTo(0), "Expected build failure for unsupported signature.")
        Assert.That(output, Does.Contain("not supported in exported signatures"))

    [<Test>]
    member _.``runtime resolver override can replace implementation and mismatch is rejected`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.RuntimeOverride")
        let exitCode, stdout, stderr = runProcess repoRoot "dotnet" $"run --project \"{fixturePath}\" -c Release"
        Assert.That(exitCode, Is.EqualTo(0), $"Expected runtime fixture success.\nStdout:\n{stdout}\nStderr:\n{stderr}")
