namespace FScript.TypeProvider.Tests

open System
open System.Diagnostics
open System.IO
open System.Text
open NUnit.Framework

type ProcessRunResult =
    { ExitCode: int
      Stdout: string
      Stderr: string
      Duration: TimeSpan
      TimedOut: bool }

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

    let runProcess
        (workingDirectory: string)
        (fileName: string)
        (arguments: string)
        (timeout: TimeSpan)
        : ProcessRunResult =
        let stdout = StringBuilder()
        let stderr = StringBuilder()
        let startInfo =
            ProcessStartInfo(
                FileName = fileName,
                Arguments = arguments,
                WorkingDirectory = workingDirectory,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false)

        use outputClosed = new System.Threading.ManualResetEventSlim(false)
        use errorClosed = new System.Threading.ManualResetEventSlim(false)

        use proc = new Process(StartInfo = startInfo)
        proc.OutputDataReceived.Add(fun args ->
            match args.Data with
            | null -> outputClosed.Set()
            | value -> stdout.AppendLine(value) |> ignore)
        proc.ErrorDataReceived.Add(fun args ->
            match args.Data with
            | null -> errorClosed.Set()
            | value -> stderr.AppendLine(value) |> ignore)

        let startedAt = DateTimeOffset.UtcNow
        proc.Start() |> ignore
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()

        let completed = proc.WaitForExit(int timeout.TotalMilliseconds)
        if not completed then
            try
                proc.Kill(true)
            with _ -> ()
            proc.WaitForExit()

        outputClosed.Wait(TimeSpan.FromSeconds(5.0)) |> ignore
        errorClosed.Wait(TimeSpan.FromSeconds(5.0)) |> ignore
        let endedAt = DateTimeOffset.UtcNow

        { ExitCode = proc.ExitCode
          Stdout = stdout.ToString()
          Stderr = stderr.ToString()
          Duration = endedAt - startedAt
          TimedOut = not completed }

    let formatResult (commandLine: string) (result: ProcessRunResult) =
        let output = result.Stdout + "\n" + result.Stderr
        let trimmedOutput =
            if output.Length > 12000 then
                output.Substring(0, 12000) + "\n...[truncated]..."
            else
                output
        $"Command: {commandLine}\nExitCode: {result.ExitCode}\nTimedOut: {result.TimedOut}\nDuration: {result.Duration}\nOutput:\n{trimmedOutput}"

    let buildCommand fixturePath =
        $"build \"{fixturePath}\" -c Release --disable-build-servers /p:UseSharedCompilation=false /nodeReuse:false"

    [<Test>]
    member _.``type provider builds valid script fixture`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.Valid")
        let command = buildCommand fixturePath
        let commandLine = "dotnet " + command
        let result = runProcess repoRoot "dotnet" command (TimeSpan.FromSeconds(120.0))
        let detail = formatResult commandLine result
        Assert.That(result.TimedOut, Is.False, $"Build command timed out.\n{detail}")
        Assert.That(result.ExitCode, Is.EqualTo(0), $"Expected build success.\n{detail}")

    [<Test>]
    member _.``type provider fails compilation on script type error`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.Invalid")
        let command = buildCommand fixturePath
        let commandLine = "dotnet " + command
        let result = runProcess repoRoot "dotnet" command (TimeSpan.FromSeconds(120.0))
        let output = result.Stdout + "\n" + result.Stderr
        let detail = formatResult commandLine result
        Assert.That(result.TimedOut, Is.False, $"Build command timed out.\n{detail}")
        Assert.That(result.ExitCode, Is.Not.EqualTo(0), $"Expected build failure for invalid script.\n{detail}")
        Assert.That(output, Does.Contain("Failed to type-check FScript"), detail)

    [<Test>]
    member _.``type provider fails unsupported exported signature`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.Unsupported")
        let command = buildCommand fixturePath
        let commandLine = "dotnet " + command
        let result = runProcess repoRoot "dotnet" command (TimeSpan.FromSeconds(120.0))
        let output = result.Stdout + "\n" + result.Stderr
        let detail = formatResult commandLine result
        Assert.That(result.TimedOut, Is.False, $"Build command timed out.\n{detail}")
        Assert.That(result.ExitCode, Is.Not.EqualTo(0), $"Expected build failure for unsupported signature.\n{detail}")
        Assert.That(output, Does.Contain("not supported in exported signatures"), detail)

    [<Test>]
    member _.``runtime resolver override can replace implementation and mismatch is rejected`` () =
        let repoRoot = findRepoRoot ()
        let fixturePath = Path.Combine(repoRoot, "tests", "FScript.TypeProvider.Tests.Fixtures.RuntimeOverride")
        let command = $"run --project \"{fixturePath}\" -c Release --disable-build-servers"
        let commandLine = "dotnet " + command
        let result = runProcess repoRoot "dotnet" command (TimeSpan.FromSeconds(180.0))
        let detail = formatResult commandLine result
        Assert.That(result.TimedOut, Is.False, $"Runtime command timed out.\n{detail}")
        Assert.That(result.ExitCode, Is.EqualTo(0), $"Expected runtime fixture success.\n{detail}")
