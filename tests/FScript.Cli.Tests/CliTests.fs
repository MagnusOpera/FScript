namespace FScript.Cli.Tests

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open FScript.Runtime
open FScript.Cli.Tests.Fixtures.Valid
open FScript.Cli.Tests.Fixtures.Conflict
open FScript.Cli.Tests.Fixtures.Invalid
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

    let validProviderAssemblyPath () =
        typeof<ValidExternProvider>.Assembly.Location

    let conflictProviderAssemblyPath () =
        typeof<ConflictExternProvider>.Assembly.Location

    let invalidProviderAssemblyPath () =
        typeof<InvalidExternProvider>.Assembly.Location

    let basicSampleScriptPath (repoRoot: string) =
        Path.Combine(repoRoot, "samples", "basic", "basic.fss")

    let basicSampleDirectory (repoRoot: string) =
        Path.Combine(repoRoot, "samples", "basic")

    [<Test>]
    member _.``Provider binaries reference SDK without interpreter implementation`` () =
        let assembly = typeof<FScript.Cli.Tests.Fixtures.Valid.ValidExternProvider>.Assembly
        let references = assembly.GetReferencedAssemblies() |> Array.map (fun reference -> reference.Name)
        Assert.That(references, Does.Contain("FScript.Extensibility"))
        Assert.That(references, Does.Not.Contain("FScript.Language"))
        Assert.That(references, Does.Not.Contain("FScript.Runtime"))

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
    member _.``Compiles a whole program to versioned IR`` () =
        let repoRoot = findRepoRoot ()
        let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-compile-ir-{Guid.NewGuid():N}")
        let script = Path.Combine(tempRoot, "program.fss")
        let output = Path.Combine(tempRoot, "out")
        try
            Directory.CreateDirectory(tempRoot) |> ignore
            File.WriteAllText(script, "let add x y = x + y\nadd 1 2")
            let code, _, stderr = runCli repoRoot repoRoot [ "compile"; script; "--target"; "ir"; "--output"; output ] None
            Assert.That(code, Is.EqualTo(0), stderr)
            let artifact = Path.Combine(output, "program.fsir")
            Assert.That(File.Exists artifact, Is.True)
            let expected = File.ReadAllBytes(artifact)
            Assert.That(File.ReadAllText(artifact), Does.Contain("\"format\": \"fscript-lir\""))
            File.WriteAllText(artifact, "stale")
            let secondCode, _, secondError = runCli repoRoot repoRoot [ "compile"; script; "--target"; "ir"; "--output"; output ] None
            Assert.That(secondCode, Is.EqualTo(0), secondError)
            Assert.That(File.ReadAllBytes(artifact) = expected, Is.True, "IR compilation must deterministically replace an existing artifact")
        finally
            if Directory.Exists tempRoot then Directory.Delete(tempRoot, true)

    [<TestCase("dotnet")>]
    [<TestCase("csharp")>]
    member _.``Native targets produce independently runnable programs with FFI`` (target: string) =
        let repoRoot = findRepoRoot ()
        let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-native-cli-{Guid.NewGuid():N}")
        Directory.CreateDirectory(tempRoot) |> ignore
        try
            let script=Path.Combine(tempRoot,"native.fss")
            File.WriteAllText(script,"Console.writeLine (Int.toString Ext.answer)")
            let output=Path.Combine(tempRoot,"output")
            let code,_,error=runCli repoRoot tempRoot ["compile";script;"--target";target;"--output";output;"--no-result";"--extern-assembly";validProviderAssemblyPath()] None
            Assert.That(code,Is.Zero,error)
            let start=ProcessStartInfo("dotnet",WorkingDirectory=tempRoot,RedirectStandardOutput=true,RedirectStandardError=true)
            if target="dotnet" then start.ArgumentList.Add(Path.Combine(output,"native.dll"))
            else
                start.ArgumentList.Add("run")
                start.ArgumentList.Add("--project")
                start.ArgumentList.Add(Path.Combine(output,"Program.csproj"))
            use proc=new Process(StartInfo=start)
            proc.Start() |> ignore
            let stdout=proc.StandardOutput.ReadToEnd()
            let stderr=proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Assert.That(proc.ExitCode,Is.Zero,stdout+stderr)
            Assert.That(stdout.Trim(),Is.EqualTo("42"))
            Assert.That(File.Exists(Path.Combine(output,"FScript.Runtime.dll")),Is.False)
        finally Directory.Delete(tempRoot,true)

    [<Test>]
    member _.``Pure provider calls and their dependencies are pruned from native output`` () =
        let root=findRepoRoot ()
        let directory=Path.Combine(Path.GetTempPath(),"fscript-pure-provider-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(directory) |> ignore
        try
            let script=Path.Combine(directory,"pure.fss")
            File.WriteAllText(script,"let discarded = Ext.readCounter (Ext.createCounter 7)\n42")
            let output=Path.Combine(directory,"output")
            let args=["compile";script;"--target";"dotnet";"--output";output;"--no-result";"--extern-assembly";validProviderAssemblyPath()]
            let code,_,error=runCli root directory args None
            Assert.That(code,Is.Zero,error)
            Assert.That(Directory.GetFiles(output) |> Array.map Path.GetFileName,Is.EquivalentTo([|"pure.dll";"pure.runtimeconfig.json"|]))
            let start=ProcessStartInfo("dotnet",RedirectStandardOutput=true,RedirectStandardError=true)
            start.ArgumentList.Add(Path.Combine(output,"pure.dll"))
            use proc=new Process(StartInfo=start)
            proc.Start() |> ignore
            let stdout=proc.StandardOutput.ReadToEnd()
            let stderr=proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Assert.That(proc.ExitCode,Is.Zero,stderr)
            Assert.That(stdout,Is.Empty)
            let irOutput=Path.Combine(directory,"ir")
            let code,_,error=runCli root directory ["compile";script;"--target";"ir";"--output";irOutput;"--extern-assembly";validProviderAssemblyPath()] None
            Assert.That(code,Is.Zero,error)
            Assert.That(File.ReadAllText(Path.Combine(irOutput,"pure.fsir")),Does.Contain("\"pure\": true"))
        finally Directory.Delete(directory,true)

    [<TestCase("unknown")>]
    [<TestCase("go")>]
    [<TestCase("GO")>]
    member _.``Invalid compile target does not create output`` (target: string) =
        let repoRoot = findRepoRoot ()
        let tempRoot = Path.Combine(Path.GetTempPath(), $"fscript-compile-invalid-{Guid.NewGuid():N}")
        let script = Path.Combine(tempRoot, "program.fss")
        let output = Path.Combine(tempRoot, "out")
        try
            Directory.CreateDirectory(tempRoot) |> ignore
            File.WriteAllText(script, "1")
            let code, _, stderr = runCli repoRoot repoRoot [ "compile"; script; "--target"; target; "--output"; output ] None
            Assert.That(code, Is.EqualTo(1))
            Assert.That(stderr, Does.Contain("Usage: fscript compile"))
            Assert.That(Directory.Exists(output), Is.False)
        finally
            if Directory.Exists tempRoot then Directory.Delete(tempRoot, true)

    [<TestCase("csharp", "--single-file")>]
    [<TestCase("csharp", "--aot")>]
    [<TestCase("ir", "--single-file")>]
    [<TestCase("dotnet", "--runtime")>]
    member _.``Executable options reject invalid combinations before creating output`` (target: string, flag: string) =
        let root=findRepoRoot ()
        let output=Path.Combine(Path.GetTempPath(),"fscript-invalid-publish-"+Guid.NewGuid().ToString("N"))
        let code,_,error=runCli root root ["compile";Path.Combine(root,"samples","hello.fss");"--target";target;"--output";output;flag] None
        Assert.That(code,Is.EqualTo(1),error)
        Assert.That(Directory.Exists output,Is.False)

    [<TestCase("--single-file")>]
    [<TestCase("--aot")>]
    member _.``Explicit runtime options still require a value`` flag =
        let root=findRepoRoot ()
        let output=Path.Combine(Path.GetTempPath(),"fscript-missing-rid-"+Guid.NewGuid().ToString("N"))
        let code,_,error=runCli root root ["compile";Path.Combine(root,"samples","hello.fss");"--target";"dotnet";"--output";output;flag;"--runtime"] None
        Assert.That(code,Is.EqualTo(1),error)
        Assert.That(error,Does.Contain("--runtime requires a value"))
        Assert.That(Directory.Exists output,Is.False)

    [<TestCase("darwin-arm64", false)>]
    [<TestCase("darwin-arm64", true)>]
    [<TestCase("unknown-rid", false)>]
    member _.``Invalid runtime identifiers report a FScript diagnostic without SDK publishing`` (rid: string, aot: bool) =
        let root=findRepoRoot ()
        let output=Path.Combine(Path.GetTempPath(),"fscript-invalid-rid-"+Guid.NewGuid().ToString("N"))
        try
            let flags=if aot then ["--single-file";"--aot"] else ["--single-file"]
            let code,_,error=runCli root root (["compile";Path.Combine(root,"samples","hello.fss");"--target";"dotnet";"--output";output;"--runtime";rid] @ flags) None
            Assert.That(code,Is.EqualTo(4),error)
            Assert.That(error,Does.Contain("FSN7101"))
            Assert.That(error,Does.Contain($"'{rid}'"))
            Assert.That(error,Does.Contain("osx-arm64"))
            Assert.That(error,Does.Not.Contain("NETSDK"))
            Assert.That((if Directory.Exists output then Directory.GetFiles(output) else [||]),Is.Empty)
        finally
            if Directory.Exists output then Directory.Delete(output,true)

    [<TestCase("--single-file", false)>]
    [<TestCase("--aot", false)>]
    [<TestCase("--single-file", true)>]
    [<TestCase("--aot", true)>]
    member _.``Self contained executables support default and explicit runtimes and run alone`` (flag: string, ffi: bool) =
        let root=findRepoRoot ()
        let directory=Path.Combine(Path.GetTempPath(),"fscript-contained-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(directory) |> ignore
        try
            let script=Path.Combine(directory,"native.fss")
            let output=Path.Combine(directory,"output")
            let standalone=Path.Combine(directory,"standalone")
            Directory.CreateDirectory(standalone) |> ignore
            let source=if ffi then "Console.writeLine (Int.toString (Ext.readCounter (Ext.createCounter (Ext.answer - 1)) + 1))\n42" else "Console.writeLine (Option.defaultValue \"missing\" (Json.serialize \"hello\"))\nConsole.writeLine (Option.defaultValue \"missing\" Env.ScriptName)\nConsole.writeLine $\"{Env.Arguments}\""
            File.WriteAllText(script,"Env.setExitCode 7\n"+source)
            let runtimeArgs=if ffi then ["--runtime";System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier] else []
            let args=["compile";script;"--target";"dotnet";"--output";output;"--no-result";flag] @ runtimeArgs @ (if ffi then ["--extern-assembly";validProviderAssemblyPath()] else [])
            let code,_,error=runCli root directory args None
            Assert.That(code,Is.Zero,error)
            let name="native"+(if OperatingSystem.IsWindows() then ".exe" else "")
            let files=Directory.GetFiles(output)
            Assert.That(files.Length,Is.EqualTo(1))
            let executable=Path.Combine(standalone,name)
            File.Copy(Path.Combine(output,name),executable)
            if not (OperatingSystem.IsWindows()) then File.SetUnixFileMode(executable,File.GetUnixFileMode(Path.Combine(output,name)))
            Directory.Delete(output,true)
            File.Delete(script)
            let start=ProcessStartInfo(executable,WorkingDirectory=standalone,RedirectStandardOutput=true,RedirectStandardError=true)
            start.ArgumentList.Add("hello")
            let missingRuntime=Path.Combine(directory,"missing-runtime")
            start.Environment.["DOTNET_ROOT"] <- missingRuntime
            start.Environment.["DOTNET_ROOT_ARM64"] <- missingRuntime
            start.Environment.["DOTNET_ROOT_X64"] <- missingRuntime
            start.Environment.["DOTNET_MULTILEVEL_LOOKUP"] <- "0"
            use proc=new Process(StartInfo=start)
            proc.Start() |> ignore
            let stdout=proc.StandardOutput.ReadToEnd()
            let stderr=proc.StandardError.ReadToEnd()
            Assert.That(proc.WaitForExit(60000),Is.True)
            Assert.That(proc.ExitCode,Is.EqualTo(7),stdout+stderr)
            let expected=if ffi then "42" else "\"hello\"\n"+name+"\n[hello]"
            Assert.That(stdout.Replace("\r\n","\n").Trim(),Is.EqualTo(expected))
        finally Directory.Delete(directory,true)

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
            + "| Some name -> Console.writeLine name\n"
            + "| None -> Console.writeLine \"none\"\n"
            + "type Wrapper = { Value: Environment }\n"
            + "let wrapped = { Value = Env }\n"
            + "Console.writeLine $\"{wrapped.Value.Arguments |> List.length}\"\n"
            + "for arg in Env.Arguments do\n"
            + "  Console.writeLine arg\n"
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
            + "| Some _ -> Console.writeLine \"unexpected\"\n"
            + "| None -> Console.writeLine \"none\"\n"
            + "for arg in Env.Arguments do\n"
            + "  Console.writeLine arg\n"
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

    [<Test>]
    member _.``No-default-externs disables runtime Registry externs`` () =
        let repoRoot = findRepoRoot ()
        let code, _, stderr = runCli repoRoot repoRoot [ "--no-default-externs" ] (Some "Fs.exists \"a.txt\"")

        Assert.That(code, Is.Not.EqualTo(0))
        Assert.That(stderr, Does.Contain("Unbound variable 'Fs'"))

    [<Test>]
    member _.``No-default-externs disables Task externs`` () =
        let repoRoot = findRepoRoot ()
        let code, _, stderr = runCli repoRoot repoRoot [ "--no-default-externs" ] (Some "Task.spawn (fun () -> 1)")

        Assert.That(code, Is.Not.EqualTo(0))
        Assert.That(stderr, Does.Contain("Unbound variable 'Task'"))

    [<Test>]
    member _.``Loads extern provider assembly and invokes exported extern`` () =
        let repoRoot = findRepoRoot ()
        let providerAssembly = validProviderAssemblyPath ()
        let code, stdout, stderr = runCli repoRoot repoRoot [ "--extern-assembly"; providerAssembly ] (Some "Ext.answer")

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(stdout.Trim(), Is.EqualTo("42"))

    [<Test>]
    member _.``Dedupes duplicate extern assembly paths`` () =
        let repoRoot = findRepoRoot ()
        let providerAssembly = validProviderAssemblyPath ()
        let code, stdout, stderr =
            runCli repoRoot repoRoot [ "--extern-assembly"; providerAssembly; "--extern-assembly"; providerAssembly ] (Some "Ext.answer")

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(stdout.Trim(), Is.EqualTo("42"))

    [<Test>]
    member _.``Supports extern provider methods that accept HostContext`` () =
        let repoRoot = findRepoRoot ()
        let providerAssembly = validProviderAssemblyPath ()
        let code, stdout, stderr = runCli repoRoot repoRoot [ "--extern-assembly"; providerAssembly ] (Some "Ext.rootDirectory")

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(stdout.Trim(), Is.EqualTo($"\"{repoRoot}\""))

    [<Test>]
    member _.``Fails fast on extern name conflicts with defaults`` () =
        let repoRoot = findRepoRoot ()
        let providerAssembly = conflictProviderAssemblyPath ()
        let code, _, stderr =
            runCli repoRoot repoRoot [ "--extern-assembly"; providerAssembly ] (Some "1")

        Assert.That(code, Is.Not.EqualTo(0))
        Assert.That(stderr, Does.Contain("External function name conflicts detected"))
        Assert.That(stderr, Does.Contain("Fs.exists"))

    [<Test>]
    member _.``Fails when extern provider signature is invalid`` () =
        let repoRoot = findRepoRoot ()
        let providerAssembly = invalidProviderAssemblyPath ()
        let code, _, stderr =
            runCli repoRoot repoRoot [ "--extern-assembly"; providerAssembly ] (Some "1")

        Assert.That(code, Is.Not.EqualTo(0))
        Assert.That(stderr, Does.Contain("Invalid extern provider"))

    [<Test>]
    member _.``Fails when extern assembly path does not exist`` () =
        let repoRoot = findRepoRoot ()
        let bogusPath = Path.Combine(Path.GetTempPath(), $"fscript-missing-{Guid.NewGuid():N}.dll")
        let code, _, stderr = runCli repoRoot repoRoot [ "--extern-assembly"; bogusPath ] (Some "1")

        Assert.That(code, Is.Not.EqualTo(0))
        Assert.That(stderr, Does.Contain("Extern assembly not found"))

    [<Test>]
    member _.``Fails when assembly has no extern providers`` () =
        let repoRoot = findRepoRoot ()
        let runtimeAssemblyPath = typeof<HostContext>.Assembly.Location
        let code, _, stderr = runCli repoRoot repoRoot [ "--extern-assembly"; runtimeAssemblyPath ] (Some "1")

        Assert.That(code, Is.Not.EqualTo(0))
        Assert.That(stderr, Does.Contain("No [<FScriptExternProvider>] methods found"))

    [<Test>]
    member _.``Basic sample runs hello world program`` () =
        let repoRoot = findRepoRoot ()
        let scriptPath = basicSampleScriptPath repoRoot
        let code, stdout, stderr = runCli repoRoot repoRoot [ scriptPath; "--"; "helloworld.bas" ] None
        let normalized = stdout.Replace("\r\n", "\n").Trim()

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(normalized, Is.EqualTo("HELLO WORLD\n()"))

    [<Test>]
    member _.``Basic sample runs control flow program`` () =
        let repoRoot = findRepoRoot ()
        let scriptPath = basicSampleScriptPath repoRoot
        let code, stdout, stderr = runCli repoRoot repoRoot [ scriptPath; "--"; "controlflow.bas" ] None
        let normalized = stdout.Replace("\r\n", "\n").Trim()

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(normalized, Is.EqualTo("6\nDONE\n()"))

    [<Test>]
    member _.``Basic sample requires a BASIC file argument`` () =
        let repoRoot = findRepoRoot ()
        let scriptPath = basicSampleScriptPath repoRoot
        let code, _, stderr = runCli repoRoot repoRoot [ scriptPath ] None

        Assert.That(code, Is.EqualTo(3))
        Assert.That(stderr, Does.Contain("Usage: basic.fss <program.bas>"))
        Assert.That(stderr, Does.Not.Contain("Usage: fscript"))

    [<Test>]
    member _.``Compiled BASIC usage identifies its standalone assembly`` () =
        let repoRoot=findRepoRoot ()
        let output=Path.Combine(Path.GetTempPath(),"fscript-basic-native-"+Guid.NewGuid().ToString("N"))
        try
            let code,_,error=runCli repoRoot repoRoot ["compile";basicSampleScriptPath repoRoot;"--target";"dotnet";"--output";output] None
            Assert.That(code,Is.Zero,error)
            let start=ProcessStartInfo("dotnet",WorkingDirectory=repoRoot,RedirectStandardOutput=true,RedirectStandardError=true)
            start.ArgumentList.Add(Path.Combine(output,"basic.dll"))
            use proc=new Process(StartInfo=start)
            proc.Start() |> ignore
            let stdout=proc.StandardOutput.ReadToEnd()
            let stderr=proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Assert.That(proc.ExitCode,Is.Not.Zero,stdout+stderr)
            Assert.That(stderr,Does.Contain("Usage: basic.dll <program.bas>"))
            Assert.That(stderr,Does.Not.Contain("Usage: fscript"))
        finally
            if Directory.Exists(output) then Directory.Delete(output,true)

    [<Test>]
    member _.``Basic sample reports invalid BASIC source`` () =
        let repoRoot = findRepoRoot ()
        let scriptPath = basicSampleScriptPath repoRoot
        let sampleDir = basicSampleDirectory repoRoot
        let invalidProgramName = $"invalid-{Guid.NewGuid():N}.bas"
        let invalidProgramPath = Path.Combine(sampleDir, invalidProgramName)

        try
            File.WriteAllText(invalidProgramPath, "10 IF 1 THEN\n20 END")
            let code, _, stderr = runCli repoRoot repoRoot [ scriptPath; "--"; invalidProgramName ] None

            Assert.That(code, Is.EqualTo(3))
            Assert.That(stderr, Does.Contain("Invalid BASIC line 10"))
        finally
            if File.Exists(invalidProgramPath) then File.Delete(invalidProgramPath)

    [<Test>]
    member _.``Basic sample supports INPUT through Console.readLine`` () =
        let repoRoot = findRepoRoot ()
        let scriptPath = basicSampleScriptPath repoRoot
        let code, stdout, stderr = runCli repoRoot repoRoot [ scriptPath; "--"; "input.bas" ] (Some "Ada\n")
        let normalized = stdout.Replace("\r\n", "\n").Trim()

        Assert.That(code, Is.EqualTo(0), $"stderr: {stderr}")
        Assert.That(normalized, Is.EqualTo("WHAT IS YOUR NAME?\nHELLO Ada\n()"))

    [<Test>]
    member _.``Provider unions support qualified construction and matching in CLI`` () =
        let repoRoot = findRepoRoot ()
        let source = "match Ext.mode (Protocol.Mode.Enabled 42) with | Enabled n -> n | Disabled -> 0"
        let code, stdout, stderr = runCli repoRoot repoRoot ["--extern-assembly"; validProviderAssemblyPath ()] (Some source)
        Assert.That(code, Is.EqualTo(0), stderr)
        Assert.That(stdout.Trim(), Is.EqualTo("42"))

    [<Test>]
    member _.``Host-managed extension works in stdin mode`` () =
        let repoRoot = findRepoRoot ()
        let code, stdout, stderr = runCli repoRoot repoRoot ["--extern-assembly"; validProviderAssemblyPath ()] (Some "let retain x =\n    let v = x.Value\n    x\nExt.acceptNode (retain (Ext.node 42))")
        Assert.That(code, Is.EqualTo(0), stderr)
        Assert.That(stdout.Trim(), Is.EqualTo("42"))

    [<Test>]
    member _.``Host-managed extension works in file mode and rejects construction`` () =
        let repoRoot = findRepoRoot ()
        let path = Path.Combine(Path.GetTempPath(), $"fscript-host-managed-{Guid.NewGuid():N}.fss")
        try
            File.WriteAllText(path, "Ext.acceptNode (Ext.node 42)")
            let code, stdout, stderr = runCli repoRoot repoRoot ["--extern-assembly"; validProviderAssemblyPath (); path] None
            Assert.That(code, Is.EqualTo(0), stderr)
            Assert.That(stdout.Trim(), Is.EqualTo("42"))
            File.WriteAllText(path, "Ext.acceptNode {| Value = 42 |}")
            let code, _, stderr = runCli repoRoot repoRoot ["--extern-assembly"; validProviderAssemblyPath (); path] None
            Assert.That(code, Is.EqualTo(2), stderr)
        finally
            if File.Exists path then File.Delete path

    [<Test>]
    member _.``Configuration loads providers in file and stdin modes from a different directory`` () =
        let repo = findRepoRoot ()
        let dir = Path.Combine(Path.GetTempPath(), $"fscript-environment-{Guid.NewGuid():N}")
        Directory.CreateDirectory(dir) |> ignore
        try
            let manifest = Path.Combine(dir, "fscript.toml")
            let assembly = validProviderAssemblyPath ()
            let relativeAssembly = Path.GetRelativePath(dir, assembly).Replace("\\", "/")
            File.WriteAllText(manifest, $"[runtime]\nroot = '.'\nextern-assemblies = ['{relativeAssembly}']")
            let script = Path.Combine(dir, "script.fss")
            File.WriteAllText(script, "Ext.acceptNode (Ext.node Ext.answer)")
            for args, input in [ ["--config"; manifest; script], None
                                 ["-c"; manifest], Some "Ext.acceptNode (Ext.node Ext.answer)"
                                 ["-c"; manifest; "--extern-assembly"; assembly], Some "Ext.answer" ] do
                let code, output, error = runCli repo repo args input
                Assert.That(code, Is.EqualTo(0), error)
                Assert.That(output.Trim(), Is.EqualTo("42"))
            let code, output, error = runCli repo repo ["-c"; manifest] (Some "Ext.rootDirectory")
            Assert.That(code, Is.EqualTo(0), error)
            Assert.That(output, Does.Contain(dir))
            let code, output, error = runCli repo repo ["-c"; manifest; "--root"; repo] (Some "Ext.rootDirectory")
            Assert.That(code, Is.EqualTo(0), error)
            Assert.That(output, Does.Contain(repo))
        finally Directory.Delete(dir, true)

    [<Test>]
    member _.``Discovered invalid manifests fail before execution and can be disabled`` () =
        let repo = findRepoRoot ()
        let dir = Path.Combine(Path.GetTempPath(), $"fscript-environment-{Guid.NewGuid():N}")
        Directory.CreateDirectory(dir) |> ignore
        try
            let manifest = Path.Combine(dir, "fscript.toml")
            File.WriteAllText(manifest, "[project]\nentry = 'unused.fss'")
            let code, output, error = runCli repo dir [] (Some "42")
            Assert.That(code, Is.Not.EqualTo(0))
            Assert.That(output, Is.Empty)
            Assert.That(error, Does.Contain("project"))
            let code, output, error = runCli repo dir ["--no-config"] (Some "42")
            Assert.That(code, Is.EqualTo(0), error)
            Assert.That(output.Trim(), Is.EqualTo("42"))
            let code, _, error = runCli repo dir ["--config"; manifest] (Some "42")
            Assert.That(code, Is.Not.EqualTo(0))
            Assert.That(error, Does.Contain("project"))
            File.WriteAllText(manifest, "[runtime]\ndefault-externs = false")
            let code, _, error = runCli repo dir ["-c"; manifest] (Some "Console.writeLine \"test\"")
            Assert.That(code, Is.Not.EqualTo(0))
            Assert.That(error, Does.Contain("Console"))
        finally Directory.Delete(dir, true)

    [<Test>]
    member _.``Provider data records can be constructed and updated by scripts`` () =
        let repo = findRepoRoot ()
        let source = "let point = { X = 1 }\nExt.acceptPoint { point with X = 42 }"
        let code, output, error = runCli repo repo ["--extern-assembly"; validProviderAssemblyPath ()] (Some source)
        Assert.That(code, Is.EqualTo(0), error)
        Assert.That(output.Trim(), Is.EqualTo("42"))

    [<Test>]
    member _.``Automatic configuration is beside the script or in stdin cwd`` () =
        let repo = findRepoRoot ()
        let dir = Path.Combine(Path.GetTempPath(), $"fscript-discovery-{Guid.NewGuid():N}")
        Directory.CreateDirectory(dir) |> ignore
        try
            let manifest = Path.Combine(dir, "fscript.toml")
            File.Copy(validProviderAssemblyPath (), Path.Combine(dir, "provider.dll"))
            File.WriteAllText(manifest, "[runtime]\nextern-assemblies = ['provider.dll']")
            let script = Path.Combine(dir, "main.fss")
            File.WriteAllText(script, "Ext.acceptNode (Ext.node Ext.answer)")
            for cwd, args, input in [repo, [script], None; dir, [], Some "Ext.answer"] do
                let code, output, error = runCli repo cwd args input
                Assert.That(code, Is.Zero, error)
                Assert.That(output.Trim(), Is.EqualTo "42")
            let code, _, error = runCli repo repo ["--no-config"; script] None
            Assert.That(code, Is.EqualTo 2)
            Assert.That(error, Does.Contain "Ext")
            let overrideManifest = Path.Combine(dir, "override.toml")
            File.WriteAllText(overrideManifest, "[runtime]\ndefault-externs = true")
            File.WriteAllText(manifest, "[broken]")
            File.WriteAllText(script, "42")
            let code, output, error = runCli repo repo [script] None
            Assert.That(code, Is.Not.Zero)
            Assert.That(output, Is.Empty)
            Assert.That(error, Does.Contain "broken")
            let code, output, error = runCli repo repo ["-c"; overrideManifest; script] None
            Assert.That(code, Is.Zero, error)
            Assert.That(output.Trim(), Is.EqualTo "42")
            let code, _, error = runCli repo dir ["-c"; overrideManifest; "--no-config"] (Some "42")
            Assert.That(code, Is.Not.Zero)
            Assert.That(error, Does.Contain "cannot be used together")
        finally Directory.Delete(dir, true)

    [<Test>]
    member _.``File discovery ignores parent and working directory manifests`` () =
        let repo = findRepoRoot ()
        let dir = Path.Combine(Path.GetTempPath(), $"fscript-discovery-{Guid.NewGuid():N}")
        let nested = Path.Combine(dir, "scripts")
        Directory.CreateDirectory(nested) |> ignore
        try
            File.WriteAllText(Path.Combine(dir, "fscript.toml"), "[broken]")
            let script = Path.Combine(nested, "main.fss")
            File.WriteAllText(script, "42")
            let code, output, error = runCli repo dir [script] None
            Assert.That(code, Is.Zero, error)
            Assert.That(output.Trim(), Is.EqualTo "42")
        finally Directory.Delete(dir, true)

    [<Test>]
    member _.``Imported modules do not discover their own environments`` () =
        let repo = findRepoRoot ()
        let dir = Path.Combine(Path.GetTempPath(), $"fscript-import-discovery-{Guid.NewGuid():N}")
        let nested = Path.Combine(dir, "module")
        Directory.CreateDirectory(nested) |> ignore
        try
            File.WriteAllText(Path.Combine(nested, "fscript.toml"), "[broken]")
            File.WriteAllText(Path.Combine(nested, "helper.fss"), "let answer = 42")
            let script = Path.Combine(dir, "main.fss")
            File.WriteAllText(script, "import \"module/helper.fss\" as Helper\nHelper.answer")
            let code, output, error = runCli repo repo [script] None
            Assert.That(code, Is.Zero, error)
            Assert.That(output.Trim(), Is.EqualTo "42")
        finally Directory.Delete(dir, true)

    [<Test>]
    member _.``NuGet configuration loads attributed providers in file and stdin modes`` () =
        use fixture = new NuGetFixture()
        fixture.AddProvider "1.0.0"
        fixture.WriteManifest "1.0.0"
        let repo = findRepoRoot ()
        let script = Path.Combine(fixture.Directory, "main.fss")
        File.WriteAllText(script, "Ext.acceptNode (Ext.node 42)")
        for args, input in [ [script], None; [], Some "Ext.answer" ] do
            let code, output, error = runCli repo fixture.Directory args input
            Assert.That(code, Is.EqualTo 0, error)
            Assert.That(output, Does.Contain("42"))
        fixture.WriteManifest "9.0.0"
        let code, output, error = runCli repo fixture.Directory [script] None
        Assert.That(code, Is.Not.EqualTo 0)
        Assert.That(error, Does.Contain("NuGet").And.Contain(fixture.Name))
        Assert.That(output, Does.Not.Contain("42"))

    [<Test>]
    member _.``Debugger capability probe is standalone and script arguments remain literal`` () =
        let repo = findRepoRoot ()
        let code, output, error = runCli repo repo ["--debug-capabilities"] None
        Assert.That(code,Is.Zero,error)
        Assert.That(output,Does.Contain("\"protocolVersion\":1"))
        let script = Path.Combine(Path.GetTempPath(), $"fscript-debug-args-{Guid.NewGuid():N}.fss")
        try
            File.WriteAllText(script,"Env.Arguments")
            let code, output, error = runCli repo repo [script;"--";"--debug-adapter"] None
            Assert.That(code,Is.Zero,error)
            Assert.That(output.Trim(),Is.EqualTo("[\"--debug-adapter\"]"))
        finally File.Delete script

    [<TestCase(false)>]
    [<TestCase(true)>]
    member _.``Interpreted status is independent of result display`` noResult =
        let root=findRepoRoot ()
        let arguments=if noResult then ["--no-result"] else []
        let code,output,error=runCli root root arguments (Some "Env.setExitCode 7\nEnv.setExitCode 9\nConsole.writeLine \"after\"\n42")
        Assert.That(code,Is.EqualTo(9),error)
        Assert.That(error,Is.Empty)
        Assert.That(output.Replace("\r\n","\n").Trim(),Is.EqualTo(if noResult then "after" else "after\n42"))

    [<TestCase("dotnet",false)>]
    [<TestCase("dotnet",true)>]
    [<TestCase("csharp",false)>]
    [<TestCase("csharp",true)>]
    member _.``Compiled status and result policy match scripting`` (target: string,noResult: bool) =
        let root=findRepoRoot ()
        let directory=Path.Combine(Path.GetTempPath(),"fscript-status-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(directory) |> ignore
        try
            let script=Path.Combine(directory,"status.fss")
            File.WriteAllText(script,"Env.setExitCode 7\nConsole.writeLine \"after\"\n42")
            let output=Path.Combine(directory,"output")
            let flags=if noResult then ["--no-result"] else []
            let code,_,error=runCli root directory (["compile";script;"--target";target;"--output";output] @ flags) None
            Assert.That(code,Is.Zero,error)
            let start=ProcessStartInfo("dotnet",WorkingDirectory=directory,RedirectStandardOutput=true,RedirectStandardError=true)
            let args=if target="dotnet" then [Path.Combine(output,"status.dll")] else ["run";"--project";Path.Combine(output,"Program.csproj")]
            for argument in args do start.ArgumentList.Add(argument)
            use proc=Process.Start(start) |> nonNull
            let stdout=proc.StandardOutput.ReadToEndAsync()
            let stderr=proc.StandardError.ReadToEndAsync()
            Assert.That(proc.WaitForExit(60000),Is.True)
            Assert.That(proc.ExitCode,Is.EqualTo(7),stderr.Result)
            Assert.That(stdout.Result.Replace("\r\n","\n").Trim(),Is.EqualTo(if noResult then "after" else "after\n42"))
        finally Directory.Delete(directory,true)

    [<Test>]
    member _.``Quiet display does not suppress failure or replace failure with selected status`` () =
        let root=findRepoRoot ()
        let code,output,error=runCli root root ["--no-result"] (Some "Env.setExitCode 7\nraise \"boom\"")
        Assert.That(code,Is.EqualTo(3))
        Assert.That(output,Is.Empty)
        Assert.That(error,Does.Contain("boom"))

    [<Test>]
    member _.``No-result is a build option and is not accepted for IR artifacts`` () =
        let root=findRepoRoot ()
        let output=Path.Combine(Path.GetTempPath(),"fscript-no-result-ir-"+Guid.NewGuid().ToString("N"))
        let code,_,error=runCli root root ["compile";Path.Combine(root,"samples","hello.fss");"--target";"ir";"--no-result";"--output";output] None
        Assert.That(code,Is.EqualTo(1),error)
        Assert.That(Directory.Exists output,Is.False)
