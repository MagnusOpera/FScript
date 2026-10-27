namespace FScript.Cli.Tests

open System
open System.IO
open NUnit.Framework
open FScript.Runtime

[<TestFixture>]
type CompileOptionsTests() =
    let script = Path.Combine(Path.GetTempPath(), "fscript-options", "main.fss")
    let defaults = { CompileConfiguration.empty with Backend=Some "dotnet"; AheadOfTime=Some true; MinimizeSize=Some true; RuntimeIdentifier=Some "linux-x64" }
    let resolve config flags =
        CompileOptions.resolve (Array.ofList ("compile" :: script :: flags)) script config
        |> function Ok value -> value | Error message -> failwith message

    [<Test>]
    member _.``Empty compile settings default to managed dotnet beside the script``() =
        let result = resolve CompileConfiguration.empty []
        Assert.That(result.Backend, Is.EqualTo("dotnet"))
        Assert.That(result.AheadOfTime, Is.False)
        Assert.That(result.OutputDirectory, Is.EqualTo(Path.Combine(Path.GetDirectoryName(script) |> nonNull, "bin", "main", "dotnet")))

    [<Test>]
    member _.``Configured native target and output convention use the resolved runtime``() =
        let result = resolve defaults []
        Assert.That(result.AheadOfTime && result.MinimizeSize, Is.True)
        Assert.That(result.RuntimeIdentifier, Is.EqualTo(Some "linux-x64"))
        Assert.That(result.OutputDirectory, Does.EndWith(Path.Combine("bin", "main", "dotnet", "linux-x64", "aot")))
        let explicitRuntime = resolve defaults ["--runtime"; "osx-arm64"]
        Assert.That(explicitRuntime.OutputDirectory, Does.EndWith(Path.Combine("osx-arm64", "aot")))

    [<Test>]
    member _.``CLI can disable inherited AOT and its size profile``() =
        let result = resolve defaults ["--no-aot"]
        Assert.That(result.AheadOfTime || result.MinimizeSize, Is.False)
        Assert.That(result.RuntimeIdentifier, Is.EqualTo(None))
        let size = resolve defaults ["--no-size-optimized"]
        Assert.That(size.AheadOfTime, Is.True)
        Assert.That(size.MinimizeSize, Is.False)

    [<Test>]
    member _.``Changing backend or publishing mode drops incompatible inherited defaults``() =
        let source = resolve defaults ["--target"; "csharp"]
        Assert.That(source.Backend, Is.EqualTo("csharp"))
        Assert.That(source.AheadOfTime || source.SingleFile || source.MinimizeSize, Is.False)
        let single = resolve defaults ["--single-file"]
        Assert.That(single.SingleFile, Is.True)
        Assert.That(single.AheadOfTime || single.MinimizeSize, Is.False)
        let aot = resolve { defaults with AheadOfTime=Some false; SingleFile=Some true } ["--aot"]
        Assert.That(aot.AheadOfTime, Is.True)
        Assert.That(aot.SingleFile, Is.False)
        let assembly = resolve { CompileConfiguration.empty with SingleFile=Some true } ["--no-single-file"]
        Assert.That(assembly.SingleFile, Is.False)

    [<Test>]
    member _.``Explicit output overrides normalized manifest output``() =
        let configured = Path.Combine(Path.GetTempPath(), "configured")
        let result = resolve { defaults with OutputDirectory=Some configured } ["--output"; "cli-output"]
        Assert.That(result.OutputDirectory, Is.EqualTo(Path.GetFullPath("cli-output")))
        Assert.That((resolve { defaults with OutputDirectory=Some configured } []).OutputDirectory, Is.EqualTo(configured))

    [<TestCase("--aot", "--no-aot")>]
    [<TestCase("--size-optimized", "--no-size-optimized")>]
    [<TestCase("--single-file", "--no-single-file")>]
    member _.``Contradictory explicit boolean flags fail``(positive: string, negative: string) =
        match CompileOptions.resolve [|"compile"; script; positive; negative|] script defaults with
        | Ok _ -> Assert.Fail("Contradictory flags must fail.")
        | Error message -> Assert.That(message, Does.Contain("cannot be used together"))

    [<TestCase("--target")>]
    [<TestCase("--output")>]
    [<TestCase("--runtime")>]
    member _.``Missing explicit values do not silently use manifest defaults`` flag =
        match CompileOptions.resolve [|"compile"; script; flag|] script defaults with
        | Ok _ -> Assert.Fail("Missing CLI value must fail.")
        | Error message -> Assert.That(message, Does.Contain(flag + " requires a value"))
