namespace FScript.Backend.DotNet.Tests

open System
open System.IO
open NUnit.Framework
open FScript.DotNet.Comparison

[<TestFixture>]
type ComparisonTests() =
    let mutable prepared: Comparison.Prepared option = None
    let program () = prepared |> Option.defaultWith (fun () -> invalidOp "Comparison not initialized.")

    [<OneTimeSetUp>]
    member _.Prepare() =
        let source = File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"workloads.fss"))
        prepared <- Some(Comparison.prepare source)

    [<Test>]
    member _.``Substantial compiled workloads match both FSharp references across runtime inputs`` () =
        Comparison.verify (program ())
        Assert.That((program ()).Workloads.Length, Is.EqualTo(5))

    [<Test>]
    member _.``Compiled workloads prune unused data and do not host an interpreter`` () =
        let assembly = (program ()).Assembly
        Assert.That(assembly.GetTypes() |> Array.exists (fun typ -> typ.Name.Contains("UnusedPayload")), Is.False)
        Assert.That(assembly.GetReferencedAssemblies() |> Array.exists (fun name -> (nonNull name.Name).StartsWith("FScript.",StringComparison.Ordinal)), Is.False)

    [<Test>]
    member _.``IL reports resolve tokens and valid branch destinations for both compilers`` () =
        let assemblies = [(program ()).Assembly; typeof<FScript.DotNet.Reference.Model.Event>.Assembly]
        for assembly in assemblies do
            for method in IlInspection.methods assembly do
                let instructions = IlInspection.decode method
                let offsets = instructions |> Array.map _.Offset |> Set.ofArray
                Assert.That((IlInspection.summarize method).UnresolvedTokens, Is.Zero, IlInspection.methodName method)
                for instruction in instructions do
                    for target in instruction.BranchTargets do
                        Assert.That(offsets.Contains target, Is.True, IlInspection.methodName method)

    [<Test>]
    member _.``Tail recursive numeric kernels contain loops rather than direct call cycles`` () =
        let numeric = (program ()).Workloads |> List.find (fun workload -> workload.Name="numeric")
        for variant in numeric.Variants do
            let assembly = variant.Root.Module.Assembly
            let graph = IlInspection.reachable assembly variant.Root
            Assert.That(graph |> Array.exists (fun method -> (IlInspection.summarize method).BackwardBranches>0), Is.True, variant.Name)
            Assert.That(IlInspection.hasCallCycle assembly variant.Root, Is.False, variant.Name)

    [<Test>]
    member _.``Known captured partial functions and tail-loop scaffolding allocate nothing per invocation`` () =
        let numeric = (program ()).Workloads |> List.find (fun workload -> workload.Name="numeric")
        let variant = numeric.Variants |> List.find (fun variant -> variant.Name="fscript")
        for seed in 1L..50L do variant.Function.Invoke seed |> ignore
        let before = GC.GetAllocatedBytesForCurrentThread()
        let mutable result = 0L
        for seed in 1L..20L do result <- variant.Function.Invoke seed
        let allocated = GC.GetAllocatedBytesForCurrentThread()-before
        Assert.That(result,Is.EqualTo(numeric.Variants.[1].Function.Invoke 20L))
        Assert.That(allocated,Is.Zero)
        let graph = IlInspection.reachable (program ()).Assembly variant.Root
        Assert.That(graph |> Array.collect IlInspection.decode |> Array.exists (fun instruction -> instruction.Opcode = System.Reflection.Emit.OpCodes.Newobj),Is.False)
        Assert.That(graph |> Array.sumBy (fun method -> (IlInspection.summarize method).DelegateCalls),Is.Zero)

    [<Test>]
    member _.``Private nominal pipelines avoid whole-array record adaptations`` () =
        let variants = (program ()).Workloads |> List.filter (fun workload -> workload.Name="pipeline" || workload.Name="pipeline_nominal") |> List.map (fun workload -> workload.Variants.Head.Function)
        let allocated (fn: Func<int64,int64>) =
            for _ in 1..10 do fn.Invoke 42L |> ignore
            let before=GC.GetAllocatedBytesForCurrentThread()
            for _ in 1..3 do fn.Invoke 42L |> ignore
            GC.GetAllocatedBytesForCurrentThread()-before
        Assert.That(allocated variants.[1],Is.EqualTo(allocated variants.[0]))

    [<Test>]
    member _.``Pure record pipelines fuse without allocating intermediate arrays or records`` () =
        for name in ["pipeline";"pipeline_nominal"] do
            let variant=(program ()).Workloads |> List.find (fun workload -> workload.Name=name) |> fun workload -> workload.Variants.Head
            for _ in 1..10 do variant.Function.Invoke 42L |> ignore
            let before=GC.GetAllocatedBytesForCurrentThread()
            for _ in 1..3 do variant.Function.Invoke 42L |> ignore
            Assert.That(GC.GetAllocatedBytesForCurrentThread()-before,Is.Zero,name)

    [<Test>]
    member _.``Independent IL verification accepts native collection and tuple representations`` () =
        let verifier=Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__,"../../.out/ilverify/ilverify"))
        if not (File.Exists verifier) then Assert.Ignore("Install dotnet-ilverify in .out/ilverify for independent verification.")
        let directory=Path.Combine(Path.GetTempPath(),"fscript-collections-ilverify-"+Guid.NewGuid().ToString("N"))
        try
            Directory.CreateDirectory directory |> ignore
            let path=Path.Combine(directory,"FScript.Benchmark.Workloads.dll")
            File.WriteAllBytes(path,(program ()).Binary)
            let start=Diagnostics.ProcessStartInfo(verifier,RedirectStandardOutput=true,RedirectStandardError=true)
            for argument in [path;"--system-module";"System.Private.CoreLib"] do start.ArgumentList.Add argument
            let platform=(AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") |> nonNull) :?> string
            for reference in platform.Split(Path.PathSeparator) |> Array.distinctBy Path.GetFileName do
                start.ArgumentList.Add "-r";start.ArgumentList.Add reference
            use proc=Diagnostics.Process.Start(start) |> nonNull
            let output=proc.StandardOutput.ReadToEndAsync()
            let error=proc.StandardError.ReadToEndAsync()
            if not (proc.WaitForExit(60000)) then proc.Kill(true);Assert.Fail("Independent IL verification timed out.")
            Assert.That(proc.ExitCode,Is.Zero,output.Result+error.Result)
        finally
            if Directory.Exists directory then Directory.Delete(directory,true)
