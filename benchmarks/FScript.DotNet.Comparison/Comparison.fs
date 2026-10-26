namespace FScript.DotNet.Comparison

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Diagnostics
open FScript.Language
open FScript.Compiler

module Comparison =
    type Variant = { Name: string; Function: Func<int64,int64>; Root: MethodInfo }
    type Workload = { Name: string; Variants: Variant list }
    type Prepared =
        { Assembly: Assembly; Binary: byte array; CompilationMilliseconds: float
          Workloads: Workload list; Source: string }
    type Sample =
        { Workload: string; Variant: string; Round: int; Milliseconds: float
          AllocatedBytes: float; Checksum: int64 }

    let workloadNames = ["numeric"; "pipeline"; "pipeline_nominal"; "routing"; "sorting"]
    let validationSeeds = [-97L; -1L; 0L; 1L; 7L; 42L; 101L; 999983L; 2147483647L] @ [for i in 1L..23L -> i * 31337L]
    let private unwrap = function Ok value -> value | Error errors -> failwith (sprintf "%A" errors)

    let prepare source =
        let watch = Stopwatch.StartNew()
        let compiled = Compiler.compile { Program=Parser.parseProgram source; Externs=[] } |> unwrap
        let options: DotNetOptions =
            { AssemblyName="FScript.Benchmark.Workloads"; Namespace="FScript.Benchmark.Generated"
              ClassName="Workloads"; OutputKind=Library; References=[] }
        let files = DotNetBackend.generate options compiled.Lir |> unwrap
        watch.Stop()
        let binary = files |> List.find (fun file -> file.RelativePath=options.AssemblyName+".dll") |> _.Content
        let assembly = Assembly.Load binary
        if assembly.GetReferencedAssemblies() |> Array.exists (fun item -> (nonNull item.Name).StartsWith("FScript.",StringComparison.Ordinal)) then
            invalidOp "Generated workload depends on the FScript interpreter."
        let typ = assembly.GetType(options.Namespace+"."+options.ClassName)
        if isNull typ then invalidOp ("Missing generated class; types: " + String.concat ", " (assembly.GetTypes() |> Array.map _.Name))
        let typ = nonNull typ
        let reference = typeof<FScript.DotNet.Reference.Model.Event>.Assembly
        let staticFunction moduleName name =
            let method = (reference.GetType("FScript.DotNet.Reference."+moduleName) |> nonNull).GetMethod(name, BindingFlags.Public ||| BindingFlags.Static) |> nonNull
            { Name="fsharp-"+moduleName.ToLowerInvariant(); Function=method.CreateDelegate<Func<int64,int64>>(); Root=method }
        let workloads = workloadNames |> List.map (fun name ->
            let property = typ.GetProperty("v_"+name,BindingFlags.Public ||| BindingFlags.Static)
            if isNull property then invalidOp ("Missing export " + name + "; properties: " + String.concat ", " (typ.GetProperties() |> Array.map _.Name))
            let property = nonNull property
            let fn = property.GetValue(null) |> nonNull :?> Func<int64,int64>
            let script = { Name="fscript"; Function=fn; Root=fn.Method }
            let referenceName = if name="pipeline_nominal" then "pipeline" else name
            let references = staticFunction "Lists" referenceName :: (if name="numeric" then [] else [staticFunction "Arrays" referenceName])
            { Name=name; Variants=script :: references })
        { Assembly=assembly; Binary=binary; CompilationMilliseconds=watch.Elapsed.TotalMilliseconds; Workloads=workloads; Source=source }

    let verify prepared =
        for workload in prepared.Workloads do
            for seed in validationSeeds do
                let expected = workload.Variants.[1].Function.Invoke seed
                for variant in workload.Variants do
                    let actual = variant.Function.Invoke seed
                    if actual<>expected then invalidOp $"{workload.Name}/{variant.Name}, seed {seed}: expected {expected}, got {actual}."
        // Ask the CLR to JIT every closed managed method, not just the executed paths.
        for method in IlInspection.methods prepared.Assembly do
            if not method.ContainsGenericParameters && not (method.DeclaringType |> nonNull).ContainsGenericParameters then
                RuntimeHelpers.PrepareMethod(method.MethodHandle)

    let measure iterations round workload variant =
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()
        let allocated = GC.GetAllocatedBytesForCurrentThread()
        let start = Stopwatch.GetTimestamp()
        let mutable checksum = 0L
        for index in 0..iterations-1 do
            // Vary the input; never benchmark a cached Run()/initialization result.
            let value = variant.Function.Invoke(42L + int64 (index % 17))
            checksum <- checksum ^^^ value
        let elapsed = Stopwatch.GetElapsedTime(start).TotalMilliseconds
        let bytes = GC.GetAllocatedBytesForCurrentThread() - allocated
        { Workload=workload; Variant=variant.Name; Round=round
          Milliseconds=elapsed/float iterations; AllocatedBytes=float bytes/float iterations; Checksum=checksum }

    let run iterations rounds prepared =
        if iterations<=0 || rounds<=0 then invalidArg "iterations" "Iterations and rounds must be positive."
        [ for workload in prepared.Workloads do
            for variant in workload.Variants do
                let warmup = Stopwatch.StartNew()
                let mutable count = 0
                let mutable checksum = 0L
                while count < 20 || warmup.ElapsedMilliseconds < 500L do
                    checksum <- checksum ^^^ variant.Function.Invoke(42L + int64 (count % 17))
                    count <- count + 1
                GC.KeepAlive checksum
            for round in 0..rounds-1 do
                // Rotate, then reverse on alternate rounds, balancing time/order bias.
                let values = workload.Variants |> List.toArray
                let order = [for i in 0..values.Length-1 -> values.[(i+round)%values.Length]]
                let order = if round%2=0 then order else List.rev order
                for variant in order do yield measure iterations round workload.Name variant ]

    let inspect prepared directory =
        Directory.CreateDirectory directory |> ignore
        File.WriteAllBytes(Path.Combine(directory,"FScript.Benchmark.Workloads.dll"),prepared.Binary)
        File.WriteAllText(Path.Combine(directory,"workloads.fss"),prepared.Source)
        let reference = typeof<FScript.DotNet.Reference.Model.Event>.Assembly
        IlInspection.write (Path.Combine(directory,"fscript.il.txt")) prepared.Assembly
        IlInspection.write (Path.Combine(directory,"fsharp.il.txt")) reference
        let summaries assembly = IlInspection.methods assembly |> Array.map IlInspection.summarize
        let graph variant =
            let assembly = variant.Root.Module.Assembly
            IlInspection.reachable assembly variant.Root
            |> Array.map IlInspection.summarize
        {| fscript=summaries prepared.Assembly
           fsharp=summaries reference
           fscriptTypes=prepared.Assembly.GetTypes() |> Array.map _.FullName
           fsharpTypes=reference.GetTypes() |> Array.map _.FullName
           fscriptReferences=prepared.Assembly.GetReferencedAssemblies() |> Array.map _.Name
           fsharpReferences=reference.GetReferencedAssemblies() |> Array.map _.Name
           fscriptAssemblyBytes=prepared.Binary.Length
           fsharpAssemblyBytes=FileInfo(reference.Location).Length
           roots=[|for workload in prepared.Workloads do
                       for variant in workload.Variants do
                           let assembly = variant.Root.Module.Assembly
                           let reached = IlInspection.reachable assembly variant.Root
                           let externalCalls =
                               reached |> Array.collect IlInspection.decode
                               |> Array.choose (fun instruction -> instruction.Target |> Option.filter (fun target -> target.Module.Assembly<>assembly) |> Option.map IlInspection.methodName)
                               |> Array.countBy id |> Array.sortByDescending snd
                               |> Array.map (fun (name,count) -> {|name=name;count=count|})
                           yield {|workload=workload.Name;variant=variant.Name;root=IlInspection.methodName variant.Root;methods=graph variant
                                   hasDirectCallCycle=IlInspection.hasCallCycle assembly variant.Root;externalCalls=externalCalls|}|] |}
