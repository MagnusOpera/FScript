open System
open System.IO
open System.Text.Json
open System.Security.Cryptography
open FScript.DotNet.Comparison

let median values = let sorted = values |> List.sort in sorted.[sorted.Length/2]

[<EntryPoint>]
let main args =
    try
        let positional = args |> Array.filter (fun value -> value<>"--verify-only")
        let iterations = if positional.Length>0 then Int32.Parse positional.[0] else 500
        let rounds = if positional.Length>1 then Int32.Parse positional.[1] else 9
        let output = if positional.Length>2 then Path.GetFullPath positional.[2] else Path.GetFullPath "out/benchmarks/dotnet-vs-fsharp"
        if positional.Length>3 then invalidArg "args" "Usage: [iterations] [rounds] [output-directory] [--verify-only]"
        if iterations<=0 || rounds<=0 then invalidArg "args" "Iterations and rounds must be positive."
        let verifyOnly = args |> Array.contains "--verify-only"
        let source = File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"workloads.fss"))
        let prepared = Comparison.prepare source
        Comparison.verify prepared
        let il = Comparison.inspect prepared output
        let samples = if verifyOnly then [] else Comparison.run iterations rounds prepared
        let summaries =
            samples |> List.groupBy (fun sample -> sample.Workload,sample.Variant)
            |> List.map (fun ((workload,variant),items) ->
                {|workload=workload;variant=variant;milliseconds=items |> List.map _.Milliseconds |> median
                  allocatedBytes=items |> List.map _.AllocatedBytes |> median
                  minimum=items |> List.map _.Milliseconds |> List.min
                  maximum=items |> List.map _.Milliseconds |> List.max|})
        let report =
            {| runtime=Environment.Version.ToString();os=System.Runtime.InteropServices.RuntimeInformation.OSDescription
               framework=System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription;architecture=System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture.ToString()
               tieredCompilation=Environment.GetEnvironmentVariable "DOTNET_TieredCompilation"
               readyToRun=Environment.GetEnvironmentVariable "DOTNET_ReadyToRun"
               sourceSha256=Convert.ToHexString(SHA256.HashData(Text.Encoding.UTF8.GetBytes source))
               compilationMilliseconds=prepared.CompilationMilliseconds;validationSeeds=Comparison.validationSeeds.Length
               iterations=iterations;rounds=rounds;measurements=samples;summaries=summaries;il=il |}
        File.WriteAllText(Path.Combine(output,"results.json"),JsonSerializer.Serialize(report,JsonSerializerOptions(WriteIndented=true)))
        printfn "Validated %d workloads across %d seeds and JIT-checked all closed generated methods." prepared.Workloads.Length Comparison.validationSeeds.Length
        printfn "FScript compilation: %.1f ms (excluded from execution measurements)." prepared.CompilationMilliseconds
        for row in summaries do printfn "%-10s %-14s %9.4f ms/call %12.0f bytes/call" row.workload row.variant row.milliseconds row.allocatedBytes
        printfn "Assemblies: FScript %d bytes; F# reference %d bytes (both exclude dependency sizes)." il.fscriptAssemblyBytes il.fsharpAssemblyBytes
        printfn "Results and IL: %s" output
        0
    with error -> eprintfn "%O" error; 1
