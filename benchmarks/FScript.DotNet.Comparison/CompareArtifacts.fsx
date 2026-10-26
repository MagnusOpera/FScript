// Build the comparison project in Release before running this script.
#r "bin/Release/net10.0/FScript.DotNet.Reference.dll"
#r "bin/Release/net10.0/FScript.DotNet.Comparison.dll"

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Text.Json
open FScript.DotNet.Comparison

let args = fsi.CommandLineArgs |> Array.skip 1
if args.Length<>3 then invalidArg "args" "Usage: CompareArtifacts.fsx <before.dll> <after.dll> <output-directory>"
let beforeBytes,afterBytes = File.ReadAllBytes(args.[0]),File.ReadAllBytes(args.[1])
let before,after = Assembly.Load(beforeBytes),Assembly.Load(afterBytes)
let exported (assembly: Assembly) label name : Comparison.Variant =
    let typ = assembly.GetType("FScript.Benchmark.Generated.Workloads") |> Option.ofObj |> Option.defaultWith (fun () -> invalidArg "assembly" "Expected the comparison workload assembly.")
    let property = typ.GetProperty("v_"+name) |> Option.ofObj |> Option.defaultWith (fun () -> invalidArg "assembly" ("Missing workload "+name))
    let fn = property.GetValue(null) :?> Func<int64,int64>
    {Name=label;Function=fn;Root=fn.Method}
let reference moduleName name : Comparison.Variant =
    let typ = typeof<FScript.DotNet.Reference.Model.Event>.Assembly.GetType("FScript.DotNet.Reference."+moduleName)
    let method = typ.GetMethod(name,BindingFlags.Static ||| BindingFlags.Public)
    {Name="fsharp-"+moduleName.ToLowerInvariant();Function=method.CreateDelegate<Func<int64,int64>>();Root=method}
let workloads : Comparison.Workload list =
    Comparison.workloadNames |> List.map (fun name ->
        let referenceName=if name="pipeline_nominal" then "pipeline" else name
        {Name=name;Variants=[exported before "before" name;exported after "after" name;reference "Lists" referenceName] @ (if name="numeric" then [] else [reference "Arrays" referenceName])})
let source = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__,"workloads.fss"))
let prepared : Comparison.Prepared =
    {Assembly=after;Binary=afterBytes;CompilationMilliseconds=0.;Workloads=workloads;Source=source}
Comparison.verify prepared
let samples = Comparison.run 500 9 prepared
let median values = let sorted=List.sort values in sorted.[sorted.Length/2]
let summaries =
    samples |> List.groupBy (fun item -> item.Workload,item.Variant)
    |> List.map (fun ((workload,variant),items) ->
        {|workload=workload;variant=variant;milliseconds=items |> List.map _.Milliseconds |> median
          allocatedBytes=items |> List.map _.AllocatedBytes |> median
          minimum=items |> List.map _.Milliseconds |> List.min;maximum=items |> List.map _.Milliseconds |> List.max|})
let output = Path.GetFullPath args.[2]
Directory.CreateDirectory output |> ignore
let report =
    {|runtime=Environment.Version.ToString();os=System.Runtime.InteropServices.RuntimeInformation.OSDescription
      architecture=System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture.ToString();serverGC=System.Runtime.GCSettings.IsServerGC
      tieredCompilation=Environment.GetEnvironmentVariable "DOTNET_TieredCompilation";readyToRun=Environment.GetEnvironmentVariable "DOTNET_ReadyToRun"
      beforeSha256=Convert.ToHexString(SHA256.HashData beforeBytes);afterSha256=Convert.ToHexString(SHA256.HashData afterBytes)
      sourceSha256=Convert.ToHexString(SHA256.HashData(Text.Encoding.UTF8.GetBytes source))
      iterations=500;rounds=9;validationSeeds=Comparison.validationSeeds.Length;measurements=samples;summaries=summaries|}
File.WriteAllText(Path.Combine(output,"results.json"),JsonSerializer.Serialize(report,JsonSerializerOptions(WriteIndented=true)))
for row in summaries do printfn "%-17s %-14s %9.5f ms %12.0f B/call" row.workload row.variant row.milliseconds row.allocatedBytes
