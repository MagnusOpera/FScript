open System
open System.Diagnostics
open FScript.Language

let benchmark iterations name source =
    // Parse and infer once: measure normal evaluation, with no debugger session.
    let program = source |> Parser.parseProgram |> TypeInfer.inferProgram
    let warmup = Stopwatch.StartNew()
    let mutable warmed = 0
    while warmed < 20 || warmup.ElapsedMilliseconds < 500L do
        Eval.evalProgram program |> GC.KeepAlive
        warmed <- warmed + 1
    let samples =
        Array.init 5 (fun _ ->
            GC.Collect()
            GC.WaitForPendingFinalizers()
            let allocated = GC.GetTotalAllocatedBytes(true)
            let timer = Stopwatch.StartNew()
            for _ in 1..iterations do Eval.evalProgram program |> GC.KeepAlive
            timer.Stop()
            timer.Elapsed.TotalMilliseconds / float iterations,
            float (GC.GetTotalAllocatedBytes(true) - allocated) / float iterations)
    let milliseconds = samples |> Array.map fst |> Array.sort |> fun values -> values[2]
    let bytes = samples |> Array.map snd |> Array.sort |> fun values -> values[2]
    printfn "%s\t%.3f ms/eval\t%.0f bytes/eval" name milliseconds bytes

[<EntryPoint>]
let main arguments =
    let iterations = if arguments.Length > 0 then Int32.Parse arguments[0] else 20
    let size = if arguments.Length > 1 then Int32.Parse arguments[1] else 10000
    if iterations <= 0 || size <= 0 then invalidArg "arguments" "Iterations and size must be positive."
    let workloads =
        [ "pipeline", $"let values = [1..{size}]\nlet increment x = x + 1\nvalues |> List.map increment |> List.fold (fun acc x -> acc + x) 0"
          "map", $"List.map (fun x -> x + 1) [1..{size}]"
          "extern-calls", $"List.map (fun x -> Int.toString x) [1..{size}]"
          "fold", $"List.fold (fun acc x -> acc + x) 0 [1..{size}]"
          "loop", $"for x in [1..{size}] do\n    let y = x + 1\n    ()"
          "curried", $"let add x y = x + y\nList.map (fun x -> add x 1) [1..{size}]"
          "records", $"type Row = {{ Value: int; Original: int }}\nList.map (fun x -> {{ Value = x + 1; Original = x }}) [1..{size}]"
          "recursion", "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)\nfib 20" ]
    for name,source in workloads do benchmark iterations name source
    0
