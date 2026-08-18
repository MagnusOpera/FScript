open System
open System.Diagnostics
open FScript.Language

type Measurement =
    { ElapsedMilliseconds: float
      AllocatedBytes: int64 }

let parsePositiveInt fallback (value: string option) =
    match value with
    | Some text ->
        match Int32.TryParse text with
        | true, parsed when parsed > 0 -> parsed
        | _ -> fallback
    | None -> fallback

let sourceFor bindingCount =
    [ for index in 1..bindingCount do
          $"let transform{index} value ="
          "    let identity item = item"
          "    let apply mapper item = mapper item"
          "    let pair = (apply identity value, identity value)"
          "    pair" ]
    |> String.concat "\n"

let measure iterations action =
    action ()
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()

    [ for _ in 1..iterations do
          let allocatedBefore = GC.GetAllocatedBytesForCurrentThread()
          let stopwatch = Stopwatch.StartNew()
          action ()
          stopwatch.Stop()
          let allocatedAfter = GC.GetAllocatedBytesForCurrentThread()
          { ElapsedMilliseconds = stopwatch.Elapsed.TotalMilliseconds
            AllocatedBytes = allocatedAfter - allocatedBefore } ]

let summarize name measurements =
    let elapsed = measurements |> List.map _.ElapsedMilliseconds |> List.sort
    let allocated = measurements |> List.averageBy (fun measurement -> float measurement.AllocatedBytes)
    let median = elapsed[elapsed.Length / 2]
    let minimum = elapsed.Head
    printfn $"{name,-24} median {median,9:F3} ms   min {minimum,9:F3} ms   allocated {allocated / 1_048_576.0,9:F3} MiB"

[<EntryPoint>]
let main args =
    let iterations = args |> Array.tryItem 0 |> parsePositiveInt 10
    let bindingCount = args |> Array.tryItem 1 |> parsePositiveInt 200
    let program = sourceFor bindingCount |> Parser.parseProgram

    printfn $"Inference benchmark: {bindingCount} bindings, {iterations} measured iterations"
    measure iterations (fun () -> TypeInfer.inferProgram program |> ignore)
    |> summarize "ordinary inference"
    measure iterations (fun () -> TypeInfer.inferProgramWithLocalVariableTypes program |> ignore)
    |> summarize "with local capture"

    let expression = $"transform{bindingCount} 42" |> Parser.parseProgram
    let legacyProgram = program @ expression
    let initialSubmission = Interactive.create [] |> fun session -> Interactive.submit session program
    let mutable interactiveSession = initialSubmission.Session

    printfn "Interactive submission benchmark: one expression against retained bindings"
    measure iterations (fun () ->
        let typed = TypeInfer.inferProgram legacyProgram
        Eval.evalProgram typed |> ignore)
    |> summarize "replay full history"
    measure iterations (fun () ->
        let submission = Interactive.submit interactiveSession expression
        interactiveSession <- submission.Session)
    |> summarize "incremental submission"
    0
