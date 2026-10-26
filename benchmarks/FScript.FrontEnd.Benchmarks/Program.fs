open System
open System.Diagnostics
open FScript.Language
open FScript.Compiler

let measure iterations name action =
    let warmup = Stopwatch.StartNew()
    let mutable count = 0
    while count < 5 || warmup.ElapsedMilliseconds < 500L do
        action ()
        count <- count + 1
    let samples =
        Array.init 7 (fun _ ->
            GC.Collect()
            GC.WaitForPendingFinalizers()
            let timer = Stopwatch()
            let allocated = GC.GetAllocatedBytesForCurrentThread()
            timer.Start()
            for _ in 1..iterations do action ()
            timer.Stop()
            timer.Elapsed.TotalMilliseconds / float iterations,
            float (GC.GetAllocatedBytesForCurrentThread() - allocated) / float iterations)
    let median projection = samples |> Array.map projection |> Array.sort |> fun values -> values[values.Length / 2]
    printfn "%s\t%.4f ms/op\t%.0f bytes/op" name (median fst) (median snd)

[<EntryPoint>]
let main args =
    let iterations = if args.Length > 0 then Int32.Parse args[0] else 5
    let size = if args.Length > 1 then Int32.Parse args[1] else 200
    if iterations <= 0 || size <= 0 then invalidArg "args" "Iterations and size must be positive."
    let bindings = [ for i in 1..size -> $"let transform{i} (value: int) = value + {i}" ] |> String.concat "\n"
    let tuple = "let values = (" + ([ for i in 1..size -> string i ] |> String.concat ", ") + ")"
    for workload, source in ["bindings", bindings; "tuple", tuple] do
        let program = Parser.parseProgram source
        let resolved = TypeInfer.inferResolvedProgram program
        printfn "%s: size=%d iterations=%d" workload size iterations
        let run phase action = measure iterations (workload + "/" + phase) action
        run "lex" (fun () -> Lexer.tokenize source |> GC.KeepAlive)
        run "parse" (fun () -> Parser.parseProgram source |> GC.KeepAlive)
        run "infer" (fun () -> TypeInfer.inferProgram program |> GC.KeepAlive)
        run "resolve" (fun () -> TypeInfer.inferResolvedProgram program |> GC.KeepAlive)
        run "analyze" (fun () -> TaskSharingAnalysis.analyze HostInterop.empty TaskSharingAnalysis.defaults resolved |> GC.KeepAlive)
        run "compile" (fun () ->
            match Compiler.compile { Program = program; Externs = [] } with
            | Ok compiled -> GC.KeepAlive compiled
            | Error errors -> failwithf "Compilation failed: %A" errors)
    0
