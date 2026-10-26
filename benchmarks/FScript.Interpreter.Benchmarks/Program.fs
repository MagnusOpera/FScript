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
        Array.init 5 (fun _ ->
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
    let size = if args.Length > 1 then Int32.Parse args[1] else 2000
    if iterations <= 0 || size <= 0 then invalidArg "args" "Iterations and size must be positive."
    let inputs = $"[1..{size}]"
    let workloads =
        [ "map", $"List.map (fun x -> x + 1) {inputs}"
          "iter", $"List.iter (fun x -> ignore (x + 1)) {inputs}"
          "choose", $"List.choose (fun x -> if x %% 4 = 0 then Some x else None) {inputs}"
          "collect", $"List.collect (fun x -> [x; x + 1]) {inputs}"
          "fold", $"List.fold (fun acc x -> acc + x) 0 {inputs}"
          "loop", $"for x in {inputs} do\n    let y = x + 1\n    ()"
          "curried", $"let add x y = x + y\nList.map (fun x -> add x 1) {inputs}"
          "records", $"type Row = {{ Value: int; Original: int }}\nList.map (fun x -> {{ Value = x + 1; Original = x }}) {inputs}"
          "record-equality", $"type Row = {{ Value: int; Original: int }}\nList.map (fun x -> {{ Value = x; Original = x }} = {{ Value = x; Original = x }}) {inputs}"
          "map-equality", $"List.map (fun x -> {{ [\"value\"] = x }} = {{ [\"value\"] = x }}) {inputs}"
          "recursion", "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)\nfib 20" ]
    printfn "Interpreter benchmark: size=%d iterations=%d" size iterations
    for name, source in workloads do
        let program = Parser.parseProgram source
        let typed = TypeInfer.inferProgram program
        let compiled =
            match Compiler.compile { Program = program; Externs = [] } with
            | Ok compiled -> compiled.Lir
            | Error errors -> failwithf "Compilation failed: %A" errors
        measure iterations (name + "/ast") (fun () -> Eval.evalProgram typed |> GC.KeepAlive)
        measure iterations (name + "/lir") (fun () ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled with
            | Ok result -> GC.KeepAlive result
            | Error error -> failwithf "Execution failed: %A" error)
    0
