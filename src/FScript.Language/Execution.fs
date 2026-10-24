namespace FScript.Language

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

/// Continuations and exception scopes are data, rather than suspended CLR stacks.
type internal Instruction =
    | Finished
    | Execute of (unit -> Instruction)
    | PushHandler of (exn -> Instruction) * (unit -> Instruction)
    | PopHandler of (unit -> Instruction)
#if !FABLE_COMPILER
    | Waiting of Task * (unit -> Instruction)
#endif

// Two fields keep completed reference values small enough to return in registers.
// A null continuation denotes completion; suspension alone allocates a closure.
[<Struct; NoEquality; NoComparison>]
type internal Execution<'T> =
    { Value: 'T
      Resume: (('T -> Instruction) -> Instruction) }
    member this.IsImmediate = obj.ReferenceEquals(this.Resume, null)

module internal Execution =
    let inline result value = { Value = value; Resume = Unchecked.defaultof<_> }
    let inline suspend resume = { Value = Unchecked.defaultof<_>; Resume = resume }
    let inline invoke (execution: Execution<'T>) ([<InlineIfLambda>] continuation) =
        if execution.IsImmediate then continuation execution.Value else execution.Resume continuation
    let inline bind ([<InlineIfLambda>] binder) (body: Execution<'T>) =
        if body.IsImmediate then binder body.Value
        else suspend (fun continuation -> invoke body (fun value -> invoke (binder value) continuation))
    let protect handler (body: Execution<'T>) =
        if body.IsImmediate then body
        else suspend (fun continuation ->
            PushHandler((fun error -> invoke (handler error) continuation),fun () ->
                invoke body (fun value -> PopHandler(fun () -> continuation value))))
    let finish cleanup (body: Execution<'T>) =
        if body.IsImmediate then cleanup (); body
        else suspend (fun continuation ->
            PushHandler((fun error -> cleanup (); raise error),fun () ->
                invoke body (fun value -> PopHandler(fun () -> cleanup (); continuation value))))
    type Builder() =
        member inline _.Return value = result value
        member inline _.ReturnFrom body = body
        member inline _.Bind(body,[<InlineIfLambda>] binder) = bind binder body
        member inline _.Zero() = result ()
        // Eager completion is the ordinary interpreter path. Only a real suspension
        // builds continuations; both paths execute these same expression reducers.
        member inline _.Delay ([<InlineIfLambda>] thunk: unit -> Execution<'T>) = thunk
        member inline _.Run ([<InlineIfLambda>] thunk) = thunk ()
        member inline _.Combine(first,[<InlineIfLambda>] second) = bind (fun () -> second ()) first
        member _.TryWith(body,handler) =
            let first = try Choice1Of2(body ()) with error -> Choice2Of2 error
            match first with Choice1Of2 value -> protect handler value | Choice2Of2 error -> handler error
        member _.TryFinally(body,cleanup) =
            let first = try body () with error -> cleanup (); reraise ()
            finish cleanup first
        member this.While(condition,body: unit -> Execution<unit>) =
            let mutable pending = None
            while pending.IsNone && condition () do
                let next = body ()
                if not next.IsImmediate then pending <- Some(bind (fun () -> this.While(condition,body)) next)
            defaultArg pending (result ())
        member _.For(items: 'T list,body: 'T -> Execution<unit>) =
            let rec loop remaining =
                match remaining with
                | [] -> result ()
                | head :: tail ->
                    let next = body head
                    if next.IsImmediate then loop tail
                    else bind (fun () -> loop tail) next
            loop items
        member this.For(items: seq<'T>,body) =
            let iterator = items.GetEnumerator()
            this.TryFinally((fun () -> this.While(iterator.MoveNext,fun () -> body iterator.Current)),iterator.Dispose)
    let execution = Builder()
    // Traverse lists directly. Completed elements need neither an enumerator nor
    // a continuation; a suspended element resumes the same traversal at its tail.
    let map (mapper: 'T -> Execution<'U>) items =
        match items with
        | [] -> result []
        // Records and tuples commonly contain one or two fields. Avoid allocating
        // a growable buffer for these small, fixed-size results.
        | [item] -> bind (fun value -> result [value]) (mapper item)
        | [first; second] -> execution {
            let! a = mapper first
            let! b = mapper second
            return [a; b]
          }
        | _ ->
            let output = ResizeArray()
            let rec loop remaining =
                match remaining with
                | [] -> result (List.ofSeq output)
                | head :: tail ->
                    let next = mapper head
                    if next.IsImmediate then output.Add next.Value; loop tail
                    else bind (fun value -> output.Add value; loop tail) next
            loop items
    let rec fold (folder: 'State -> 'T -> Execution<'State>) state items =
        match items with
        | [] -> result state
        | head :: tail ->
            let next = folder state head
            if next.IsImmediate then fold folder next.Value tail
            else bind (fun value -> fold folder value tail) next
    let run body =
        let mutable result = None
        let mutable current = invoke body (fun value -> result <- Some value; Finished)
        let mutable handlers = []
        while result.IsNone do
            try
                match current with
                | Finished -> invalidOp "Execution completed without a result."
                | Execute next -> current <- next ()
                | PushHandler(handler,next) -> handlers <- handler :: handlers; current <- next ()
                | PopHandler next -> handlers <- List.tail handlers; current <- next ()
#if !FABLE_COMPILER
                | Waiting(wait,next) ->
                    try wait.GetAwaiter().GetResult() with _ -> ()
                    current <- next ()
#endif
            with error ->
                match handlers with
                | handler :: rest -> handlers <- rest; current <- Execute(fun () -> handler error)
                | [] -> reraise ()
        result.Value
#if !FABLE_COMPILER
    let awaitTask (task: Task<'T>) =
        if task.IsCompletedSuccessfully then result (task.GetAwaiter().GetResult())
        else suspend (fun continuation -> Waiting(task,fun () -> continuation (task.GetAwaiter().GetResult())))
    let awaitUnit (task: Task) =
        if task.IsCompletedSuccessfully then result ()
        else suspend (fun continuation -> Waiting(task,fun () -> task.GetAwaiter().GetResult(); continuation ()))
    let runTask (scheduler: TaskScheduler) factory =
        let completion = TaskCompletionSource<_>(TaskCreationOptions.RunContinuationsAsynchronously)
        let mutable current = Execute(fun () -> invoke (factory ()) (fun value -> completion.TrySetResult(value) |> ignore; Finished))
        let mutable handlers = []
        let rec schedule () =
            Task.Factory.StartNew(System.Action drive, System.Threading.CancellationToken.None, TaskCreationOptions.DenyChildAttach, scheduler) |> ignore
        and drive () =
            let mutable runnable = true
            let mutable budget = 256
            while runnable && not completion.Task.IsCompleted && budget > 0 do
                budget <- budget - 1
                try
                    match current with
                    | Finished -> runnable <- false
                    | Execute next -> current <- next ()
                    | PushHandler(handler,next) -> handlers <- handler :: handlers; current <- next ()
                    | PopHandler next -> handlers <- List.tail handlers; current <- next ()
                    | Waiting(wait,next) ->
                        current <- Execute next
                        if not wait.IsCompleted then
                            runnable <- false
                            wait.ContinueWith(System.Action<Task>(fun _ -> schedule ()),System.Threading.CancellationToken.None,TaskContinuationOptions.ExecuteSynchronously,TaskScheduler.Default) |> ignore
                with error ->
                    match handlers with
                    | handler :: rest -> handlers <- rest; current <- Execute(fun () -> handler error)
                    | [] -> completion.TrySetException(error) |> ignore
            if runnable && not completion.Task.IsCompleted then schedule ()
        schedule ()
        completion.Task
#endif
