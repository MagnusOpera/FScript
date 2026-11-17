namespace FScript.Runtime.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open FsCheck
open FsCheck.FSharp
open FScript.Extensibility
open FScript.Runtime

module internal MailboxProperties =
    type Command = Post of int | Finish
    type Sample = { Capacity: int; Initial: int; Prefix: int list; Operations: Command list }
    type Scenario = Sequential | HandlerFailure | Backpressure | BackpressureFailure
    type Snapshot = { Total: int64; Seen: int list }
    type Observation = Posted | Finished of Snapshot | RejectedClosed | HandlerFailed
    type Model = { State: Snapshot; Closed: bool; Failed: bool }

    let private failureMessage = "property mailbox handler failed"
    let private closedMessage = "Cannot post to a finished mailbox."
    let private failureMessageValue = 99
    let private timeout = TimeSpan.FromSeconds 5.0

    let generator = Gen.sized (fun size -> gen {
        let! capacity = Gen.choose (1, 4)
        let! initial = Gen.choose (-9, 9)
        let! prefixLength = Gen.choose (0, min 8 (size / 4))
        let! prefix = Gen.listOfLength prefixLength (Gen.choose (-9, 9))
        let! count = Gen.choose (0, min 16 size)
        let operation = Gen.frequency [8, Gen.choose (-9, 9) |> Gen.map Post; 2, Gen.constant Finish]
        let! operations = Gen.listOfLength count operation
        return { Capacity = capacity; Initial = initial; Prefix = prefix; Operations = operations }
    })

    let private shrinkInteger value = seq {
        if value <> 0 then yield 0
        let half = value / 2
        if half <> 0 && half <> value then yield half
    }

    let private shrinkList shrinkItem items =
        seq {
            if not (List.isEmpty items) then
                yield []
                for index in 0 .. List.length items - 1 do
                    yield List.removeAt index items
                    for smaller in shrinkItem items[index] do
                        yield List.updateAt index smaller items
        } |> Seq.distinct

    let shrink sample = seq {
        if sample.Capacity > 1 then yield { sample with Capacity = 1 }
        for initial in shrinkInteger sample.Initial do yield { sample with Initial = initial }
        for prefix in shrinkList shrinkInteger sample.Prefix do yield { sample with Prefix = prefix }
        let shrinkCommand = function Post value -> shrinkInteger value |> Seq.map Post | Finish -> Seq.empty
        for operations in shrinkList shrinkCommand sample.Operations do yield { sample with Operations = operations }
    }

    let arbitrary = Arb.fromGenShrink(generator, shrink)

    let queuedMessages sample =
        // The blocked-worker scenario always fills the queue, even after the
        // optional prefix shrinks to nothing. Message zero is still observable
        // in the history, so a lost zero cannot hide behind the checksum.
        List.init sample.Capacity (fun index -> sample.Prefix |> List.tryItem index |> Option.defaultValue 0)

    let commands scenario sample =
        match scenario with
        | Sequential -> List.map Post sample.Prefix @ sample.Operations @ [Finish]
        | HandlerFailure -> List.map Post sample.Prefix @ [Post failureMessageValue] @ sample.Operations @ [Finish; Finish; Post 0]
        | Backpressure -> List.map Post (queuedMessages sample) @ [Post sample.Initial; Finish; Post sample.Initial] @ sample.Operations @ [Finish]
        | BackpressureFailure -> [Post failureMessageValue] @ List.map Post (queuedMessages sample) @ [Post sample.Initial; Finish; Post sample.Initial] @ sample.Operations @ [Finish]

    let initialModel sample = { State = { Total = int64 sample.Initial; Seen = [] }; Closed = false; Failed = false }

    let transition model command =
        match command with
        | Post _ when model.Closed -> model, RejectedClosed
        | Post _ when model.Failed -> model, HandlerFailed
        | Post value when value = failureMessageValue -> { model with Failed = true }, Posted
        | Post value ->
            let messages = model.State.Seen @ [value]
            let state = { Total = model.State.Total * 2L + int64 value; Seen = messages }
            { model with State = state }, Posted
        | Finish ->
            { model with Closed = true }, (if model.Failed then HandlerFailed else Finished model.State)

    let private observationsFrom model operations =
        operations
        |> List.mapFold (fun model command -> let next, observation = transition model command in observation, next) model
        |> fst

    let modelObservations sample operations = observationsFrom (initialModel sample) operations

    let expected scenario sample =
        match scenario with
        | BackpressureFailure ->
            // Hold the failing handler after it dequeues its message, then fill
            // the queue. Releasing that handler cannot free another queue slot
            // before failure, so the pending post deterministically fails too.
            let failed = { initialModel sample with Closed = true; Failed = true }
            List.replicate (sample.Capacity + 1) Posted @ [HandlerFailed; HandlerFailed; RejectedClosed]
            @ observationsFrom failed (sample.Operations @ [Finish])
        | _ -> modelObservations sample (commands scenario sample)

    let expectedAttempts scenario sample =
        match scenario with
        | BackpressureFailure -> [failureMessageValue]
        | _ ->
            List.zip (commands scenario sample) (expected scenario sample)
            |> List.choose (function Post value, Posted -> Some value | _ -> None)

    /// Real provider calls and workers. Only the handler and worker scheduler
    /// belong to the test; the mailbox's queue/admission/shutdown code is real.
    type private Session(sample: Sample, scenario: Scenario) =
        let functions = (MailboxProvider.register ()).Functions |> List.map (fun fn -> fn.Name, fn.Impl) |> Map.ofList
        let workers = ConcurrentBag<Task>()
        let failures = ConcurrentQueue<exn>()
        let attempts = ConcurrentQueue<int>()
        let operations = ResizeArray<Task>()
        let deadline = new CancellationTokenSource(timeout)
        let release = new ManualResetEventSlim(scenario <> Backpressure)
        let releaseFailure = new ManualResetEventSlim(scenario <> BackpressureFailure)
        let handlerEntered = new ManualResetEventSlim(false)

        let rec context waiting =
            ExternalCallContext(
                (fun fn argument ->
                    match fn, argument with
                    | VUnit, VTuple [VInt _; VList _] -> argument
                    | VTuple [VInt total; VList seen], VInt message ->
                        attempts.Enqueue(int message)
                        if message = int64 failureMessageValue then
                            handlerEntered.Set()
                            releaseFailure.Wait(deadline.Token)
                            raise (ExtensionException failureMessage)
                        // Deliberately separate from the model transition.
                        VTuple [VInt(total + total + message); VList(List.append seen [VInt message])]
                    | _ -> failwithf "Unexpected mailbox handler application: %A %A" fn argument),
                (fun _ -> failwith "Unexpected script task"),
                (fun _ -> failwith "Unexpected task await"),
                (fun () -> deadline.Token.ThrowIfCancellationRequested(); waiting ()),
                (fun run ->
                    let task = Task.Run(Action(fun () ->
                        try
                            release.Wait(deadline.Token)
                            run (context ignore)
                        with error -> failures.Enqueue error))
                    workers.Add task
                    VUnit))

        let invoke name waiting arguments = functions["Mailbox." + name] (context waiting) arguments
        let inbox = invoke "start" ignore [VInt(int64 sample.Capacity); VUnit; VTuple [VInt(int64 sample.Initial); VList []]]

        let observe waiting command =
            try
                match command with
                | Post value ->
                    match invoke "post" waiting [VInt(int64 value); inbox] with
                    | VUnit -> Posted
                    | actual -> failwithf "Mailbox.post returned %A" actual
                | Finish ->
                    match invoke "finish" waiting [inbox] with
                    | VTuple [VInt total; VList values] ->
                        let seen = values |> List.map (function VInt value -> int value | value -> failwithf "Invalid history value %A" value)
                        Finished { Total = total; Seen = seen }
                    | actual -> failwithf "Mailbox.finish returned %A" actual
            with
            | :? ExtensionException as error when error.Message = closedMessage -> RejectedClosed
            | :? ExtensionException as error when error.Message = failureMessage -> HandlerFailed

        let join (task: Task) =
            if not (task.Wait timeout) then failwith "Mailbox operation or worker did not complete within five seconds"
            task.GetAwaiter().GetResult()

        let settle tasks =
            let all = Task.WhenAll(Seq.toArray tasks)
            try
                if not (all.Wait timeout) then failwith "Mailbox cleanup could not join its tasks within five seconds"
            with :? AggregateException -> () // All tasks completed; the caller owns their failure.

        member _.Observe command = observe ignore command
        member _.Start(command, waiting) =
            let task = Task.Run(fun () -> observe waiting command)
            operations.Add task
            task
        member _.Release() = release.Set(); releaseFailure.Set()
        member _.WaitForFailingHandler() =
            if not (handlerEntered.Wait timeout) then failwith "The faulting handler did not start"
        member _.Join(task: Task<Observation>) = join task; task.Result
        member _.Quiesce() = workers.ToArray() |> Array.iter join
        member _.Attempts = List.ofSeq attempts
        member _.CheckWorkers allowFailure =
            for error in failures do
                match error with
                | :? ExtensionException as error when allowFailure && error.Message = failureMessage -> ()
                | _ -> raise error

        interface IDisposable with
            member _.Dispose() =
                // Release first on every failure path, then close admission and
                // join owned tasks before disposing events. Cancellation makes
                // provider wait loops abort if a regression prevents progress.
                release.Set()
                releaseFailure.Set()
                try observe ignore Finish |> ignore with _ -> ()
                let mutable joined = false
                try
                    deadline.Cancel()
                    settle operations
                    settle (workers.ToArray())
                    joined <- true
                finally
                    // A task that failed to join may still access its context.
                    if joined then
                        release.Dispose()
                        releaseFailure.Dispose()
                        handlerEntered.Dispose()
                        deadline.Dispose()

    let private compare scenario sample actual =
        let predicted = expected scenario sample
        if predicted <> actual then failwithf "Reference: %A\nActual: %A" predicted actual

    let check scenario sample =
        // These events outlive Session.Dispose, including if an assertion fails
        // while a pending operation can still invoke its wait callback.
        use postWaiting = new ManualResetEventSlim(false)
        use finishWaiting = new ManualResetEventSlim(false)
        use session = new Session(sample, scenario)
        let actual = ResizeArray<Observation>()
        match scenario with
        | Sequential | HandlerFailure ->
            for command in commands scenario sample do
                actual.Add(session.Observe command)
                // A deliberate handler failure is observed before the next
                // command. Whether a worker wins a race cannot change the oracle.
                if scenario = HandlerFailure then session.Quiesce()
        | Backpressure | BackpressureFailure ->
            if scenario = BackpressureFailure then
                actual.Add(session.Observe(Post failureMessageValue))
                session.WaitForFailingHandler()
            for value in queuedMessages sample do actual.Add(session.Observe(Post value))
            let pending = session.Start(Post sample.Initial, postWaiting.Set)
            try
                if not (postWaiting.Wait timeout) then failwith "Full queue did not block the pending post"
                if pending.IsCompleted then failwith "A post completed while the queue was full and the worker was held"
                let finishing = session.Start(Finish, finishWaiting.Set)
                if not (finishWaiting.Wait timeout) then failwith "Finish did not wait for the held worker and pending post"
                if finishing.IsCompleted then failwith "Finish completed before the held worker was released"
                let late = session.Observe(Post sample.Initial)
                session.Release()
                actual.Add(session.Join pending)
                actual.Add(session.Join finishing)
                actual.Add late
            finally session.Release()
            for command in sample.Operations @ [Finish] do actual.Add(session.Observe command)
        session.Quiesce()
        session.CheckWorkers((scenario = HandlerFailure || scenario = BackpressureFailure))
        compare scenario sample (List.ofSeq actual)
        let attempts = expectedAttempts scenario sample
        if attempts <> session.Attempts then failwithf "Reference handler attempts: %A\nActual handler attempts: %A" attempts session.Attempts

    let describe scenario sample =
        // List.ToString truncates after a few elements. Structured formatting
        // retains the complete bounded sequence needed to reproduce a failure.
        sprintf "Mailbox generator version: 1\nScenario: %A\n%A\nCommands: %A\nReference observations: %A\nReference handler attempts: %A"
            scenario sample (commands scenario sample) (expected scenario sample) (expectedAttempts scenario sample)

    let defaultReplay seed = { Rnd = Rnd seed; Size = None }
    let parseReplay (value: string) =
        match value.Split(',') with
        | [| seed; gamma |] -> { Rnd = Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size = None }
        | [| seed; gamma; size |] ->
            let size = Int32.Parse size
            if size < 0 then invalidArg "value" "Replay size must be nonnegative"
            { Rnd = Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size = Some size }
        | _ -> invalidArg "value" "FSCRIPT_MAILBOX_REPLAY must be seed,gamma or seed,gamma,size"

    let run replay count arb property =
        let mutable result = None
        let runner =
            { new IRunner with
                member _.OnStartFixture _ = ()
                member _.OnArguments(_, _, _) = ()
                member _.OnShrink(_, _) = ()
                member _.OnFinished(_, value) = result <- Some value }
        let config = Config.Quick.WithMaxTest(count).WithStartSize(0).WithEndSize(40).WithReplay(Some replay).WithRunner(runner)
        Check.One(config, Prop.forAll arb property)
        result |> Option.defaultWith (fun () -> failwith "FsCheck did not report a result")

    let report name scenario result =
        let details = Runner.onFinishedToStringWithFormatter (fun value -> describe scenario (unbox<Sample> value)) name result
        match result with
        | TestResult.Failed(_, _, _, _, _, seed, size) ->
            details + $"\nReplay the failing case:\nFSCRIPT_MAILBOX_REPLAY='{seed.Seed},{seed.Gamma},{size}' dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter 'FullyQualifiedName~MailboxPropertyTests.{name}'"
        | _ -> details
