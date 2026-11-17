namespace FScript.Runtime.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open FScript.Language
open FScript.Runtime
open NUnit.Framework
open RuntimePropertySupport

[<TestFixture>]
type TaskPropertyTests() =
    let registration = Registry.registration { HostContext.create "." with Permissions = HostPermissions.Threading }
    let load extra source = ScriptHost.loadSourceWithRegistration { registration with Functions = registration.Functions @ extra } source
    let external name argument result implementation: ExternalFunction =
        { Name = name; RequiredPermissions = HostPermissions.None; Scheme = Forall([], TFun(argument, result)); Arity = 1; Impl = fun _ args -> implementation args.Head }

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.FiniteTaskGraphsMatchPureFold(seed: uint64) =
        verify "TaskPropertyTests.FiniteTaskGraphsMatchPureFold" 60 inputs seed (fun input ->
            let mutable expected = int64 input.Number
            let mutable prior = "start"
            let number = integerLiteral (int64 input.Number)
            let mutable source = $"let start = Task.spawn (fun () -> {number})\n"
            for index, offset in List.indexed input.Numbers do
                let multiplier = offset % 3
                expected <- expected * int64 multiplier + int64 offset
                let next = $"step{index}"
                let multiply, add = integerLiteral (int64 multiplier), integerLiteral (int64 offset)
                source <- source + $"let {next} = Task.continueWith (fun value -> value * {multiply} + {add}) {prior}\n"
                prior <- next
            let jobs = input.Numbers |> List.map (fun n -> $"Task.spawn (fun () -> {integerLiteral (int64 n)})") |> String.concat "; "
            source <- source + $"let jobs = [{jobs}]\nlet duplicate = Task.whenAll (fun values -> values) [{prior}; {prior}] |> Task.await\n"
            source <- source + "let ordered = Task.whenAll (fun values -> values) jobs |> Task.await\n"
            source <- source + $"let ready = Task.fromResult {number}\nlet winner = Task.whenAny (fun value -> value + 1) [ready; ready] |> Task.await\n"
            let delay = abs input.Number % 4
            source <- source + $"let nested = Task.fromResult (Task.fromResult {number}) |> Task.await |> Task.await\nTask.delay {delay} |> Task.await\n"
            source <- source + $"(Task.await {prior}, duplicate, ordered, Task.awaitAll jobs, winner, nested)"
            let result = load [] source
            value (VTuple [VInt expected; VList [VInt expected; VInt expected]; integers input.Numbers; integers input.Numbers; VInt(int64 input.Number + 1L); VInt(int64 input.Number)]) result.LastValue)

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.CompositeResultsSurviveTypeChangingAndNestedContinuations(seed: uint64) =
        verify "TaskPropertyTests.CompositeResultsSurviveTypeChangingAndNestedContinuations" 60 inputs seed (fun input ->
            let payload = VTuple [integers input.Numbers; VString input.Text]
            let supply = external "Host.payload" TUnit (TTuple [TList TInt; TString]) (fun _ -> payload)
            let source =
                "let ready = Task.fromResult 0 |> Task.continueWith (fun _ -> Host.payload ())\n"
                + "let nested = Task.spawn (fun () -> ready) |> Task.continueWith (fun task -> Task.await task)\n"
                + "(Task.await nested, Task.whenAll (fun values -> values) [ready; ready] |> Task.await)"
            value (VTuple [payload; VList [payload; payload]]) (load [supply] source).LastValue)

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.FailedPrerequisitesNeverInvokeContinuations(seed: uint64) =
        verify "TaskPropertyTests.FailedPrerequisitesNeverInvokeContinuations" 50 inputs seed (fun input ->
            let mutable calls = 0
            let tick = external "Host.tick" TInt TInt (fun value -> Interlocked.Increment(&calls) |> ignore; value)
            let source = $"Task.spawn (fun () -> raise \"ffi-source-failure\") |> Task.continueWith (fun (value: int) -> Host.tick (value + {integerLiteral (int64 input.Number)})) |> Task.await"
            let error = raises<EvalException> (fun () -> load [tick] source |> ignore)
            contains "ffi-source-failure" error.Message
            equal 0 calls)

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.ScopeJoinsEffectsAndCollectsUnobservedFailures(seed: uint64) =
        verify "TaskPropertyTests.ScopeJoinsEffectsAndCollectsUnobservedFailures" 50 inputs seed (fun input ->
            let effects = ConcurrentQueue<int64>()
            let record = external "Host.record" TInt TInt (function VInt n -> effects.Enqueue n; VInt n | _ -> failwith "Expected integer")
            let successes = input.Numbers |> List.mapi (fun index n -> $"let success{index} = Task.spawn (fun () -> Host.record {integerLiteral (int64 n)})\n") |> String.concat ""
            let markers = [for index in 0 .. abs input.Number % 4 -> $"ffi-unobserved-{index}-end"]
            let failures = markers |> List.mapi (fun index marker -> $"let failure{index} = Task.spawn (fun () -> raise {quote marker})\n") |> String.concat ""
            let error = raises<EvalException> (fun () -> load [record] (successes + failures + "7") |> ignore)
            for marker in markers do equal 2 (error.Message.Split([|marker|], StringSplitOptions.None).Length)
            equal (input.Numbers |> List.map int64 |> List.sort) (effects |> Seq.toList |> List.sort))

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.WhenAnySelectsReadyInputWithoutObservingPendingInput(seed: uint64) =
        verify "TaskPropertyTests.WhenAnySelectsReadyInputWithoutObservingPendingInput" 30 inputs seed (fun input ->
            let pending = TaskCompletionSource<TaskOutcome>(TaskCreationOptions.RunContinuationsAsynchronously)
            use deadline = new CancellationTokenSource(TimeSpan.FromSeconds 5.0)
            use watchdog = deadline.Token.Register(Action(fun () ->
                pending.TrySetResult(TaskFailed { Message = "FFI property pending-task deadline"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) }) |> ignore))
            let pendingHandle = { Worker = pending.Task; DebugTaskId = None; Awaited = false }
            let readyHandle = { Worker = Task.FromResult(TaskSucceeded(VInt(int64 input.Number))); DebugTaskId = None; Awaited = false }
            let tasks = external "Host.tasks" TUnit (TList(TTask TInt)) (fun _ -> VList [VTask pendingHandle; VTask readyHandle])
            try
                let result = load [tasks] "Task.whenAny (fun value -> value + 1) (Host.tasks ()) |> Task.await"
                value (VInt(int64 input.Number + 1L)) result.LastValue
                equal true readyHandle.Awaited
                equal false pendingHandle.Awaited
                equal false deadline.IsCancellationRequested
            finally pending.TrySetResult(TaskSucceeded(VInt 0L)) |> ignore)

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.InvalidTaskBoundsAndEmptyAnyFail(seed: uint64) =
        verify "TaskPropertyTests.InvalidTaskBoundsAndEmptyAnyFail" 50 inputs seed (fun input ->
            for milliseconds in [-1L - int64(abs input.Number); 2147483648L + int64(abs input.Number)] do
                let error = raises<EvalException> (fun () -> load [] $"Task.delay {integerLiteral milliseconds} |> ignore" |> ignore)
                contains "0..2147483647" error.Message
            let error = raises<EvalException> (fun () -> load [] "Task.whenAny (fun (value: int) -> value) [] |> Task.await" |> ignore)
            contains "non-empty" error.Message)
