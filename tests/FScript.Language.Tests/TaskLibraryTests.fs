namespace FScript.Language.Tests

open System
open System.Threading
open System.Threading.Tasks
open FScript.Language
open FScript.Tests
open NUnit.Framework

[<TestFixture>]
type TaskLibraryTests() =
    let evalWith extra source = Helpers.evalWithExterns (TaskFunctions.all @ extra) source
    let eval source = evalWith [] source |> Pretty.valueToString
    let foreignTask (worker: Task<TaskOutcome>) = { Worker=worker;DebugTaskId=None;Awaited=false }
    let external name result implementation : ExternalFunction =
        {Name=name;Scheme=Forall([],TFun(TUnit,result));Arity=1;Impl=fun _ _ -> implementation()}
    static member Cases = TaskLibraryCases.successes |> List.map (fun (name,source,expected) -> TestCaseData(source,expected).SetName("Task: "+name))
    static member Failures = TaskLibraryCases.failures |> List.map (fun (name,source,message) -> TestCaseData(source,message).SetName("Task error: "+name))

    [<TestCaseSource(nameof TaskLibraryTests.Cases)>]
    member _.``Task composition returns expected values`` (source: string,expected: string) = Assert.That(eval source,Is.EqualTo(expected))

    [<TestCaseSource(nameof TaskLibraryTests.Failures)>]
    member _.``Task failures preserve lifetime and error contracts`` (source: string,message: string) =
        let error=Assert.Throws<EvalException>(Action(fun () -> eval source |> ignore))
        Assert.That((nonNull error).ToString(),Does.Contain(message))

    [<Test>]
    member _.``WhenAny selects before callback scheduling and does not observe losers`` () =
        let pending=TaskCompletionSource<TaskOutcome>(TaskCreationOptions.RunContinuationsAsynchronously)
        let pendingHandle=foreignTask pending.Task
        let ready=foreignTask (Task.FromResult(TaskSucceeded(VInt 42L)))
        let functions=
            [ external "Host.pending" (TTask TInt) (fun () -> VTask pendingHandle)
              external "Host.ready" (TTask TInt) (fun () -> VTask ready) ]
        try
            let result=evalWith functions "Task.whenAny (fun value -> value) [Host.pending ();Host.ready ()] |> Task.await"
            Assert.That(Pretty.valueToString result,Is.EqualTo("42"))
            Assert.That(ready.Awaited,Is.True)
            Assert.That(pendingHandle.Awaited,Is.False)
        finally pending.TrySetResult(TaskSucceeded(VInt 0L)) |> ignore

    [<Test>]
    member _.``AwaitAll waits for every input even if an earlier input failed`` () =
        let pending=TaskCompletionSource<TaskOutcome>(TaskCreationOptions.RunContinuationsAsynchronously)
        let pendingHandle=foreignTask pending.Task
        let failed=foreignTask (Task.FromResult(TaskFailed {Message="expected failure";Span=Span.mk (Span.pos 0 0) (Span.pos 0 0)}))
        use entered=new ManualResetEventSlim()
        let functions=[external "Host.tasks" (TList(TTask TInt)) (fun () -> entered.Set(); VList[VTask failed;VTask pendingHandle])]
        let execution=Task.Run(fun () ->
            try evalWith functions "Task.awaitAll (Host.tasks ())" |> ignore; false
            with EvalException _ -> true)
        try
            Assert.That(entered.Wait(5000),Is.True)
            Assert.That(execution.Wait(100),Is.False,"A failure must not bypass waiting for later tasks.")
        finally pending.TrySetResult(TaskSucceeded(VInt 1L)) |> ignore
        Assert.That(execution.Wait(5000),Is.True)
        Assert.That(execution.Result,Is.True)
        Assert.That(pendingHandle.Awaited,Is.True)

    [<Test>]
    member _.``A failed prerequisite never invokes its callback`` () =
        let mutable calls=0
        let tick=external "Host.tick" TInt (fun () -> Interlocked.Increment(&calls) |> ignore; VInt 0L)
        Assert.Throws<EvalException>(Action(fun () -> evalWith [tick] "Task.continueWith (fun (_: int) -> Host.tick ()) (Task.spawn (fun () -> raise \"boom\")) |> Task.await" |> ignore)) |> ignore
        Assert.That(calls,Is.Zero)

    [<TestCase("[1; \"x\"]")>]
    [<TestCase("Task.whenAll List.length [Task.fromResult 1;Task.fromResult \"x\"]")>]
    [<TestCase("Task.whenAny (fun value -> value) [Task.fromResult 1;Task.fromResult ()]")>]
    member _.``Candidate results must be homogeneous`` (source: string) =
        Assert.Throws<TypeException>(Action(fun () -> Helpers.inferWithExterns TaskFunctions.all source |> ignore)) |> ignore

    [<Test>]
    member _.``Provider forwarding preserves canonical task scheduling`` () =
        let alias={TaskFunctions.continueWith with Name="Host.chain"; Impl=fun context args -> TaskFunctions.continueWith.Impl context args}
        Assert.That(evalWith [alias] "Host.chain (fun value -> value+1) (Task.fromResult 41) |> Task.await" |> Pretty.valueToString,Is.EqualTo("42"))

    [<TestCase("Task.continueWith (fun _ -> Host.tick ()) (Task.fromResult 1) |> Task.await")>]
    [<TestCase("Task.whenAll (fun _ -> Host.tick ()) [Task.fromResult 1;Task.fromResult 2] |> Task.await")>]
    [<TestCase("Task.whenAny (fun () -> Host.tick ()) [Task.fromResult ()] |> Task.await")>]
    member _.``Each continuation callback executes once`` (source: string) =
        let mutable calls=0
        let tick=external "Host.tick" TInt (fun () -> VInt(int64 (Interlocked.Increment(&calls))))
        Assert.That(evalWith [tick] source |> Pretty.valueToString,Is.EqualTo("1"))
        Assert.That(calls,Is.EqualTo(1))

    [<TestCase("Task.continueWith (Task.fromResult 41) (fun value -> value+1)")>]
    [<TestCase("Task.whenAll [Task.fromResult 1] List.length")>]
    [<TestCase("Task.whenAny [Task.fromResult 1] (fun value -> value)")>]
    member _.``Task combinators require callback first and data last`` (source: string) =
        Assert.Throws<TypeException>(Action(fun () -> Helpers.inferWithExterns TaskFunctions.all source |> ignore)) |> ignore
