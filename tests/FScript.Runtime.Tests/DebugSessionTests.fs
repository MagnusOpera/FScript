namespace FScript.Runtime.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type DebugSessionTests() =
    let createWith extensions scheduler source stop =
        let path = System.IO.Path.GetFullPath("debug-test.fss")
        let program = FScript.parseWithSourceName (Some path) source
        let registration = HostInterop.ofExterns (TaskExterns.all @ extensions)
        let session = new DebugSession(registration,program,stop,taskScheduler = scheduler)
        let events = new BlockingCollection<DebugEvent>()
        session.Events.Add events.Add
        let completion = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
        let thread = Thread(ThreadStart(fun () -> try session.Run(); completion.SetResult(()) with error -> completion.TrySetException(error) |> ignore))
        thread.IsBackground <- true
        session,events,completion.Task,thread,path
    let create source stop = createWith [] TaskScheduler.Default source stop
    let nextStop (events: BlockingCollection<DebugEvent>) =
        let mutable result = Unchecked.defaultof<DebugEvent>
        let mutable found = false
        while not found do
            if not (events.TryTake(&result,5000)) then Assert.Fail("Timed out waiting for debugger stop.")
            found <- result.Kind = "stopped"
        result

    [<Test>]
    member _.``Entry inspection is safe and old handles expire``() =
        let session,events,completion,thread,_ = create "type Secret = { Value: int }\nlet secret = { Value = 42 }\nlet read x = x.Value\nread secret" true
        use session = session
        thread.Start()
        let stopped = nextStop events
        let frame = session.StackTrace(stopped.TaskId)[0]
        let _ = session.Scopes frame.Id
        Assert.Throws<ArgumentException>(Action(fun () -> session.Evaluate(frame.Id,"Console.writeLine \"oops\"") |> ignore)) |> ignore
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)
        Assert.Throws<ArgumentException>(Action(fun () -> session.Scopes frame.Id |> ignore)) |> ignore

    [<Test>]
    member _.``Paused worker does not hold a scheduler worker or stop independent tasks``() =
        let source = "let worker x = x + 1\nlet a = Task.spawn (fun _ -> worker 41)\nlet b = Task.spawn (fun _ -> 2 + 3)\n(Task.await a, Task.await b)"
        let scheduler = new ConcurrentExclusiveSchedulerPair(TaskScheduler.Default,1)
        let session,events,completion,thread,path = createWith [] scheduler.ConcurrentScheduler source false
        use session = session
        session.SetBreakpoints(path,[(1,16)]) |> ignore
        thread.Start()
        let stopped = nextStop events
        Assert.That(stopped.TaskId,Is.Not.EqualTo(1))
        let mutable exited = false
        let deadline = DateTime.UtcNow.AddSeconds(5.)
        while not exited && DateTime.UtcNow < deadline do
            let mutable event = Unchecked.defaultof<DebugEvent>
            if events.TryTake(&event,100) then exited <- event.Kind = "threadExited" && event.TaskId <> stopped.TaskId
        Assert.That(exited,Is.True,"Independent task must complete while worker is paused")
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Stepping enters List map callback and displays lexical locals``() =
        let session,events,completion,thread,path = create "let transform x =\n    let local = x + 1\n    local * 2\n[1;2] |> List.map transform" false
        use session = session
        session.SetBreakpoints(path,[(3,5)]) |> ignore
        thread.Start()
        let stopped = nextStop events
        let frames = session.StackTrace stopped.TaskId
        Assert.That(frames.Length,Is.GreaterThan(1))
        Assert.That(session.Evaluate(frames[0].Id,"local").Value,Is.EqualTo("2"))
        session.SetBreakpoints(path,[]) |> ignore
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Two paused tasks have independent frame and value generations``() =
        let source = "let work x = x + 1\nlet a = Task.spawn (fun _ -> work 1)\nlet b = Task.spawn (fun _ -> work 2)\n(Task.await a, Task.await b)"
        let session,events,completion,thread,path = create source false
        use session = session
        session.SetBreakpoints(path,[(1,14)]) |> ignore
        thread.Start()
        let a = nextStop events
        let b = nextStop events
        Assert.That(a.TaskId,Is.Not.EqualTo(b.TaskId))
        let frameA = session.StackTrace(a.TaskId)[0]
        let frameB = session.StackTrace(b.TaskId)[0]
        let scopeB = session.Scopes(frameB.Id)[0]
        session.SetBreakpoints(path,[]) |> ignore
        session.Resume(a.TaskId,"continue",true)
        Assert.That(session.Evaluate(frameB.Id,"x").Value,Is.Not.Empty)
        Assert.That(session.Variables(scopeB.VariablesReference,0,10).Length,Is.EqualTo(1))
        Assert.Throws<ArgumentException>(Action(fun () -> session.Scopes(frameA.Id) |> ignore)) |> ignore
        session.Resume(b.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Host values expose visible snapshots and never payloads``() =
        let descriptor = { Name = "Node"; Fields = Map.ofList ["Value",TInt]; HostManaged = true }
        let registration = { HostInterop.empty with Types = [descriptor] }
        let node = HostInterop.create registration "Node" (Map.ofList ["Value",VInt 42L]) ("TOP SECRET PAYLOAD" :> obj)
        let registration = { registration with Functions = [{ Name = "Host.node"; Scheme = Forall([],TNamed "Node"); Arity = 0; Impl = fun _ _ -> node }] }
        let path = System.IO.Path.GetFullPath("host-debug-test.fss")
        let program = FScript.parseWithSourceName (Some path) "let node = Host.node\nnode.Value"
        use session = new DebugSession(registration,program,false)
        session.SetBreakpoints(path,[(2,1)]) |> ignore
        let events = new BlockingCollection<DebugEvent>()
        session.Events.Add events.Add
        let completion = Task.Run(fun () -> session.Run())
        let stopped = nextStop events
        let frame = session.StackTrace(stopped.TaskId)[0]
        let variable = session.Evaluate(frame.Id,"node")
        let fields = session.Variables(variable.VariablesReference,0,100)
        Assert.That(fields |> Array.map (fun field -> field.Name),Is.EqualTo<string>([|"Value"|]))
        Assert.That(fields[0].Value,Is.EqualTo("42"))
        Assert.That(System.Text.Json.JsonSerializer.Serialize(variable),Does.Not.Contain("TOP SECRET"))
        Assert.Throws<ArgumentException>(Action(fun () -> session.Evaluate(frame.Id,"node.Payload") |> ignore)) |> ignore
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Synchronous host callbacks retain their native caller thread``() =
        let mutable hostThread = 0
        let mutable callbackThread = 0
        let callback =
            { Name = "Host.callback"; Scheme = Forall([],TFun(TFun(TInt,TInt),TInt)); Arity = 1
              Impl = fun context args ->
                hostThread <- Thread.CurrentThread.ManagedThreadId
                let result = context.Apply args.Head (VInt 41L)
                Assert.That(Thread.CurrentThread.ManagedThreadId,Is.EqualTo(hostThread))
                result }
        let observe =
            { Name = "Host.observe"; Scheme = Forall([],TFun(TInt,TInt)); Arity = 1
              Impl = fun _ args -> callbackThread <- Thread.CurrentThread.ManagedThreadId; args.Head }
        let session,events,completion,thread,path = createWith [callback;observe] TaskScheduler.Default "let f x = Host.observe (x + 1)\nHost.callback f" false
        use session = session
        session.SetBreakpoints(path,[(1,25)]) |> ignore
        thread.Start()
        let stopped = nextStop events
        session.SetBreakpoints(path,[]) |> ignore
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)
        Assert.That(callbackThread,Is.EqualTo(hostThread))

    [<Test>]
    member _.``Cancellation releases paused main and worker continuations``() =
        let session,events,completion,thread,path = create "let work x = x + 1\nlet a = Task.spawn (fun _ -> work 41)\nTask.await a" false
        use session = session
        session.SetBreakpoints(path,[(1,14)]) |> ignore
        thread.Start()
        nextStop events |> ignore
        session.Cancel()
        Assert.That(SpinWait.SpinUntil((fun () -> completion.IsCompleted),5000),Is.True)
        Assert.That(session.Threads().Length,Is.EqualTo(0))

    [<Test>]
    member _.``Uncaught faults retain the originating FScript stack``() =
        let session,events,completion,thread,_ = create "let crash x = raise \"boom\"\ncrash 1" false
        use session = session
        thread.Start()
        let stopped = nextStop events
        Assert.That(stopped.Reason,Is.EqualTo("exception"))
        let stack = session.StackTrace(stopped.TaskId)
        Assert.That(stack[0].Name,Is.EqualTo("crash"))
        Assert.That(session.ExceptionInfo(stopped.TaskId),Does.Contain("boom"))
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(SpinWait.SpinUntil((fun () -> completion.IsCompleted),5000),Is.True)
        Assert.That(completion.IsFaulted,Is.True)

    [<Test>]
    member _.``Locals expansion is bounded and breakpoint location queries do not replace breakpoints``() =
        let session,events,completion,thread,path = create "let values = [1..2000]\nList.length values" false
        use session = session
        let points = session.SetBreakpoints(path,[(2,1);(20,1)])
        Assert.That(points[0].Verified,Is.True)
        Assert.That(points[1].Verified,Is.False)
        Assert.That(session.BreakpointLocations(path,1,2).Length,Is.GreaterThan(0))
        thread.Start()
        let stopped = nextStop events
        let frame = session.StackTrace(stopped.TaskId)[0]
        let values = session.Evaluate(frame.Id,"values")
        Assert.That(values.IndexedVariables,Is.EqualTo(2000))
        Assert.That(session.Variables(values.VariablesReference,100,5000).Length,Is.EqualTo(1000))
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Shadowed locals do not overwrite parameter snapshots``() =
        let session,events,completion,thread,path = create "let x = 99\nlet f x =\n    let x = x + 1\n    x * 2\nf 41" false
        use session = session
        session.SetBreakpoints(path,[(4,5)]) |> ignore
        thread.Start()
        let stopped = nextStop events
        let frame = session.StackTrace(stopped.TaskId)[0]
        let scopes = session.Scopes frame.Id
        let parameters = session.Variables(scopes[0].VariablesReference,0,100)
        let locals = session.Variables(scopes[1].VariablesReference,0,100)
        let parameter = parameters |> Array.find (fun value -> value.Name = "x")
        let local = locals |> Array.find (fun value -> value.Name = "x")
        Assert.That(parameter.Value,Is.EqualTo("41"))
        Assert.That(local.Value,Is.EqualTo("42"))
        Assert.That(session.Evaluate(frame.Id,"x").Value,Is.EqualTo("42"))
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Uncaught exception policy does not stop exceptions recovered by a host callback``() =
        let recover =
            { Name = "Host.recover"; Scheme = Forall([],TFun(TFun(TUnit,TInt),TInt)); Arity = 1
              Impl = fun context arguments ->
                try context.Apply arguments.Head VUnit
                with EvalException _ -> VInt 99L }
        let session,events,completion,thread,_ = createWith [recover] TaskScheduler.Default "let fail () = raise \"recovered\"\nHost.recover fail" false
        use session = session
        thread.Start()
        Assert.That(completion.Wait(5000),Is.True)
        Assert.That(session.ResultSummary,Is.EqualTo("99"))
        Assert.That(events.ToArray() |> Array.exists (fun event -> event.Kind = "stopped"),Is.False)

    [<Test>]
    member _.``Small collection construction and list loops resume without repeating expressions``() =
        let source = "type Pair = { A: int; B: int }\nlet field x = x + 1\nlet pair = { A = field 10; B = field 20 }\nlet (a,b) = (field 30, field 40)\nfor x in [1;2] do\n    field x |> ignore\npair.A + pair.B + a + b"
        let session,events,completion,thread,path = create source false
        use session = session
        session.SetBreakpoints(path,[(2,15)]) |> ignore
        thread.Start()
        for expected in [10;20;30;40;1;2] do
            let stopped = nextStop events
            let frame = session.StackTrace(stopped.TaskId)[0]
            Assert.That(session.Evaluate(frame.Id,"x").Value,Is.EqualTo(string expected))
            session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)
        Assert.That(session.ResultSummary,Is.EqualTo("104"))

    [<Test>]
    member _.``Choose resumes selected and discarded callbacks without repetition`` () =
        let source = "let choose x =\n    let local = x + 1\n    if local % 2 = 0 then Some local else None\nList.choose choose [1; 2; 3]"
        let session,events,completion,thread,path = create source false
        use session = session
        session.SetBreakpoints(path,[(3,5)]) |> ignore
        thread.Start()
        for expected in [2;3;4] do
            let stopped = nextStop events
            let frame = session.StackTrace(stopped.TaskId)[0]
            Assert.That(session.Evaluate(frame.Id,"local").Value,Is.EqualTo(string expected))
            session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)
        Assert.That(session.ResultSummary,Is.EqualTo("[2;4]"))

    [<Test>]
    member _.``Task continuations and delay release the sole debugger scheduler worker`` () =
        let scheduler=new ConcurrentExclusiveSchedulerPair(TaskScheduler.Default,1)
        let source="let first=Task.continueWith (fun () -> 20) (Task.delay 5)\nlet second=Task.fromResult 22\nTask.whenAll (List.fold (fun total value -> total+value) 0) [first;second] |> Task.await"
        let session,_,completion,thread,_=createWith [] scheduler.ConcurrentScheduler source false
        use session=session
        thread.Start()
        Assert.That(completion.Wait(5000),Is.True,"A task wait must not occupy the only scheduler worker.")

    [<Test>]
    member _.``Continuation callbacks support breakpoints and lexical frames`` () =
        let source="let work value =\n    let local=value+1\n    local*2\nTask.continueWith work (Task.fromResult 20) |> Task.await"
        let session,events,completion,thread,path=create source false
        use session=session
        session.SetBreakpoints(path,[(3,5)]) |> ignore
        thread.Start()
        let stopped=nextStop events
        let frames=session.StackTrace stopped.TaskId
        Assert.That(frames.Length,Is.GreaterThanOrEqualTo(1))
        Assert.That(session.Evaluate(frames[0].Id,"local").Value,Is.EqualTo("21"))
        session.Resume(stopped.TaskId,"continue",true)
        Assert.That(completion.Wait(5000),Is.True)

    [<Test>]
    member _.``Cancelling the debugger interrupts a long task delay`` () =
        let session,events,completion,thread,_=create "Task.delay 60000 |> Task.await" false
        use session=session
        thread.Start()
        let mutable timerStarted=false
        let deadline=DateTime.UtcNow.AddSeconds(5.)
        while not timerStarted && DateTime.UtcNow<deadline do
            let mutable item=Unchecked.defaultof<DebugEvent>
            if events.TryTake(&item,100) then timerStarted <- item.Kind="threadStarted" && item.TaskId<>1
        Assert.That(timerStarted,Is.True)
        session.Cancel()
        Assert.That(SpinWait.SpinUntil((fun () -> completion.IsCompleted),5000),Is.True)
        Assert.That(completion.IsFaulted,Is.True)
        completion.Exception |> ignore
        Assert.That(thread.Join(1000),Is.True)
