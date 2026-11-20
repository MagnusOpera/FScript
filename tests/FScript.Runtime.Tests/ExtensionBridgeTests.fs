namespace FScript.Runtime.Tests

open System
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type ExtensionBridgeTests() =
    let run functions source =
        let registration = ExtensionBridge.toLanguageRegistrationWithPermissions FScript.Language.HostPermissions.All { FScript.Extensibility.HostInterop.empty with Functions = functions }
        ScriptHost.loadSourceWithRegistration registration source
    let functionOf name signature impl : FScript.Extensibility.ExternalFunction =
        { Name = name; Arity = 1; Scheme = FScript.Extensibility.Forall([], signature); Impl = impl }
    let callableType = FScript.Extensibility.TFun(FScript.Extensibility.TInt, FScript.Extensibility.TInt)

    [<Test>]
    member _.``Handles survive separate extension calls in one session`` () =
        let mutable saved = FScript.Extensibility.VUnit
        let save = functionOf "Host.save" (FScript.Extensibility.TFun(callableType, FScript.Extensibility.TUnit)) (fun _ args -> saved <- List.head args; FScript.Extensibility.VUnit)
        let call = functionOf "Host.call" (FScript.Extensibility.TFun(FScript.Extensibility.TInt, FScript.Extensibility.TInt)) (fun context args -> context.Apply saved (List.head args))
        let result = run [save; call] "Host.save (fun x -> x + 1)\nHost.call 41"
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("42"))
        let error = Assert.Throws<EvalException>(Action(fun () -> run [call] "Host.call 41" |> ignore)) |> nonNull
        Assert.That(error.Data0.Message, Does.Contain("Foreign or expired"))

    [<Test>]
    member _.``Invented handles and expired contexts are rejected`` () =
        let mutable saved: FScript.Extensibility.ExternalCallContext option = None
        let remember = functionOf "Host.remember" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit, FScript.Extensibility.TUnit)) (fun context _ -> saved <- Some context; FScript.Extensibility.VUnit)
        run [remember] "Host.remember ()" |> ignore
        let error = Assert.Throws<FScript.Extensibility.ExtensionException>(Action(fun () -> saved.Value.CheckRuntime())) |> nonNull
        Assert.That(error.Message, Does.Contain("expired"))
        let forged = functionOf "Host.forged" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit, callableType)) (fun _ _ -> FScript.Extensibility.VCallable { new FScript.Extensibility.ICallableHandle })
        let error = Assert.Throws<EvalException>(Action(fun () -> run [forged] "Host.forged ()" |> ignore)) |> nonNull
        Assert.That(error.Data0.Message, Does.Contain("Foreign or expired"))

    [<Test>]
    member _.``Scripts await tasks exposed by libraries`` () =
        let ready = functionOf "Host.ready" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit,FScript.Extensibility.TTask FScript.Extensibility.TInt))
                        (fun context _ -> context.ExposeAsync(fun _ -> System.Threading.Tasks.Task.FromResult(FScript.Extensibility.VInt 42L)))
        let sdk = {FScript.Extensibility.HostInterop.empty with Functions=[ready]}
        let library = sdk |> ExtensionBridge.toLanguageRegistration
        let registration = {library with Permissions=HostPermissions.All; Functions=library.Functions @ TaskFunctions.all}
        let result = ScriptHost.loadSourceWithRegistration registration "Host.ready () |> Task.await"
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("42"))

    [<Test>]
    member _.``Default functions can be supplied and filtered through SDK context`` () =
        let nativeContext: HostContext = { Permissions = FScript.Language.HostPermissions.All; RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
        let context = FScript.Extensibility.HostContext(nativeContext.RootDirectory, [], ExtensionBridge.toExtensionFunctions (Registry.all nativeContext))
        let functions = context.DefaultFunctions |> List.filter (fun fn -> fn.Name = "Guid.new")
        Assert.That(functions, Has.Length.EqualTo(1))
        let result = run functions "Guid.new ()"
        match result.LastValue with
        | VOption(Some(VString value)) -> Assert.That(Guid.TryParse value |> fst, Is.True)
        | value -> Assert.Fail($"Unexpected value: {value}")

    [<Test>]
    member _.``Callback failures retain script source locations`` () =
        let call = functionOf "Host.call" (FScript.Extensibility.TFun(FScript.Extensibility.TFun(FScript.Extensibility.TUnit, FScript.Extensibility.TInt), FScript.Extensibility.TInt))
                       (fun context args -> context.Apply (List.head args) FScript.Extensibility.VUnit)
        let error = Assert.Throws<EvalException>(Action(fun () -> run [call] "Host.call (fun () -> raise \"callback failed\")" |> ignore)) |> nonNull
        Assert.That(error.Data0.Span.Start.Line, Is.GreaterThan(0))

    [<Test>]
    member _.``Async task handles pass through libraries without library awaiting`` () =
        let ready = functionOf "Host.ready" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit,FScript.Extensibility.TTask FScript.Extensibility.TInt))
                        (fun context _ -> context.ExposeAsync(fun _ -> System.Threading.Tasks.Task.FromResult(FScript.Extensibility.VInt 42L)))
        let pass = functionOf "Host.pass" (FScript.Extensibility.TFun(FScript.Extensibility.TTask FScript.Extensibility.TInt,FScript.Extensibility.TTask FScript.Extensibility.TInt))
                        (fun _ args -> args.Head)
        let library = ExtensionBridge.toLanguageRegistration {FScript.Extensibility.HostInterop.empty with Functions=[ready;pass]}
        let registration = {library with Permissions=HostPermissions.All; Functions=library.Functions @ TaskFunctions.all}
        let result = ScriptHost.loadSourceWithRegistration registration "Host.ready () |> Host.pass |> Task.await"
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("42"))

    [<Test>]
    member _.``SDK runtime checks delegate to the owning host`` () =
        let check = functionOf "Host.check" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit, FScript.Extensibility.TUnit))
                        (fun context _ -> context.CheckRuntime(); FScript.Extensibility.VUnit)
        let registration = ExtensionBridge.toLanguageRegistrationWithPermissions FScript.Language.HostPermissions.All { FScript.Extensibility.HostInterop.empty with Functions = [check] }
        let context: ExternalCallContext =
            { CheckPermissions = (fun _ -> ()); Session = obj(); IsActive = fun () -> true
              Apply = fun _ _ -> failwith "unused"
              SpawnTask = fun _ -> failwith "unused"
              AwaitTask = fun _ -> failwith "unused"
              SetExitCode = ignore
              CheckRuntime = fun () -> raise (OperationCanceledException "host limit") }
        let error = Assert.Throws<OperationCanceledException>(Action(fun () -> registration.Functions.Head.Impl context [VUnit] |> ignore)) |> nonNull
        Assert.That(error.Message, Is.EqualTo("host limit"))

    [<Test>]
    member _.``Async I/O registration neither spawns a worker nor requires Threading`` () =
        use cancellation = new System.Threading.CancellationTokenSource(2000)
        let completion = System.Threading.Tasks.TaskCompletionSource<FScript.Extensibility.ScriptValue>()
        use cancel = cancellation.Token.Register(fun () -> completion.TrySetCanceled() |> ignore)
        let callerThread = Environment.CurrentManagedThreadId
        let mutable factoryThread = 0
        let read = functionOf "Host.read" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit,FScript.Extensibility.TTask FScript.Extensibility.TInt))
                       (fun context _ -> context.ExposeAsync(fun _ -> factoryThread <- Environment.CurrentManagedThreadId; completion.Task))
        let complete = functionOf "Host.complete" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit,FScript.Extensibility.TUnit))
                           (fun _ _ -> completion.SetResult(FScript.Extensibility.VInt 7L); FScript.Extensibility.VUnit)
        let registration = ExtensionBridge.toLanguageRegistrationWithPermissions HostPermissions.Native {FScript.Extensibility.HostInterop.empty with Functions=[read;complete]}
        let program = Parser.parseProgram "let pending = Host.read ()\nHost.complete ()\n42" |> TypeInfer.inferProgramWithRegistration registration
        let result = Eval.evalProgramWithCancellation cancellation.Token registration program
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("42"))
        Assert.That(factoryThread, Is.EqualTo(callerThread))

    [<Test>]
    member _.``Async I/O is canceled and joined when the caller is canceled`` () =
        let mutable cleanedUp = false
        let read = functionOf "Host.read" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit,FScript.Extensibility.TTask FScript.Extensibility.TInt))
                       (fun context _ -> context.ExposeAsync(fun token -> task {
                           try
                               do! System.Threading.Tasks.Task.Delay(30000,token)
                               return FScript.Extensibility.VInt 42L
                           finally cleanedUp <- true
                       }))
        let registration = ExtensionBridge.toLanguageRegistrationWithPermissions HostPermissions.Native {FScript.Extensibility.HostInterop.empty with Functions=[read]}
        let program = Parser.parseProgram "let pending = Host.read ()\n42" |> TypeInfer.inferProgramWithRegistration registration
        use cancellation = new System.Threading.CancellationTokenSource(100)
        Assert.Catch<Exception>(Action(fun () -> Eval.evalProgramWithCancellation cancellation.Token registration program |> ignore)) |> ignore
        Assert.That(cleanedUp, Is.True)
