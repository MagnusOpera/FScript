namespace FScript.Runtime.Tests

open System
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type ExtensionBridgeTests() =
    let run functions source =
        let registration = ExtensionBridge.toLanguageRegistration { FScript.Extensibility.HostInterop.empty with Functions = functions }
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
    member _.``Tasks cross the SDK boundary without exposing workers`` () =
        let execute = functionOf "Host.execute" (FScript.Extensibility.TFun(FScript.Extensibility.TFun(FScript.Extensibility.TUnit, FScript.Extensibility.TInt), FScript.Extensibility.TInt))
                          (fun context args -> context.SpawnTask (List.head args) |> context.AwaitTask)
        let result = run [execute] "Host.execute (fun () -> 42)"
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("42"))

    [<Test>]
    member _.``Default functions can be supplied and filtered through SDK context`` () =
        let nativeContext: HostContext = { RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] }
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
        let error = Assert.Throws<EvalException>(Action(fun () -> run [call] "Host.call (fun () -> match None with | Some x -> x)" |> ignore)) |> nonNull
        Assert.That(error.Data0.Span.Start.Line, Is.GreaterThan(0))

    [<Test>]
    member _.``Task results can pass through script before another SDK call awaits them`` () =
        let spawn = functionOf "Host.spawn" (FScript.Extensibility.TFun(FScript.Extensibility.TFun(FScript.Extensibility.TUnit, FScript.Extensibility.TInt), FScript.Extensibility.TTask FScript.Extensibility.TInt))
                        (fun context args -> context.SpawnTask (List.head args))
        let await = functionOf "Host.await" (FScript.Extensibility.TFun(FScript.Extensibility.TTask FScript.Extensibility.TInt, FScript.Extensibility.TInt))
                        (fun context args -> context.AwaitTask (List.head args))
        let result = run [spawn; await] "let task = Host.spawn (fun () -> 42)\nHost.await task"
        Assert.That(Pretty.valueToString result.LastValue, Is.EqualTo("42"))

    [<Test>]
    member _.``SDK runtime checks delegate to the owning host`` () =
        let check = functionOf "Host.check" (FScript.Extensibility.TFun(FScript.Extensibility.TUnit, FScript.Extensibility.TUnit))
                        (fun context _ -> context.CheckRuntime(); FScript.Extensibility.VUnit)
        let registration = ExtensionBridge.toLanguageRegistration { FScript.Extensibility.HostInterop.empty with Functions = [check] }
        let context: ExternalCallContext =
            { Session = obj(); IsActive = fun () -> true
              Apply = fun _ _ -> failwith "unused"
              SpawnTask = fun _ -> failwith "unused"
              AwaitTask = fun _ -> failwith "unused"
              SetExitCode = ignore
              CheckRuntime = fun () -> raise (OperationCanceledException "host limit") }
        let error = Assert.Throws<OperationCanceledException>(Action(fun () -> registration.Functions.Head.Impl context [VUnit] |> ignore)) |> nonNull
        Assert.That(error.Message, Is.EqualTo("host limit"))
