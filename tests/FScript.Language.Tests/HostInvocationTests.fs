namespace FScript.Language.Tests

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language

[<TestFixture>]
type HostInvocationTests() =
    let typed registration source = Parser.parseProgram source |> TypeInfer.inferProgramWithRegistration registration

    [<Test>]
    member _.``Invocation metadata reports active grants and expires after return`` () =
        let mutable captured = None
        let fn : ExternalFunction =
            {Name="Host.grants";Scheme=Forall([],TFun(TUnit,TInt));Arity=1;RequiredPermissions=HostPermissions.None
             Impl=fun context _ -> captured <- Some context; VInt(int64 (int (HostInterop.currentPermissions context)))}
        let registration = HostInterop.ofExternsWithPermissions HostPermissions.Threading [fn]
        let result = typed registration "Host.grants ()" |> Eval.evalProgramWithRegistration registration
        Assert.That(Pretty.valueToString result, Is.EqualTo("2"))
        Assert.Throws<EvalException>(Action(fun () -> HostInterop.currentPermissions captured.Value |> ignore)) |> ignore

    [<Test>]
    member _.``Async FFI bridge checks Threading before starting work`` () =
        let mutable started = false
        let fn : ExternalFunction =
            {Name="Host.work";Scheme=Forall([],TFun(TUnit,TTask TString));Arity=1;RequiredPermissions=HostPermissions.None
             Impl=fun context _ -> HostInterop.startTask context (fun _ -> started <- true; Task.FromResult(VString "bad"))}
        let registration = HostInterop.ofExterns [fn]
        Assert.Throws<EvalException>(Action(fun () -> typed registration "Host.work () |> ignore" |> Eval.evalProgramWithRegistration registration |> ignore)) |> ignore
        Assert.That(started, Is.False)

    [<Test>]
    member _.``Generic cancellation stops pure evaluation`` () =
        let registration = HostInterop.empty
        let program = typed registration "let rec loop n = loop (n + 1)\nloop 0"
        use cancelled = new CancellationTokenSource(50)
        Assert.Catch<OperationCanceledException>(Action(fun () -> Eval.evalProgramWithCancellation cancelled.Token registration program |> ignore)) |> ignore

    [<Test>]
    member _.``Async FFI is joined on cancellation even when unawaited`` () =
        let mutable finished = false
        let fn : ExternalFunction =
            {Name="Host.work";Scheme=Forall([],TFun(TUnit,TTask TString));Arity=1;RequiredPermissions=HostPermissions.Threading
             Impl=fun context _ -> HostInterop.startTask context (fun cancellation -> task {
                 try
                     do! Task.Delay(30000,cancellation)
                     return VString "done"
                 finally finished <- true
             })}
        let registration = HostInterop.ofExternsWithPermissions HostPermissions.Threading [fn]
        let program = typed registration "let pending = Host.work ()\n42"
        use cancelled = new CancellationTokenSource(100)
        let timer = Diagnostics.Stopwatch.StartNew()
        Assert.Catch<Exception>(Action(fun () -> Eval.evalProgramWithCancellation cancelled.Token registration program |> ignore)) |> ignore
        Assert.That(finished, Is.True)
        Assert.That(timer.Elapsed.TotalSeconds, Is.LessThan(5))
