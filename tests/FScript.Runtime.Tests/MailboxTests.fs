namespace FScript.Runtime.Tests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type MailboxTests() =
    let host = { HostContext.create "." with Permissions = HostPermissions.Threading }
    let load source =
        ScriptHost.loadSourceWithRegistration (Registry.registration host) source

    [<Test>]
    member _.ConcurrentProducers() =
        let sample = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "../../samples/mailbox.fss"))
        Assert.That((load sample).LastValue |> Pretty.valueToString, Is.EqualTo("15"))
        Assert.Throws<TypeException>(Action(fun () ->
            ScriptHost.loadSourceWithRegistration HostInterop.empty sample |> ignore)) |> ignore

    [<Test>]
    member _.GenericHandleInScriptRecordAndFifo() =
        let source = """
type Collection = { Inbox: Mailbox<string, string> }
let collect state message =
    Task.await (Task.delay 1)
    String.concat "" [state; message]
let collection = { Inbox = Mailbox.start 1 collect "" }
Mailbox.post "a" collection.Inbox
Mailbox.post "b" collection.Inbox
Mailbox.post "c" collection.Inbox
(Mailbox.finish collection.Inbox, Mailbox.finish collection.Inbox)
"""
        Assert.That((load source).LastValue |> Pretty.valueToString, Is.EqualTo("(\"abc\", \"abc\")"))

    [<Test>]
    member _.DistinctHandlesHaveDistinctIdentity() =
        let source = """
let handle state message = state + message
let first = Mailbox.start 1 handle 0
let second = Mailbox.start 1 handle 0
(first = first, first = second, List.distinct [first; second; first] |> List.length)
"""
        Assert.That((load source).LastValue |> Pretty.valueToString, Is.EqualTo("(true, false, 2)"))

    [<TestCase("VHostManaged")>]
    [<TestCase("let inbox = Mailbox.start 1 (fun state message -> state + message) 0\ninbox.Payload")>]
    member _.ProviderRepresentationIsOpaque(source) =
        Assert.Throws<TypeException>(Action(fun () -> load source |> ignore)) |> ignore

    [<Test>]
    member _.OneMailboxKeepsItsMessageType() =
        let source = """
let inbox = Mailbox.start 1 (fun state message -> message :: state) []
Mailbox.post 1 inbox
Mailbox.post "two" inbox
Mailbox.finish inbox
"""
        Assert.Throws<TypeException>(Action(fun () -> load source |> ignore)) |> ignore

    [<TestCase("Mailbox.start 0 (fun state _ -> state) 0", "capacity")>]
    [<TestCase("let inbox = Mailbox.start 1 (fun state message -> state + message) 0\nMailbox.finish inbox |> ignore\nMailbox.post 1 inbox", "finished mailbox")>]
    [<TestCase("let inbox = Mailbox.start 1 (fun (_: int) (_: int) -> raise \"handler failed\") 0\nMailbox.post 1 inbox\nMailbox.finish inbox", "handler failed")>]
    member _.FailuresReachTheScript(source, expected: string) =
        let error = Assert.Throws<EvalException>(Action(fun () -> load source |> ignore)) |> nonNull
        Assert.That(error.Message, Does.Contain(expected))

    [<Test>]
    member _.FinishDrainsPendingPostsAndRejectsNewOnes() =
        let functions = (MailboxProvider.register ()).Functions |> List.map (fun fn -> fn.Name, fn.Impl) |> Map.ofList
        use releaseWorker = new ManualResetEventSlim(false)
        use postWaiting = new ManualResetEventSlim(false)
        use finishWaiting = new ManualResetEventSlim(false)
        let rec context signal =
            FScript.Extensibility.ExternalCallContext(
                (fun fn value ->
                    match fn, value with
                    | FScript.Extensibility.VInt state, FScript.Extensibility.VInt message ->
                        FScript.Extensibility.VInt(state + message)
                    | _ -> value),
                (fun _ -> failwith "Unexpected script task"),
                (fun _ -> failwith "Unexpected task await"), signal,
                (fun run ->
                    Task.Run(Action(fun () ->
                        if not (releaseWorker.Wait(TimeSpan.FromSeconds 5.0)) then failwith "Worker was not released"
                        run (context ignore))) |> ignore
                    FScript.Extensibility.VUnit))
        let invoke name ctx args = functions.["Mailbox." + name] ctx args
        let integer = FScript.Extensibility.VInt
        let initial = context ignore
        let inbox = invoke "start" initial [integer 1L; FScript.Extensibility.VUnit; integer 0L]
        invoke "post" initial [integer 1L; inbox] |> ignore
        let pending = Task.Run(fun () -> invoke "post" (context postWaiting.Set) [integer 2L; inbox])
        Assert.That(postWaiting.Wait(TimeSpan.FromSeconds 5.0), Is.True)
        let finish = Task.Run(fun () -> invoke "finish" (context finishWaiting.Set) [inbox])
        Assert.That(finishWaiting.Wait(TimeSpan.FromSeconds 5.0), Is.True)
        try
            Assert.Throws<FScript.Extensibility.ExtensionException>(Action(fun () ->
                invoke "post" initial [integer 3L; inbox] |> ignore)) |> ignore
        finally
            releaseWorker.Set()
        Assert.That(pending.Wait(TimeSpan.FromSeconds 5.0), Is.True)
        Assert.That(finish.Wait(TimeSpan.FromSeconds 5.0), Is.True)
        Assert.That(finish.Result, Is.EqualTo(integer 3L))
