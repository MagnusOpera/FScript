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

    let analyzeMailbox options source =
        let counter = TNamed "Counter"
        let external name argument result =
            { Name = name; Scheme = Forall([], TFun(argument, result)); Arity = 1
              RequiredPermissions = HostPermissions.None
              Impl = fun _ _ -> failwith "Analysis must not execute host code" }
        let registration = Registry.registration host
        let registration =
            { registration with
                Types = registration.Types @ [{ Name = "Counter"; Fields = Map.empty; HostManaged = true; TypeParameters = [] }]
                Functions = registration.Functions @ [external "Host.make" TUnit counter; external "Host.read" counter TInt; external "Host.tick" TUnit TInt] }
        let options = TaskSharingContracts.withRuntime registration options
        Parser.parseProgram source
        |> TypeInfer.inferResolvedProgramWithRegistrationAndPurity registration PurityContracts.empty
        |> TaskSharingAnalysis.analyze registration options


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

    [<TestCase("let make () =\n    let inbox = Mailbox.start 1 (fun state message -> message :: state) []\n    fun () -> inbox\nlet get = make ()\nMailbox.post 1 (get ())\nMailbox.post \"two\" (get ())")>]
    [<TestCase("let make () =\n    let inbox = Mailbox.start 1 (fun state message -> message :: state) []\n    fun message -> Mailbox.post message inbox\nlet send = make ()\nlet alias = if true then send else send\nalias 1\nsend \"two\"")>]
    [<TestCase("let make () =\n    let inbox = Mailbox.start 1 (fun state (_: int) -> state) []\n    fun () -> Mailbox.finish inbox\nlet read = make ()\nlet ints (values: int list) = values\nlet words (values: string list) = values\n(ints (read ()), words (read ()))")>]
    [<TestCase("let make () =\n    let inbox = Mailbox.start 1 (fun state message -> message :: state) []\n    fun message -> Mailbox.post message inbox\nlet wrap factory =\n    let send = factory ()\n    fun message -> send message\nlet send = wrap make\nsend 1\nsend \"two\"")>]
    [<TestCase("let (send, read) = (fun inbox -> ((fun value -> Mailbox.post value inbox), (fun () -> Mailbox.finish inbox))) (Mailbox.start 1 (fun state message -> message :: state) [])\nsend 1\nlet words (values: string list) = values\nwords (read ())")>]
    [<TestCase("let make () =\n    let inbox = Mailbox.start 1 (fun state message -> message :: state) []\n    fun message -> Mailbox.post message inbox\nlet callbacks = [make ()]\nlet send = match callbacks with | first :: _ -> first | [] -> fun _ -> ()\nsend 1\nsend \"two\"")>]
    member _.CapturedInstancesRetainTheirTypes(source: string) =
        Assert.Throws<TypeException>(Action(fun () ->
            Parser.parseProgram source |> TypeInfer.inferProgramWithRegistration (Registry.registration host) |> ignore)) |> ignore

    [<TestCase("let pair = (Mailbox.start 1 (fun state (_: int) -> state) 0, fun value -> value)\nlet (inbox, identity) = pair\n(identity 1, identity \"two\", Mailbox.finish inbox)", "(1, \"two\", 0)")>]
    [<TestCase("let (send, identity, finish) = (fun inbox -> ((fun message -> Mailbox.post message inbox), (if true then (fun value -> value) else (fun value -> value)), (fun () -> Mailbox.finish inbox))) (Mailbox.start 1 (fun state message -> message :: state) [])\nsend 42\n(finish (), identity 1, identity \"two\")", "([42], 1, \"two\")")>]
    [<TestCase("let create = Mailbox.start 1 (fun state (_: int) -> state)\nlet numbers = create [42]\nlet words = create [\"ok\"]\n(Mailbox.finish numbers, Mailbox.finish words)", "([42], [\"ok\"])")>]
    [<TestCase("let create state = Mailbox.start 1 (fun state (_: int) -> state) state\nlet identity value = value\nlet factory = identity create\nlet numbers = factory [42]\nlet words = factory [\"ok\"]\n(Mailbox.finish numbers, Mailbox.finish words)", "([42], [\"ok\"])")>]
    member _.IndependentCallbacksAndFactoriesRemainPolymorphic(source: string, expected: string) =
        Assert.That((load source).LastValue |> Pretty.valueToString, Is.EqualTo(expected))

    [<TestCase("let inbox = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nMailbox.post 1 inbox\nHost.tick () |> ignore\nMailbox.finish inbox", "FST1002")>]
    [<TestCase("let counter = Host.make ()\nlet inbox = Mailbox.start 1 (fun state (_: int) -> Host.read counter) 0\nMailbox.post 1 inbox\nHost.read counter |> ignore\nMailbox.finish inbox", "FST1001")>]
    [<TestCase("let counter = Host.make ()\nlet inbox = Mailbox.start 1 (fun state (_: int) -> if Host.read state = 0 then state else state) counter\nMailbox.post 1 inbox\nHost.read counter |> ignore\nMailbox.finish inbox", "FST1001")>]
    [<TestCase("let counter = Host.make ()\nlet inbox = Mailbox.start 1 (fun state message -> Host.read message) 0\nMailbox.post counter inbox\nHost.read counter |> ignore\nMailbox.finish inbox", "FST1001")>]
    [<TestCase("let first = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nlet second = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nMailbox.post 1 first\nMailbox.post 1 second\n(Mailbox.finish first, Mailbox.finish second)", "FST1002")>]
    member _.AnalysisReportsConsumerConcurrency(source: string, code: string) =
        let warnings = analyzeMailbox TaskSharingAnalysis.defaults source
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = code), Is.True, string warnings)

    [<TestCase("let inbox = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nMailbox.post 1 inbox\nMailbox.finish inbox |> ignore\nHost.tick ()")>]
    [<TestCase("let inbox = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nMailbox.post 1 inbox\nMailbox.post 2 inbox\nMailbox.finish inbox")>]
    [<TestCase("let inbox = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nHost.tick () |> ignore\nMailbox.finish inbox")>]
    [<TestCase("let inbox = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\nHost.tick () |> ignore\nMailbox.post 1 inbox\nMailbox.finish inbox")>]
    [<TestCase("let counter = Host.make ()\nlet inbox = Mailbox.start 1 (fun state message -> Host.read message) 0\nlet alias = inbox\nMailbox.post counter alias\nMailbox.finish inbox |> ignore\nHost.read counter")>]
    member _.AnalysisPreservesConsumerOrdering(source: string) =
        Assert.That(analyzeMailbox TaskSharingAnalysis.defaults source, Is.Empty)

    [<Test>]
    member _.ConditionalFinishDoesNotProveCompletion() =
        let source = "let work wait =\n    let inbox = Mailbox.start 1 (fun state (_: int) -> Host.tick ()) 0\n    Mailbox.post 1 inbox\n    if wait then Mailbox.finish inbox |> ignore else ()\n    Host.tick ()"
        let warnings = analyzeMailbox TaskSharingAnalysis.defaults source
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = "FST1002"), Is.True)

    [<Test>]
    member _.SafeContainerDoesNotProtectHostState() =
        let source = "let counter = Host.make ()\nlet inbox = Mailbox.start 1 (fun state message -> Host.read message) 0\nMailbox.post counter inbox\nHost.read counter |> ignore\nMailbox.finish inbox"
        let options = { TaskSharingAnalysis.defaults with ThreadSafeFunctions = set ["Host.read"] }
        let warnings = analyzeMailbox options source
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = "FST1001"), Is.True)
        Assert.That(warnings |> List.exists (fun warning -> warning.Code = "FST1002"), Is.False)
        let options = { options with ThreadSafeTypes = set ["Counter"] }
        Assert.That(analyzeMailbox options source, Is.Empty)

    [<Test>]
    member _.AnalysisDoesNotInheritConsumerContractsByName() =
        let registration = Registry.registration host
        let functions = registration.Functions |> List.map (fun fn ->
            if fn.Name = "Mailbox.start" then { fn with Impl = fun _ _ -> VUnit } else fn)
        let options = TaskSharingContracts.withRuntime { registration with Functions = functions } TaskSharingAnalysis.defaults
        Assert.That(options.ConsumerOperations.ContainsKey "Mailbox.start", Is.False)
        Assert.That(options.ConsumerOperations.ContainsKey "Mailbox.post", Is.True)

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
