namespace FScript.Runtime.Tests

open System
open System.Threading.Tasks
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<FScriptHostType("FunctionNode")>]
type FunctionNode = { [<FScriptVisible>] Value: int64; Token: Guid }

[<FScriptHostType("FunctionNode")>]
type OtherFunctionNode = { [<FScriptVisible>] Value: int64; Token: Guid }

[<FScriptHostType("FunctionPoint", ScriptConstructible = true)>]
type FunctionPoint = { [<FScriptVisible>] X: int; [<FScriptVisible>] Label: string }

[<FScriptHostType("FunctionSnapshot")>]
type FunctionSnapshot() =
    let mutable reads = 0
    [<FScriptVisible>]
    member _.Value = reads <- reads + 1; 42L
    member _.Reads = reads

[<TestFixture>]
type HostFunctionsTests() =
    let empty = HostInterop.empty
    let registration descriptors = { HostInterop.empty with Types = descriptors }
    let nodeRegistration () = registration [HostTypes.describe<FunctionNode>()]
    let error action = (Assert.Throws<EvalException>(Action action) |> nonNull).Data0.Message
    let run types functions source =
        ScriptHost.loadSourceWithRegistration { HostInterop.empty with Types = types; Functions = functions } source
    let assertValue expected actual =
        Assert.That(Pretty.valueToString actual, Is.EqualTo(Pretty.valueToString expected))

    [<Test>]
    member _.``Derives closed curried signatures and supports partial application`` () =
        let mutable calls = 0
        let add = HostFunctions.create empty "Host.add" (fun (left: int64) (right: int64) -> calls <- calls + 1; left + right)
        Assert.That(add.Scheme, Is.EqualTo(Forall([], TFun(TInt, TFun(TInt, TInt)))) )
        Assert.That(add.Arity, Is.EqualTo(2))
        let loaded = run [] [add] "let partial = Host.add 2\npartial 40"
        assertValue (VInt 42L) loaded.LastValue
        Assert.That(calls, Is.EqualTo(1))
        Assert.Throws<TypeException>(Action(fun () -> run [] [add] "Host.add 1.0 2.0" |> ignore)) |> ignore
        Assert.That(calls, Is.EqualTo(1))

    [<Test>]
    member _.``Unit inputs and outputs remain explicit script arguments`` () =
        let mutable calls = 0
        let tick = HostFunctions.create empty "Host.tick" (fun () -> calls <- calls + 1)
        Assert.That(tick.Arity, Is.EqualTo(1))
        Assert.That(tick.Scheme, Is.EqualTo(Forall([], TFun(TUnit, TUnit))))
        assertValue VUnit (run [] [tick] "Host.tick ()").LastValue
        Assert.That(calls, Is.EqualTo(1))
        let mixed = HostFunctions.create empty "Host.mixed" (fun () (value: int) -> value)
        assertValue (VInt 42L) (run [] [mixed] "Host.mixed () 42").LastValue

    [<Test>]
    member _.``Scalar handlers infer concrete schemes and capture host context`` () =
        let root = "/host/root"
        let handler = HostFunctions.create empty "Host.scalars" (fun (flag: bool) (ratio: float) (text: string) ->
            (if flag then $"{root}:{text}" else "disabled"), ratio)
        assertValue (VTuple [VString "/host/root:ready"; VFloat 1.5]) (run [] [handler] "Host.scalars true 1.5 \"ready\"").LastValue

    [<Test>]
    member _.``Container and tuple conversions are shared with host types`` () =
        let handler = HostFunctions.create empty "Host.containers"
                          (fun (xs: int list, ys: int64 array, entries: Map<string, int option>, pair: struct (bool * string)) ->
                              xs @ [int ys[0]], [|Some entries["answer"]; None|], pair)
        let input = VTuple [VList [VInt 1L]; VList [VInt 2L]; VMap(Map.ofList [MKString "answer", VOption(Some(VInt 42L))]); VTuple [VBool true; VString "ok"]]
        let result = HostTestHelpers.invoke handler [input]
        assertValue (VTuple [VList [VInt 1L;VInt 2L]; VList [VOption(Some(VOption(Some(VInt 42L)))); VOption None]; VTuple [VBool true; VString "ok"]]) result
        let absent = HostFunctions.create empty "Host.absent" (fun (value: unit option) -> value)
        assertValue (VOption None) (HostTestHelpers.invoke absent [VOption None])
        assertValue (VOption(Some VUnit)) (HostTestHelpers.invoke absent [VOption(Some VUnit)])

    [<Test>]
    member _.``Typed factories and readers preserve hidden host payloads`` () =
        let types = nodeRegistration ()
        let token = Guid.NewGuid()
        let make = HostFunctions.create types "Host.makeNode" (fun (value: int64) -> ({ Value = value; Token = token }: FunctionNode))
        let read = HostFunctions.create types "Host.readNode" (fun (node: FunctionNode) ->
            Assert.That(node.Token, Is.EqualTo(token)); node.Value)
        Assert.That(make.Scheme, Is.EqualTo(Forall([], TFun(TInt, TNamed "FunctionNode"))))
        let loaded = run types.Types [make; read] "export let node = Host.makeNode 42\nHost.readNode node"
        assertValue (VInt 42L) loaded.LastValue
        let node = ScriptHost.getValue loaded "node" |> HostTypes.unwrap<FunctionNode>
        Assert.That(node.Token, Is.EqualTo(token))
        Assert.Throws<TypeException>(Action(fun () -> run types.Types [read] "Host.readNode {| Value = 42 |}" |> ignore)) |> ignore

    [<Test>]
    member _.``Script constructed records are decoded and results encoded as ordinary records`` () =
        let types = registration [HostTypes.describe<FunctionPoint>()]
        let move = HostFunctions.create types "Host.move" (fun (point: FunctionPoint) -> {point with X=point.X+1})
        let loaded = run types.Types [move] "Host.move { X = 41; Label = \"api\" }"
        match loaded.LastValue with VRecord _ -> () | _ -> Assert.Fail("Expected ordinary record")
        let point = HostTypes.unwrap<FunctionPoint> loaded.LastValue
        Assert.That(point.X, Is.EqualTo(42))
        Assert.That(point.Label, Is.EqualTo("api"))

    [<Test>]
    member _.``Opaque values round trip through containers`` () =
        let types = nodeRegistration ()
        let token = Guid.NewGuid()
        let handler = HostFunctions.create types "Host.nested" (fun (nodes: Map<string, FunctionNode option list>) -> nodes)
        let node = HostTypes.wrap types ({Value=42L; Token=token}: FunctionNode)
        let input = VMap(Map.ofList [MKString "nodes", VList [VOption(Some node); VOption None]])
        match HostTestHelpers.invoke handler [input] with
        | VMap fields ->
            match fields[MKString "nodes"] with
            | VList [VOption(Some value); VOption None] ->
                Assert.That((HostTypes.unwrap<FunctionNode> value).Token, Is.EqualTo(token))
                match value with VHostManaged host -> Assert.That(host.Descriptor.HostManaged, Is.True) | _ -> Assert.Fail("Lost metadata")
            | _ -> Assert.Fail("Unexpected nested data")
        | _ -> Assert.Fail("Expected map")

    [<Test>]
    member _.``Signature preparation does not run handlers or visible getters`` () =
        let value = FunctionSnapshot()
        let types = registration [HostTypes.describe<FunctionSnapshot>()]
        let mutable calls = 0
        let handler = HostFunctions.create types "Host.snapshot" (fun () -> calls <- calls + 1; value)
        Assert.That(calls, Is.Zero)
        Assert.That(value.Reads, Is.Zero)
        HostTestHelpers.invoke handler [VUnit] |> ignore
        Assert.That(calls, Is.EqualTo(1))
        Assert.That(value.Reads, Is.EqualTo(1))

    [<Test>]
    member _.``Missing or mismatched host descriptors fail before invocation even for empty containers`` () =
        let mutable calls = 0
        let make () : FunctionNode option list = calls <- calls + 1; []
        let message = error (fun () -> HostFunctions.create empty "Host.nodes" make |> ignore)
        Assert.That(message, Does.Contain("Host.nodes.result"))
        Assert.That(message, Does.Contain("FunctionNode"))
        let descriptor = HostTypes.describe<FunctionNode>()
        let wrong = registration [{descriptor with HostManaged=false}]
        error (fun () -> HostFunctions.create wrong "Host.nodes" make |> ignore) |> ignore
        Assert.That(calls, Is.Zero)

    [<Test>]
    member _.``Distinct CLR types cannot share a script name across a function signature`` () =
        let types = nodeRegistration ()
        let message = error (fun () -> HostFunctions.create types "Host.conflict"
                                          (fun (node: FunctionNode) -> ({Value=node.Value;Token=node.Token}: OtherFunctionNode)) |> ignore)
        Assert.That(message, Does.Contain("Multiple CLR types"))

    [<Test>]
    member _.``Rejects unsupported signatures before handler execution`` () =
        let mutable calls = 0
        let unsupported () = calls <- calls + 1; Guid.NewGuid()
        Assert.That(error (fun () -> HostFunctions.create empty "Host.guid" unsupported |> ignore), Does.Contain("Host.guid.result"))
        error (fun () -> HostFunctions.create empty "Host.task" (fun () -> Task.FromResult 42) |> ignore) |> ignore
        error (fun () -> HostFunctions.create empty "Host.async" (fun () -> async {return 42}) |> ignore) |> ignore
        error (fun () -> HostFunctions.create empty "Host.callback" (fun (callback: int -> int) -> callback 1) |> ignore) |> ignore
        error (fun () -> HostFunctions.create empty "Host.map" (fun (value: Map<int,int>) -> value) |> ignore) |> ignore
        error (fun () -> HostFunctions.create empty "Host.value" 42 |> ignore) |> ignore
        error (fun () -> HostFunctions.create empty "Host.null" (Unchecked.defaultof<int -> int>) |> ignore) |> ignore
        for name in [""; "Host..bad"; "Host bad"; ".bad"] do
            error (fun () -> HostFunctions.create empty name (fun () -> 42) |> ignore) |> ignore
        Assert.That(calls, Is.Zero)

    [<Test>]
    member _.``All arguments are validated before curried handler side effects`` () =
        let mutable calls = 0
        let handler = HostFunctions.create empty "Host.checked" (fun (first: int) -> calls <- calls + 1; fun (second: int) -> first + second)
        for arguments in [[VInt 1L]; [VInt 1L;VInt 2L;VInt 3L]; [VInt 1L;VString "bad"]; [VInt 1L;VInt(Int64.MaxValue)]] do
            error (fun () -> HostTestHelpers.invoke handler arguments |> ignore) |> ignore
        Assert.That(calls, Is.Zero)
        assertValue (VInt 3L) (HostTestHelpers.invoke handler [VInt 1L;VInt 2L])
        Assert.That(calls, Is.EqualTo(1))

    [<Test>]
    member _.``Opaque argument bypasses fail on fields metadata or CLR payload before execution`` () =
        let types = nodeRegistration ()
        let descriptor = types.Types.Head
        let mutable calls = 0
        let handler = HostFunctions.create types "Host.node" (fun (node: FunctionNode) -> calls <- calls + 1; node.Value)
        let original = HostTypes.wrap types ({Value=42L;Token=Guid.NewGuid()}: FunctionNode)
        let host = HostInterop.unwrap descriptor.Name original
        let invalid = [VRecord host.Fields; VHostManaged {host with Descriptor={descriptor with HostManaged=false}};
                       VHostManaged {host with Payload=(box "wrong" |> nonNull)}; VHostManaged {host with Fields=Map.ofList ["Value", VString "wrong"]}]
        for value in invalid do error (fun () -> HostTestHelpers.invoke handler [value] |> ignore) |> ignore
        Assert.That(calls, Is.Zero)

    [<Test>]
    member _.``Errors identify the function and do not leak reflection wrappers`` () =
        let handler = HostFunctions.create empty "Host.explode" (fun () -> (failwith "broken handler": int))
        let message = error (fun () -> HostTestHelpers.invoke handler [VUnit] |> ignore)
        Assert.That(message, Does.Contain("Host.explode"))
        Assert.That(message, Does.Contain("broken handler"))
        Assert.That(message, Does.Not.Contain("TargetInvocationException"))
        let nullResult = HostFunctions.create empty "Host.nullResult" (fun () -> Unchecked.defaultof<string>)
        Assert.That(error (fun () -> HostTestHelpers.invoke nullResult [VUnit] |> ignore), Does.Contain("Host.nullResult.result"))
