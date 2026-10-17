namespace FScript.Runtime.Tests

open System
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<FScriptHostType("Node")>]
type ReflectedNode =
    { [<FScriptVisible>] Value: int
      Token: Guid }

[<Struct; FScriptHostType("Tiny")>]
type ReflectedStruct = { [<FScriptVisible>] Count: int64 }

[<FScriptHostType("Snapshot")>]
type ReflectedSnapshot() =
    let numbers = [| 1; 2 |]
    let mutable reads = 0
    [<FScriptVisible>]
    member _.Numbers = reads <- reads + 1; numbers
    [<FScriptVisible>]
    member _.Data = (Some [3L; 4L], true, 1.5, ())
    [<FScriptVisible>]
    member _.StructData = struct (9L, Some ())
    [<FScriptVisible>]
    member _.Missing: int option = None
    [<FScriptVisible>]
    member _.Lookup = Map.ofList [ ("first", Some "text"); ("missing", None) ]
    member _.Hidden: Guid = failwith "Hidden getters must not run"
    member _.Reads = reads
    member _.Change() = numbers[0] <- 99

[<FScriptHostType("Parent")>]
type ReflectedParent = { [<FScriptVisible>] Child: ReflectedNode option list }

[<FScriptHostType("Bad")>]
type UnsupportedVisible = { [<FScriptVisible>] Id: Guid }

[<FScriptHostType("Cycle")>]
type RecursiveVisible = { [<FScriptVisible>] Next: RecursiveVisible option }

[<FScriptHostType("Throwing")>]
type ThrowingVisible() =
    [<FScriptVisible>]
    member _.Value: int = failwith "getter exploded"

[<FScriptHostType("Static")>]
type StaticVisible() =
    [<FScriptVisible>]
    static member Value = 1

[<FScriptHostType("Indexed")>]
type IndexedVisible() =
    [<FScriptVisible>]
    member _.Item with get (index: int) = index

[<FScriptHostType("Private")>]
type PrivateVisible() =
    [<FScriptVisible>]
    member private _.Value = 1

[<FScriptHostType("NullText")>]
type NullVisible() =
    [<FScriptVisible>]
    member _.Text = Unchecked.defaultof<string>

[<FScriptHostType("Node")>]
type OtherReflectedNode = { [<FScriptVisible>] Value: int }

[<FScriptHostType("Collision")>]
type CollidingParent =
    { [<FScriptVisible>] First: ReflectedNode
      [<FScriptVisible>] Second: OtherReflectedNode }

[<FScriptHostType(" ")>]
type InvalidHostName = { [<FScriptVisible>] Number: int }

[<TestFixture>]
type HostTypesTests() =
    let registration descriptors = { HostInterop.empty with Types = descriptors }
    let nodeRegistration () = registration [HostTypes.describe<ReflectedNode>()]
    let node () : ReflectedNode = { Value = 42; Token = Guid.NewGuid() }
    let assertValue actual expected = Assert.That(Pretty.valueToString actual, Is.EqualTo(Pretty.valueToString expected))
    let error action = (Assert.Throws<EvalException>(Action(action)) |> nonNull).Data0.Message

    [<Test>]
    member _.``Descriptors opt in fields and map CLR int to script int`` () =
        let descriptor = HostTypes.describe<ReflectedNode>()
        Assert.That(descriptor.Name, Is.EqualTo("Node"))
        Assert.That(descriptor.HostManaged, Is.True)
        Assert.That(descriptor.Fields, Is.EqualTo(box (Map.ofList [ ("Value", TInt) ])))
        Assert.That(descriptor.Fields.ContainsKey "Token", Is.False)

    [<Test>]
    member _.``Reflection snapshots collections and invokes visible getters only when wrapping`` () =
        let source = ReflectedSnapshot()
        let descriptor = HostTypes.describe<ReflectedSnapshot>()
        Assert.That(source.Reads, Is.Zero)
        let wrapped = HostTypes.wrap (registration [descriptor]) source
        Assert.That(source.Reads, Is.EqualTo(1))
        source.Change()
        let host = HostInterop.unwrap "Snapshot" wrapped
        assertValue (host.Fields["Numbers"]) (VList [VInt 1L; VInt 2L])
        assertValue (host.Fields["Data"]) (VTuple [VOption(Some(VList [VInt 3L; VInt 4L])); VBool true; VFloat 1.5; VUnit])
        assertValue (host.Fields["StructData"]) (VTuple [VInt 9L; VOption(Some VUnit)])
        assertValue (host.Fields["Missing"]) (VOption None)
        assertValue (host.Fields["Lookup"]) (VMap(Map.ofList [ (MKString "first", VOption(Some(VString "text"))); (MKString "missing", VOption None) ]))
        Assert.That((HostTypes.unwrap<ReflectedSnapshot> wrapped).Reads, Is.EqualTo(1))
        let fresh = HostTypes.wrap (registration [descriptor]) source
        assertValue ((HostInterop.unwrap "Snapshot" fresh).Fields["Numbers"]) (VList [VInt 99L; VInt 2L])
        assertValue (host.Fields["Numbers"]) (VList [VInt 1L; VInt 2L])

    [<Test>]
    member _.``Wrapped payloads survive exports structural readers and containers`` () =
        let registered = nodeRegistration ()
        let source = node ()
        let wrapped = HostTypes.wrap registered source
        let loaded = ScriptHost.loadSourceWithRegistration registered "export let keep x =\n    let visible = x.Value\n    [Some x]\nexport let accept (node: Node) = node.Value\n"
        match ScriptHost.invoke loaded "keep" [wrapped] with
        | VList [VOption(Some retained)] ->
            Assert.That((HostTypes.unwrap<ReflectedNode> retained).Token, Is.EqualTo(source.Token))
            Assert.That((HostInterop.unwrap "Node" retained).Descriptor, Is.EqualTo(HostTypes.describe<ReflectedNode>()))
        | _ -> Assert.Fail("Expected retained host value")
        Assert.That(error (fun () -> ScriptHost.invoke loaded "accept" [VRecord(Map.ofList [ ("Value", VInt 1L) ])] |> ignore), Does.Contain("match"))

    [<Test>]
    member _.``Nested annotated values require matching registrations even when absent`` () =
        let parentDescriptor = HostTypes.describe<ReflectedParent>()
        let registered = registration [parentDescriptor; HostTypes.describe<ReflectedNode>()]
        let source: ReflectedParent = { Child = [Some(node()); None] }
        let wrapped = HostTypes.wrap registered source
        match (HostInterop.unwrap "Parent" wrapped).Fields["Child"] with
        | VList [VOption(Some child); VOption None] -> Assert.That((HostTypes.unwrap<ReflectedNode> child).Value, Is.EqualTo(42))
        | _ -> Assert.Fail("Nested host value metadata was lost")
        let absent: ReflectedParent = { Child = [None] }
        Assert.That(error (fun () -> HostTypes.wrap (registration [parentDescriptor]) absent |> ignore), Does.Contain("Node"))
        Assert.That((HostTypes.unwrap<ReflectedParent> wrapped).Child.Length, Is.EqualTo(2))
        let host = HostInterop.unwrap "Parent" wrapped
        let child = HostInterop.unwrap "Node" (HostTypes.wrap registered (node ()))
        let forgedChild = VHostManaged { child with
                                            Descriptor = { child.Descriptor with Fields = Map.ofList [("Value", TString)] }
                                            Fields = Map.ofList [("Value", VString "wrong")] }
        let forged = VHostManaged { host with Fields = Map.ofList [("Child", VList [VOption(Some forgedChild)])] }
        Assert.That(error (fun () -> HostTypes.unwrap<ReflectedParent> forged |> ignore), Does.Contain("match"))

    [<Test>]
    member _.``Typed retrieval rejects forged metadata fields and payloads`` () =
        let registered = nodeRegistration ()
        let value = HostTypes.wrap registered (node ())
        let host = HostInterop.unwrap "Node" value
        let forgedPayload = VHostManaged { host with Payload = ("not a Node" :> obj) }
        Assert.That(error (fun () -> HostTypes.unwrap<ReflectedNode> forgedPayload |> ignore), Does.Contain("CLR type"))
        let forgedFields = VHostManaged { host with Fields = Map.ofList [ ("Value", VString "wrong") ] }
        Assert.That(error (fun () -> HostTypes.unwrap<ReflectedNode> forgedFields |> ignore), Does.Contain("match"))
        let forgedDescriptor = VHostManaged { host with Descriptor = { host.Descriptor with Fields = Map.empty } }
        Assert.That(error (fun () -> HostTypes.unwrap<ReflectedNode> forgedDescriptor |> ignore), Does.Contain("descriptor"))
        Assert.That(error (fun () -> HostTypes.unwrap<OtherReflectedNode> value |> ignore), Does.Contain("CLR type"))
        Assert.That(error (fun () -> HostTypes.unwrap<ReflectedStruct> value |> ignore), Does.Contain("Tiny"))

    [<Test>]
    member _.``Missing mismatched and duplicate registrations cannot wrap values`` () =
        let descriptor = HostTypes.describe<ReflectedNode>()
        for descriptors in [ []; [{descriptor with HostManaged = false}]; [{descriptor with Fields = Map.empty}]; [descriptor; descriptor] ] do
            Assert.That(error (fun () -> HostTypes.wrap (registration descriptors) (node ()) |> ignore), Is.Not.Empty)

    [<Test>]
    member _.``Struct records retain typed payloads`` () =
        let descriptor = HostTypes.describe<ReflectedStruct>()
        let wrapped = HostTypes.wrap (registration [descriptor]) { ReflectedStruct.Count = 5L }
        Assert.That((HostTypes.unwrap<ReflectedStruct> wrapped).Count, Is.EqualTo(5L))

    [<Test>]
    member _.``Unsupported visible members and recursive projections fail with paths`` () =
        Assert.That(error (fun () -> HostTypes.describe<UnsupportedVisible>() |> ignore), Does.Contain("UnsupportedVisible.Id").And.Contain("System.Guid"))
        Assert.That(error (fun () -> HostTypes.describe<RecursiveVisible>() |> ignore), Does.Contain("Next").And.Contain("Recursive"))
        Assert.That(error (fun () -> HostTypes.describe<StaticVisible>() |> ignore), Does.Contain("Value").And.Contain("instance getter"))
        Assert.That(error (fun () -> HostTypes.describe<IndexedVisible>() |> ignore), Does.Contain("Item").And.Contain("indexers"))
        Assert.That(error (fun () -> HostTypes.describe<PrivateVisible>() |> ignore), Does.Contain("Value").And.Contain("public instance"))
        Assert.That(error (fun () -> HostTypes.describe<int>() |> ignore), Does.Contain("FScriptHostType"))
        Assert.That(error (fun () -> HostTypes.describe<InvalidHostName>() |> ignore), Does.Contain("identifier"))
        Assert.That(error (fun () -> HostTypes.describe<CollidingParent>() |> ignore), Does.Contain("Multiple CLR types"))

    [<Test>]
    member _.``Getter exceptions and null fields cannot produce partial host values`` () =
        let descriptor = HostTypes.describe<ThrowingVisible>()
        Assert.That(error (fun () -> HostTypes.wrap (registration [descriptor]) (ThrowingVisible()) |> ignore), Does.Contain("ThrowingVisible.Value").And.Contain("getter exploded"))
        let nullDescriptor = HostTypes.describe<NullVisible>()
        Assert.That(error (fun () -> HostTypes.wrap (registration [nullDescriptor]) (NullVisible()) |> ignore), Does.Contain("NullVisible.Text").And.Contain("Null"))
        Assert.That(error (fun () -> HostTypes.wrap (nodeRegistration ()) Unchecked.defaultof<ReflectedNode> |> ignore), Does.Contain("null host object"))

[<FScriptHostType("Point", ScriptConstructible = true)>]
type ConstructiblePoint =
    { [<FScriptVisible>] X: int
      [<FScriptVisible>] Label: string }

[<FScriptHostType("Points", ScriptConstructible = true)>]
type ConstructiblePoints =
    { [<FScriptVisible>] Items: ConstructiblePoint option list
      [<FScriptVisible>] Lookup: Map<string, ConstructiblePoint>
      [<FScriptVisible>] Tuple: struct (int64 * bool * unit)
      [<FScriptVisible>] Numbers: int64 array }

[<FScriptHostType("InvalidConstructible", ScriptConstructible = true)>]
type ConstructibleHidden =
    { [<FScriptVisible>] Value: int64
      Secret: string }

[<FScriptHostType("Mixed", ScriptConstructible = true)>]
type ConstructibleWithOpaque =
    { [<FScriptVisible>] Node: ReflectedNode }

[<FScriptHostType("PlainClass", ScriptConstructible = true)>]
type ConstructibleClass() =
    [<FScriptVisible>]
    member _.Value = 1

[<TestFixture>]
type ConstructibleHostTypesTests() =
    let registered descriptors = { HostInterop.empty with Types = descriptors }
    let error action = (Assert.Throws<EvalException>(Action(action)) |> nonNull).Data0.Message
    let pointRegistration () = registered [HostTypes.describe<ConstructiblePoint>()]

    [<Test>]
    member _.``Scripts construct update and pass module provided records to CLR code`` () =
        let baseRegistration = pointRegistration ()
        Assert.That(baseRegistration.Types.Head.HostManaged, Is.False)
        let mutable received: ConstructiblePoint option = None
        let accept =
            { Name = "Host.acceptPoint"; Scheme = Forall([], TFun(TNamed "Point", TInt)); Arity = 1
              Impl = fun _ args ->
                  let point = HostTypes.unwrap<ConstructiblePoint> args.Head
                  received <- Some point
                  VInt(int64 point.X) }
        let registration = { baseRegistration with Functions = [accept] }
        let source = "let original = { X = 1; Label = \"first\" }\nlet updated = { original with X = 42 }\nHost.acceptPoint updated"
        let loaded = ScriptHost.loadSourceWithRegistration registration source
        Assert.That(Pretty.valueToString loaded.LastValue, Is.EqualTo("42"))
        Assert.That(received.Value.X, Is.EqualTo(42))
        Assert.That(received.Value.Label, Is.EqualTo("first"))
        let structuralUpdate = ScriptHost.loadSourceWithRegistration registration "let original = { X = 1; Label = \"first\" }\nlet change (p: Point) = {| p with X = 9 |}\nHost.acceptPoint (change original)"
        Assert.That(Pretty.valueToString structuralUpdate.LastValue, Is.EqualTo("9"))
        let scriptValue = HostTypes.wrap registration received.Value
        match scriptValue with VRecord _ -> () | _ -> Assert.Fail("Constructible values must use ordinary records")
        Assert.That((HostTypes.unwrap<ConstructiblePoint> scriptValue).X, Is.EqualTo(9))

    [<Test>]
    member _.``Nested record and container conversions reconstruct CLR data`` () =
        let registration = registered [HostTypes.describe<ConstructiblePoints>(); HostTypes.describe<ConstructiblePoint>()]
        let source =
            "let point = { X = 7; Label = \"nested\" }\nlet points = { Items = [Some point; None]; Lookup = { [\"key\"] = point }; Tuple = (8, true, ()); Numbers = [1; 2] }\npoints"
        let value = (ScriptHost.loadSourceWithRegistration registration source).LastValue
        let result = HostTypes.unwrap<ConstructiblePoints> value
        Assert.That(result.Items.Length, Is.EqualTo(2))
        Assert.That(result.Items.Head.Value.X, Is.EqualTo(7))
        Assert.That(result.Items[1].IsNone, Is.True)
        Assert.That(result.Lookup["key"].Label, Is.EqualTo("nested"))
        Assert.That(result.Tuple, Is.EqualTo(struct (8L, true, ())))
        Assert.That(result.Numbers, Is.EqualTo(box [|1L; 2L|]))
        let wrapped = HostTypes.wrap registration result
        Assert.That(Pretty.valueToString wrapped, Is.EqualTo(Pretty.valueToString value))

    [<Test>]
    member _.``Declaration rejects constructible records with hidden fields`` () =
        Assert.That(error (fun () -> HostTypes.describe<ConstructibleHidden>() |> ignore), Does.Contain("every constructor field").And.Contain("Secret"))
        Assert.That(error (fun () -> HostTypes.describe<ConstructibleClass>() |> ignore), Does.Contain("F# record"))

    [<Test>]
    member _.``Conversion rejects missing extra wrong typed fields and integer overflow`` () =
        for value in [ VRecord Map.empty
                       VRecord(Map.ofList [("X", VInt 1L); ("Label", VString "ok"); ("Extra", VUnit)])
                       VRecord(Map.ofList [("X", VString "bad"); ("Label", VString "ok")]) ] do
            Assert.That(error (fun () -> HostTypes.unwrap<ConstructiblePoint> value |> ignore), Is.Not.Empty)
        let oversized = VRecord(Map.ofList [("X", VInt Int64.MaxValue); ("Label", VString "ok")])
        Assert.That(error (fun () -> HostTypes.unwrap<ConstructiblePoint> oversized |> ignore), Does.Contain("Int32 range"))

    [<Test>]
    member _.``Constructible records preserve nested opaque values without manufacturing them`` () =
        let baseRegistration = registered [HostTypes.describe<ConstructibleWithOpaque>(); HostTypes.describe<ReflectedNode>()]
        let source: ReflectedNode = { Value = 5; Token = Guid.NewGuid() }
        let makeNode =
            { Name = "Host.node"; Scheme = Forall([], TNamed "Node"); Arity = 0
              Impl = fun _ _ -> HostTypes.wrap baseRegistration source }
        let registration = { baseRegistration with Functions = [makeNode] }
        let value = (ScriptHost.loadSourceWithRegistration registration "let wrapped = { Node = Host.node }\nwrapped").LastValue
        Assert.That((HostTypes.unwrap<ConstructibleWithOpaque> value).Node.Token, Is.EqualTo(source.Token))
        Assert.Throws<TypeException>(Action(fun () -> ScriptHost.loadSourceWithRegistration registration "let wrapped = { Node = { Value = 1 } }\nwrapped" |> ignore)) |> ignore
