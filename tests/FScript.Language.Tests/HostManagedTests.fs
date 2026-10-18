namespace FScript.Language.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime

module HostManagedFixture =
    let descriptor = { Name = "Node"; Fields = Map.ofList ["Value", TInt]; HostManaged = true }
    let payload = ("host-secret" :> obj)
    let types = { HostInterop.empty with Types = [descriptor] }
    let node value = HostInterop.create types "Node" (Map.ofList ["Value", VInt value]) payload
    let registration =
        { types with
            Functions =
                [ { Name = "Host.make"; Scheme = Forall([], TFun(TInt, TNamed "Node")); Arity = 1
                    Impl = fun _ args -> match args with [VInt n] -> node n | _ -> failwith "invalid" }
                  { Name = "Host.accept"; Scheme = Forall([], TFun(TNamed "Node", TNamed "Node")); Arity = 1
                    Impl = fun _ args ->
                        let host = HostInterop.unwrap "Node" args.Head
                        Assert.That(host.Payload, Is.EqualTo(payload))
                        Assert.That(host.Descriptor.HostManaged, Is.True)
                        args.Head }
                  { Name = "Host.clone"; Scheme = Forall([], TFun(TNamed "Node", TNamed "Node")); Arity = 1
                    Impl = fun _ args ->
                        let host = HostInterop.unwrap "Node" args.Head
                        HostInterop.create types "Node" (host.Fields |> Map.toList |> Map.ofList) host.Payload } ] }
    let infer source = FScript.parse source |> TypeInfer.inferProgramWithRegistration registration
    let run source = infer source |> Eval.evalProgramWithRegistration registration

[<TestFixture>]
type HostManagedTests() =
    [<TestCase("let read x = x.Value\nread (Host.make 42)")>]
    [<TestCase("let read (x: {| Value: int |}) = x.Value\nread (Host.make 42)")>]
    [<TestCase("let node = Host.make 42\nmatch node with | { Value = n } -> n")>]
    [<TestCase("let read x = match x with | { Value = n } -> n\nread (Host.make 42)")>]
    member _.``Visible fields support structural readers and destructuring`` source =
        match HostManagedFixture.run source with
        | VInt 42L -> ()
        | other -> Assert.Fail(Pretty.valueToString other)

    [<TestCase("let id x = x\nid (Host.make 42)")>]
    [<TestCase("let retain x =\n    let n = x.Value\n    x\nretain (Host.make 42)")>]
    [<TestCase("let retain (x: {| Value: int |}) = x\nretain (Host.make 42)")>]
    [<TestCase("match List.tryHead [Host.make 42] with | Some n -> n | None -> Host.make 0")>]
    [<TestCase("let n = Host.make 42\nlet get () = n\nget ()")>]
    [<TestCase("match Some (Host.make 42) with | Some n -> n | None -> Host.make 0")>]
    [<TestCase("let (n, _) = (Host.make 42, 0)\nn")>]
    [<TestCase("type Wrapped = | Wrap of Node\nmatch Wrap (Host.make 42) with | Wrap n -> n")>]
    [<TestCase("match { [\"node\"] = Host.make 42 }[\"node\"] with | Some n -> n | None -> Host.make 0")>]
    [<TestCase("Host.clone (Host.make 42)")>]
    [<TestCase("Host.accept (Host.make 42)")>]
    [<TestCase("let retain x =\n    let v = x.Value\n    x\nHost.accept (retain (Host.make 42))")>]
    [<TestCase("let retain x =\n    let v = x.Value\n    x\nHost.accept (Option.map retain (Some (Host.make 42)) |> Option.defaultValue (Host.make 0))")>]
    member _.``Transport retains payload and metadata`` expression =
        let value = HostManagedFixture.run expression
        let host = HostInterop.unwrap "Node" value
        Assert.That(host.Payload, Is.EqualTo(HostManagedFixture.payload))
        Assert.That(host.Descriptor, Is.EqualTo(HostManagedFixture.descriptor))
        Assert.That(Pretty.valueToString host.Fields.["Value"], Is.EqualTo("42"))

    [<TestCase("(Host.make 1).Secret")>]
    [<TestCase("match Host.make 1 with | { Secret = s } -> s")>]
    [<TestCase("Host.accept {| Value = 1 |}")>]
    [<TestCase("let id x = x\nHost.accept (id {| Value = 1 |})")>]
    [<TestCase("{ Value = 1 }")>]
    [<TestCase("let n = Host.make 1\n{ n with Value = 2 }")>]
    [<TestCase("let n = Host.make 1\n{| n with Value = 2 |}")>]
    [<TestCase("let change (x: {| Value: int |}) = { x with Value = 2 }\nchange (Host.make 1)")>]
    [<TestCase("let change (x: {| Value: int |}) = {| x with Value = 2 |}\nchange (Host.make 1)")>]
    [<TestCase("let retain x =\n    let n = x.Value\n    x\nlet n = retain (Host.make 1)\n{ n with Value = 2 }")>]
    [<TestCase("let retain (x: {| Value: int |}) = x\nlet n = retain (Host.make 1)\n{| n with Value = 2 |}")>]
    [<TestCase("let change x =\n    let v = x.Value\n    { x with Value = 2 }\nchange (Host.make 1)")>]
    [<TestCase("let change (x: {| Value: int |}) = { x with Value = 2 }\nOption.map change (Some (Host.make 1))")>]
    [<TestCase("let reconstruct x = {| Value = x.Value |}\nHost.accept (reconstruct (Host.make 1))")>]
    [<TestCase("let invalid x =\n    let n = x.Value + 1\n    if true then x else {| Value = 1; Extra = x |}\n()")>]
    member _.``Construction updates and hidden access are rejected`` source =
        Assert.Throws<TypeException>(System.Action(fun () -> HostManagedFixture.infer source |> ignore)) |> ignore

    [<Test>]
    member _.``Equality ignores private state recursively and in collection helpers`` () =
        let a = HostManagedFixture.node 1L
        let b = HostInterop.create HostManagedFixture.types "Node" (Map.ofList ["Value", VInt 1L]) ("different secret" :> obj)
        let registration =
            { HostManagedFixture.registration with
                Functions =
                    HostManagedFixture.registration.Functions @
                    [ { Name = "a"; Scheme = Forall([], TNamed "Node"); Arity = 0; Impl = fun _ _ -> a }
                      { Name = "b"; Scheme = Forall([], TNamed "Node"); Arity = 0; Impl = fun _ _ -> b } ] }
        for source in ["a = b"; "[Some a] = [Some b]"; "List.length (List.distinct [a; b]) = 1"; "(Host.make 1 = Host.make 2) = false"] do
            let result = FScript.parse source |> TypeInfer.inferProgramWithRegistration registration |> Eval.evalProgramWithRegistration registration
            Assert.That(Pretty.valueToString result, Is.EqualTo("true"), source)

    [<Test>]
    member _.``Ordinary record literals remain unambiguous and updatable`` () =
        let result = HostManagedFixture.run "type Ordinary = { Value: int }\nlet x = { Value = 1 }\n{ x with Value = 2 }"
        Assert.That(Pretty.valueToString result, Is.EqualTo("{ Value = 2 }"))

    [<Test>]
    member _.``Type registrations cannot conflict or leak across runs`` () =
        let parsed = FScript.parse "Host.make 1"
        let duplicate = { HostManagedFixture.registration with Types = [HostManagedFixture.descriptor; HostManagedFixture.descriptor] }
        Assert.Throws<TypeException>(System.Action(fun () -> TypeInfer.inferProgramWithRegistration duplicate parsed |> ignore)) |> ignore
        Assert.Throws<TypeException>(System.Action(fun () -> HostManagedFixture.infer "type Node = { Value: int }\n()" |> ignore)) |> ignore
        Assert.Throws<TypeException>(System.Action(fun () -> FScript.run "let f (n: Node) = n.Value\n()" |> ignore)) |> ignore

    [<Test>]
    member _.``Interactive submissions retain host metadata and constraints`` () =
        let first = Interactive.createWithRegistration HostManagedFixture.registration |> fun s -> Interactive.submit s (FScript.parse "let n = Host.make 42\nlet retain x =\n    let v = x.Value\n    x")
        let second = Interactive.submit first.Session (FScript.parse "Host.accept (retain n)")
        Assert.That((HostInterop.unwrap "Node" second.State.LastValue).Payload, Is.EqualTo(HostManagedFixture.payload))
        Assert.Throws<TypeException>(System.Action(fun () -> Interactive.submit second.Session (FScript.parse "let x = retain n\n{ x with Value = 0 }") |> ignore)) |> ignore

    [<Test>]
    member _.``Registered unions persist across interactive submissions and remain nominal`` () =
        let registration = { HostInterop.empty with Unions = [{ Name = "Protocol.Mode"; Cases = Map.ofList ["Off", None; "On", Some TInt] }] }
        let first = Interactive.createWithRegistration registration |> fun session -> Interactive.submit session (FScript.parse "let accept (mode: Protocol.Mode) = mode\nlet value = accept (Protocol.Mode.On 42)")
        let second = Interactive.submit first.Session (FScript.parse "match value with | On n -> n | Off -> 0")
        Assert.That(Pretty.valueToString second.State.LastValue, Is.EqualTo("42"))
        Assert.Throws<TypeException>(System.Action(fun () -> Interactive.submit second.Session (FScript.parse "accept 42") |> ignore)) |> ignore
