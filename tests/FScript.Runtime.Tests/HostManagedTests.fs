namespace FScript.Runtime.Tests

open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type HostManagedTests() =
    let descriptor = { Name = "Node"; Fields = Map.ofList ["Value", TInt]; HostManaged = true }
    let payload = ("private-native-handle" :> obj)
    let types = { HostInterop.empty with Types = [descriptor] }
    let node = HostInterop.create types "Node" (Map.ofList ["Value", VInt 42L]) payload
    let registration =
        { types with
            Functions =
                Registry.all { RootDirectory = System.IO.Directory.GetCurrentDirectory(); DeniedPathGlobs = [] } @
                [ { Name = "Host.node"; Scheme = Forall([], TNamed "Node"); Arity = 0; Impl = fun _ _ -> node } ] }
    let load source = ScriptHost.loadSourceWithRegistration registration source

    [<Test>]
    member _.``Task and exported function calls preserve opaque values`` () =
        let loaded = load "export let pass (n: Node) = Task.await (Task.spawn (fun () -> n))"
        let result = ScriptHost.invoke loaded "pass" [node]
        Assert.That((HostInterop.unwrap "Node" result).Payload, Is.EqualTo(payload))
        Assert.Throws<EvalException>(System.Action(fun () -> ScriptHost.invoke loaded "pass" [VRecord (Map.ofList ["Value", VInt 42L])] |> ignore)) |> ignore

    [<Test>]
    member _.``JSON and script formatting reveal only visible fields`` () =
        let result = load "Json.serialize [Host.node]"
        match result.LastValue with
        | VOption (Some (VString json)) -> Assert.That(json, Is.EqualTo("[{\"Value\":42}]"))
        | _ -> Assert.Fail("Expected serialized visible fields")
        Assert.That(Pretty.valueToString node, Is.EqualTo("{ Value = 42 }"))
        let interpolated = load "$\"node={Host.node}\""
        Assert.That(Pretty.valueToString interpolated.LastValue, Is.EqualTo("\"node={ Value = 42 }\""))
        let ordinary = load "type Plain = { Value: int }\nJson.deserialize (typeof Plain) \"{\\\"Value\\\":42}\""
        Assert.That(Pretty.valueToString ordinary.LastValue, Is.EqualTo("Some { Value = 42 }"))
        let decoded = load "Json.deserialize (typeof Node) \"{\\\"Value\\\":42}\""
        Assert.That(Pretty.valueToString decoded.LastValue, Is.EqualTo("None"))
        let nested = load "type Box = { Item: Node }\nJson.deserialize (typeof Box) \"{\\\"Item\\\":{\\\"Value\\\":42}}\""
        Assert.That(Pretty.valueToString nested.LastValue, Is.EqualTo("None"))

    [<Test>]
    member _.``Host creation validates visible field contracts`` () =
        for fields in [Map.empty; Map.ofList ["Value", VString "wrong"]; Map.ofList ["Value", VInt 1L; "Hidden", VUnit]] do
            Assert.Throws<EvalException>(System.Action(fun () -> HostInterop.create types "Node" fields payload |> ignore)) |> ignore
        Assert.Throws<EvalException>(System.Action(fun () -> HostInterop.create types "Missing" Map.empty payload |> ignore)) |> ignore

    [<Test>]
    member _.``Extern returns cannot forge host types even without inference`` () =
        for returned in [VRecord(Map.ofList ["Value", VInt 1L]); VHostManaged { Descriptor = { descriptor with Name = "Other" }; Fields = Map.ofList ["Value", VInt 1L]; Payload = payload }] do
            let bad = { Name = "Host.bad"; Scheme = Forall([], TNamed "Node"); Arity = 0; Impl = fun _ _ -> returned }
            let registration = { types with Functions = [bad] }
            Assert.Throws<EvalException>(System.Action(fun () -> Eval.evalProgramWithRegistration registration [] |> ignore)) |> ignore

    [<Test>]
    member _.``Runtime updates reject host values through structural annotations`` () =
        for expression in ["{ n with Value = 0 }"; "{| n with Value = 0 |}"] do
            let ordinaryRegistration = HostInterop.ofExterns []
            let program = FScript.parse ($"export let update (n: {{| Value: int |}}) = {expression}")
            let typed = TypeInfer.inferProgramWithRegistration ordinaryRegistration program
            let state = Eval.evalProgramWithRegistrationState ordinaryRegistration typed
            Assert.Throws<EvalException>(System.Action(fun () -> Eval.invokeValue state.TypeDefs state.Env.["update"] [node] |> ignore)) |> ignore

    [<Test>]
    member _.``Non-managed registered types retain ordinary record semantics`` () =
        let registration = { HostInterop.empty with Types = [{ descriptor with HostManaged = false }] }
        let loaded = ScriptHost.loadSourceWithRegistration registration "let n = { Value = 1 }\n{ n with Value = 2 }"
        Assert.That(Pretty.valueToString loaded.LastValue, Is.EqualTo("{ Value = 2 }"))

    [<Test>]
    member _.``Nested host values are validated and preserve their private state`` () =
        let parent = { Name = "Parent"; Fields = Map.ofList ["Child", TNamed "Node"]; HostManaged = true }
        let registered = { registration with Types = [descriptor; parent] }
        let value = HostInterop.create registered "Parent" (Map.ofList ["Child", node]) ("parent-secret" :> obj)
        let registered =
            { registered with
                Functions =
                    registered.Functions @
                    [ { Name = "Host.parent"; Scheme = Forall([], TNamed "Parent"); Arity = 0; Impl = fun _ _ -> value } ] }
        let loaded = ScriptHost.loadSourceWithRegistration registered "match Host.parent with | { Child = child } -> child"
        Assert.That((HostInterop.unwrap "Node" loaded.LastValue).Payload, Is.EqualTo(payload))
        let nestedPattern = ScriptHost.loadSourceWithRegistration registered "match Host.parent with | { Child = { Value = n } } -> n"
        Assert.That(Pretty.valueToString nestedPattern.LastValue, Is.EqualTo("42"))
        Assert.Throws<EvalException>(System.Action(fun () -> HostInterop.create registered "Parent" (Map.ofList ["Child", VRecord(Map.ofList ["Value", VInt 42L])]) payload |> ignore)) |> ignore

    [<Test>]
    member _.``Extern arguments cannot substitute another host type or a plain nested record`` () =
        let definitions = HostInterop.typeDefinitions registration []
        let mutable called = false
        let consume =
            { Name = "Host.consume"; Scheme = Forall([], TFun(TOption(TNamed "Node"), TUnit)); Arity = 1
              Impl = fun _ _ -> called <- true; VUnit }
        let fn = VExternal(consume, [])
        let other = VHostManaged { Descriptor = { descriptor with Name = "Other" }; Fields = Map.ofList ["Value", VInt 42L]; Payload = payload }
        for argument in [VOption(Some other); VOption(Some(VRecord(Map.ofList ["Value", VInt 42L])))] do
            Assert.Throws<EvalException>(System.Action(fun () -> Eval.invokeValue definitions fn [argument] |> ignore)) |> ignore
        Assert.That(called, Is.False)

    [<Test>]
    member _.``Host clone can change visible snapshot without changing its source`` () =
        let original = HostInterop.unwrap "Node" node
        let clone = HostInterop.create registration "Node" (original.Fields.Add("Value", VInt 99L)) original.Payload
        Assert.That(Pretty.valueToString node, Is.EqualTo("{ Value = 42 }"))
        Assert.That(Pretty.valueToString clone, Is.EqualTo("{ Value = 99 }"))
        Assert.That((HostInterop.unwrap "Node" clone).Descriptor.HostManaged, Is.True)
        Assert.That((HostInterop.unwrap "Node" clone).Payload, Is.EqualTo(payload))

    [<Test>]
    member _.``Polymorphic externs cannot erase host metadata in a round trip`` () =
        let bad =
            { Name = "Host.erase"; Scheme = Forall([0], TFun(TVar 0, TVar 0)); Arity = 1
              Impl = fun _ _ -> VRecord(Map.ofList ["Value", VInt 42L]) }
        let registered = { registration with Functions = registration.Functions @ [bad] }
        Assert.Throws<EvalException>(System.Action(fun () -> ScriptHost.loadSourceWithRegistration registered "Host.erase Host.node" |> ignore)) |> ignore
        let badNested =
            { bad with Impl = fun _ _ -> VList [VOption None; VOption(Some(VRecord(Map.ofList ["Value", VInt 42L])))] }
        let registered = { registration with Functions = registration.Functions @ [badNested] }
        Assert.Throws<EvalException>(System.Action(fun () -> ScriptHost.loadSourceWithRegistration registered "Host.erase [None; Some (Host.node)]" |> ignore)) |> ignore
