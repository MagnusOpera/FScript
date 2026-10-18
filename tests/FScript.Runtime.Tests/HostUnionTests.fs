namespace FScript.Runtime.Tests

open System
open NUnit.Framework
open FScript.Language
open FScript.Runtime
open FScript.Extensibility

module ScriptHost = FScript.Testing.SdkTestHost
module Pretty = FScript.Testing.SdkTestPretty

[<FScriptHostType("Foreign.Mode", ScriptConstructible = true)>]
type ForeignMode = Idle | Message of string | Pair of int64 * bool | Labels of Set<string>

[<FScriptHostType("Foreign.Settings", ScriptConstructible = true)>]
type ForeignSettings =
    { [<FScriptVisible>] Mode: ForeignMode
      [<FScriptVisible>] Names: Set<string> }

[<FScriptHostType("Foreign.Recursive", ScriptConstructible = true)>]
type RecursiveForeign = Next of RecursiveForeign

[<FScriptHostType("Foreign.OpaqueUnion")>]
type OpaqueUnion = OpaqueCase

[<TestFixture>]
type HostUnionTests() =
    let types =
        { HostInterop.empty with Types = [HostTypes.describe<ForeignSettings>()]
                                 Unions = [HostTypes.describeUnion<ForeignMode>()] }
    let registration =
        { types with Functions = [HostFunctions.create types "Host.echo" (fun (value: ForeignSettings) -> value)
                                  HostFunctions.create types "Host.mode" (fun (value: ForeignMode) -> value)
                                  HostFunctions.create types "Host.combine" (fun (mode: ForeignMode) (names: Set<string>) -> { Mode = mode; Names = names })
                                  HostFunctions.create types "Host.modes" (fun (values: ForeignMode option list) -> List.rev values)] }
    let run source = (ScriptHost.loadSourceWithRegistration registration source).LastValue

    [<Test>]
    member _.``union descriptor includes nullary and tuple payloads``() =
        let descriptor = HostTypes.describeUnion<ForeignMode>()
        Assert.That(descriptor.Name, Is.EqualTo "Foreign.Mode")
        Assert.That(descriptor.Cases["Idle"], Is.EqualTo None)
        Assert.That(descriptor.Cases["Pair"], Is.EqualTo(Some(TTuple [TInt; TBool])))
        Assert.That(descriptor.Cases["Labels"], Is.EqualTo(Some(TList TString)))

    [<TestCase("Idle")>]
    [<TestCase("Message \"hello\"")>]
    [<TestCase("Pair (42, true)")>]
    [<TestCase("Foreign.Mode.Message \"qualified\"")>]
    member _.``scripts construct and round trip host union cases`` expression =
        let value = run ("Host.mode (" + expression + ")")
        let native = HostTypes.unwrap<ForeignMode> value
        Assert.That(Pretty.valueToString (HostTypes.wrap types native), Is.EqualTo(Pretty.valueToString value))

    [<Test>]
    member _.``union matching records options sets and partial application share typed conversion``() =
        let value = run """
let echo = Host.echo
let result = echo { Mode = Labels ["z"; "a"; "z"]; Names = ["second"; "first"; "second"] }
match result.Mode with
| Labels names -> List.length names + List.length result.Names
| _ -> 0
"""
        match value with VInt 4L -> () | other -> Assert.Fail($"Expected 4, got {other}")
        let record = run "let create = Host.combine (Pair (42, true))\ncreate [\"x\"; \"x\"]" |> HostTypes.unwrap<ForeignSettings>
        Assert.That(record.Mode, Is.EqualTo(ForeignMode.Pair(42L, true)))
        Assert.That(record.Names, Is.EqualTo(box (set ["x"])))
        match run "Host.modes [Some (Message \"nested\"); None]" with
        | VList [VOption None; VOption(Some(VUnionCase("Foreign.Mode", "Message", Some(VString "nested"))))] -> ()
        | other -> Assert.Fail($"Unexpected nested union result: {other}")

    [<Test>]
    member _.``host unions reject wrong payloads and union identities``() =
        Assert.That(Action(fun () -> run "Host.mode (Message 42)" |> ignore), Throws.TypeOf<TypeException>())
        Assert.That(Action(fun () -> HostTypes.unwrap<ForeignMode> (VUnionCase("Other", "Idle", None)) |> ignore), Throws.TypeOf<ExtensionException>())
        Assert.That(Action(fun () -> HostTypes.unwrap<ForeignMode> (VUnionCase("Foreign.Mode", "Pair", Some(VInt 42L))) |> ignore), Throws.TypeOf<ExtensionException>())

    [<Test>]
    member _.``registration checks cover unions records and script declarations``() =
        let duplicate = { types with Unions = types.Unions @ types.Unions }
        Assert.That(Action(fun () -> HostInterop.typeDefinitions duplicate [] |> ignore), Throws.TypeOf<ExtensionException>())
        let conflict = { types with Types = [{ Name = "Foreign.Mode"; Fields = Map.empty; HostManaged = false }] }
        Assert.That(Action(fun () -> HostInterop.typeDefinitions conflict [] |> ignore), Throws.TypeOf<ExtensionException>())
        Assert.That(Action(fun () -> HostFunctions.create HostInterop.empty "Host.mode" (fun (mode: ForeignMode) -> mode) |> ignore), Throws.TypeOf<ExtensionException>())

    [<Test>]
    member _.``recursive and opaque union projections fail early``() =
        Assert.That(Action(fun () -> HostTypes.describeUnion<RecursiveForeign>() |> ignore), Throws.TypeOf<ExtensionException>())
        Assert.That(Action(fun () -> HostTypes.describeUnion<OpaqueUnion>() |> ignore), Throws.TypeOf<ExtensionException>())
