namespace FScript.Runtime.Tests

open System
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type RefTests() =
    let registration = Registry.registration { HostContext.create "." with Permissions = HostPermissions.None }
    let load source = ScriptHost.loadSourceWithRegistration registration source

    [<TestCase("let cell = Ref.create 1\nlet alias = cell\nlet before = Ref.get cell\nlet result = alias |> Ref.set 42\n(before, Ref.get cell, result)", "(1, 42, ())")>]
    [<TestCase("let first = Ref.create 0\nlet second = Ref.create 0\nRef.set 1 first\n(Ref.get first, Ref.get second, first = first, first = second)", "(1, 0, true, false)")>]
    [<TestCase("type State = { Value: Ref<int list> }\nlet state = { Value = Ref.create [] }\nRef.set [1; 2] state.Value\nRef.get state.Value", "[1;2]")>]
    [<TestCase("let make value = Ref.create value\nlet numbers = make [1]\nlet words = make [\"two\"]\n(Ref.get numbers, Ref.get words)", "([1], [\"two\"])")>]
    [<TestCase("let cell = Ref.create (fun n -> n + 1)\nRef.set (fun n -> n * 2) cell\n(Ref.get cell) 21", "42")>]
    [<TestCase("let cell = Ref.create None\nRef.set (Some (Ref.create \"ok\")) cell\nRef.get (Ref.get cell |> Option.defaultValue (Ref.create \"fallback\"))", "\"ok\"")>]
    member _.``Refs store values and preserve aliases without permissions``(source: string, expected: string) =
        Assert.That((load source).LastValue |> Pretty.valueToString, Is.EqualTo(expected))

    [<TestCase("let cell = Ref.create []\nRef.set [1] cell\nRef.set [\"two\"] cell")>]
    [<TestCase("let cell = Ref.create []\nlet alias = cell\nRef.set [1] cell\nRef.set [\"two\"] alias")>]
    [<TestCase("let make () =\n    let cell = Ref.create []\n    ((fun value -> Ref.set value cell), (fun () -> Ref.get cell))\nlet (write, read) = make ()\nwrite [1]\nlet words (values: string list) = values\nwords (read ())")>]
    [<TestCase("let cell = Ref.create 1\ncell.Value")>]
    [<TestCase("Ref.get { Value = 1 }")>]
    member _.``Refs keep their type and hide storage``(source: string) =
        Assert.Throws<TypeException>(Action(fun () -> load source |> ignore)) |> ignore

    [<Test>]
    member _.``Refs require runtime registration``() =
        Assert.Throws<TypeException>(Action(fun () ->
            ScriptHost.loadSourceWithRegistration HostInterop.empty "Ref.create 1" |> ignore)) |> ignore

    [<Test>]
    member _.``Compiled cells share storage and return unit from set``() =
        let cell = CompiledRef.Create [1L]
        let alias = cell
        let result = CompiledRef.Set([42L], alias)
        Assert.That(result, Is.EqualTo(()))
        Assert.That(CompiledRef.Get cell, Is.EqualTo([42L]))
