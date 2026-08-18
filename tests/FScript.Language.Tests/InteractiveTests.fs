namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language

[<TestFixture>]
type InteractiveTests () =
    let submit session source =
        source
        |> Helpers.parse
        |> Interactive.submit session

    [<Test>]
    member _.``Retained polymorphic bindings are available to later submissions`` () =
        let first = submit (Interactive.create []) "let identity value = value"
        let second = submit first.Session "(identity 42, identity \"ok\")"

        match second.State.LastValue with
        | VTuple [ VInt 42L; VString "ok" ] -> ()
        | value -> Assert.Fail($"Unexpected result: {Pretty.valueToString value}")
        second.RetainedCount |> should equal 1

    [<Test>]
    member _.``Failed submissions do not mutate the session`` () =
        let first = submit (Interactive.create []) "let value = 41"

        let fail () = submit first.Session "let broken = missing" |> ignore
        fail |> should throw typeof<TypeException>

        let second = submit first.Session "value + 1"
        match second.State.LastValue with
        | VInt 42L -> ()
        | value -> Assert.Fail($"Unexpected result: {Pretty.valueToString value}")
        second.RetainedCount |> should equal 1

    [<Test>]
    member _.``New type declarations rebuild retained inference safely`` () =
        let first = submit (Interactive.create []) "let value = 41"
        let second = submit first.Session "type Person = { Name: string }\nlet person = { Name = \"Ada\" }"
        let third = submit second.Session "(value + 1, person.Name)"

        match third.State.LastValue with
        | VTuple [ VInt 42L; VString "Ada" ] -> ()
        | value -> Assert.Fail($"Unexpected result: {Pretty.valueToString value}")
        third.RetainedCount |> should equal 3

    [<Test>]
    member _.``Later submissions do not replay retained evaluations`` () =
        let mutable calls = 0L
        let counter =
            { Name = "Counter.next"
              Scheme = Forall([], TFun(TUnit, TInt))
              Arity = 1
              Impl = fun _ _ ->
                  calls <- calls + 1L
                  VInt calls }

        let first = submit (Interactive.create [ counter ]) "let captured = Counter.next ()"
        let second = submit first.Session "captured"

        match second.State.LastValue with
        | VInt 1L -> ()
        | value -> Assert.Fail($"Unexpected result: {Pretty.valueToString value}")
        calls |> should equal 1L
