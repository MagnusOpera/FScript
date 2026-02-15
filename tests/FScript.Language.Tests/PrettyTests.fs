namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type PrettyTests () =
    [<Test>]
    member _.``Formats list result`` () =
        Helpers.evalToString "[1;2] @ [3]" |> should equal "[1;2;3]"

    [<Test>]
    member _.``Formats option result`` () =
        Helpers.evalToString "Some 4" |> should equal "Some 4"
        Helpers.evalToString "None" |> should equal "None"

    [<Test>]
    member _.``Formats tuple and record`` () =
        Helpers.evalToString "(1, true)" |> should equal "(1, true)"
        Helpers.evalToString "{| Name = \"a\"; Age = 1 |}" |> should equal "{ Age = 1; Name = \"a\" }"

    [<Test>]
    member _.``Formats function values with parameter names`` () =
        Helpers.evalToString "fun x -> x" |> should equal "<fun x>"
        Helpers.evalToString "fun x -> fun y -> x + y" |> should equal "<fun x y>"

    [<Test>]
    member _.``Formats extern values with name and application progress`` () =
        Helpers.evalToString "print" |> should equal "<extern print>"
