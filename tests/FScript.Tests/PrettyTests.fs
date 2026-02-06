namespace FScript.Tests

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
        Helpers.evalToString "{ Name = \"a\"; Age = 1 }" |> should equal "{ Age = 1; Name = \"a\" }"
