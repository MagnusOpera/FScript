namespace FScript.Language.Tests

open NUnit.Framework
open FScript.Language
open FScript.Tests

[<TestFixture>]
type SupportLibraryTests() =
    static member Cases =
        SupportLibraryCases.core @ SupportLibraryCases.runtime
        |> List.map (fun (name,source,expected) -> TestCaseData(source,expected).SetName("SupportLibrary: " + name))

    [<TestCaseSource(nameof SupportLibraryTests.Cases)>]
    member _.``Pure library examples`` (source: string,expected: string) =
        let registration=HostInterop.ofExterns FScript.Runtime.PureExterns.functions
        let actual=Parser.parseProgram source |> TypeInfer.inferProgramWithRegistration registration |> Eval.evalProgramWithRegistration registration |> Pretty.valueToString
        Assert.That(actual,Is.EqualTo(expected))

    [<TestCase("Int.abs (0 - 9223372036854775807 - 1)")>]
    [<TestCase("Int.clamp 5 1 3")>]
    [<TestCase("Float.sign (Float.sqrt (0.0 - 1.0))")>]
    [<TestCase("List.take (0 - 1) [1]")>]
    [<TestCase("List.take 2 [1]")>]
    [<TestCase("List.skip 2 [1]")>]
    [<TestCase("List.sortBy (fun x -> (x,x)) [1]")>]
    [<TestCase("List.zip [1] []")>]
    [<TestCase("List.chunkBySize 0 [1]")>]
    [<TestCase("String.repeat (0 - 1) \"a\"")>]
    [<TestCase("Glob.filter \"a**\" []")>]
    [<TestCase("Glob.isMatch \"a**\" \"abc\"")>]
    member _.``Invalid library arguments raise a script error`` (source: string) =
        Assert.Throws<EvalException>(System.Action(fun () -> Helpers.eval source |> ignore)) |> ignore

    [<Test>]
    member _.``Every added function has an example`` () =
        let examples=SupportLibraryCases.core @ SupportLibraryCases.runtime |> List.map (fun (name,_,_) -> name) |> Set.ofList
        for name,_,_ in SupportLibrary.signatures @ SupportLibrary.runtimeSignatures do
            Assert.That(Set.contains name examples,Is.True,name)
