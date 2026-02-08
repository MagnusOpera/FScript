namespace FScript.Runtime.Tests

open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type ScriptHostTests () =
    [<Test>]
    member _.``script_host lists and invokes declared function`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "let add x y = x + y\nlet value = 42"
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("add"))

        match ScriptHost.invoke loaded "add" [ VInt 1L; VInt 2L ] with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected add result 3")

    [<Test>]
    member _.``script_host invokes closure function`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "let makeAdder x = fun y -> x + y\nlet add2 = makeAdder 2"
        match ScriptHost.invoke loaded "add2" [ VInt 5L ] with
        | VInt 7L -> ()
        | _ -> Assert.Fail("Expected closure invocation result 7")

    [<Test>]
    member _.``script_host reports unknown and non callable symbols`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "let value = 1"

        let unknown () = ScriptHost.invoke loaded "missing" [] |> ignore
        Assert.Throws<EvalException>(TestDelegate unknown) |> ignore

        let nonCallable () = ScriptHost.invoke loaded "value" [] |> ignore
        Assert.Throws<EvalException>(TestDelegate nonCallable) |> ignore
