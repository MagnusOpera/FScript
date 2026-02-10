namespace FScript.Runtime.Tests

open System.IO
open NUnit.Framework
open FScript.Language
open FScript.Runtime

[<TestFixture>]
type ScriptHostTests () =
    [<Test>]
    member _.``script_host lists and invokes exported function`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "[<export>] let add x y = x + y\nlet value = 42"
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("add"))

        match ScriptHost.invoke loaded "add" [ VInt 1L; VInt 2L ] with
        | VInt 3L -> ()
        | _ -> Assert.Fail("Expected add result 3")

    [<Test>]
    member _.``script_host invokes exported closure function`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "let makeAdder x = fun y -> x + y\n[<export>] let add2 = makeAdder 2"
        match ScriptHost.invoke loaded "add2" [ VInt 5L ] with
        | VInt 7L -> ()
        | _ -> Assert.Fail("Expected closure invocation result 7")

    [<Test>]
    member _.``script_host exposes exported values separately from functions`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "[<export>] let version = \"1.0\"\n[<export>] let add x y = x + y"

        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("add"))
        Assert.That(ScriptHost.listValues loaded, Does.Contain("version"))

        match ScriptHost.getValue loaded "version" with
        | VString "1.0" -> ()
        | _ -> Assert.Fail("Expected exported value")

    [<Test>]
    member _.``script_host rejects invocation of exported value`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "[<export>] let version = \"1.0\""
        let act () = ScriptHost.invoke loaded "version" [ VUnit ] |> ignore
        Assert.Throws<EvalException>(TestDelegate act) |> ignore

    [<Test>]
    member _.``script_host hides non-exported functions`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let loaded = ScriptHost.loadSource externs "let private_fn x = x + 1\nlet value = 1"

        Assert.That(ScriptHost.listFunctions loaded, Is.Empty)
        let hidden () = ScriptHost.invoke loaded "private_fn" [ VInt 1L ] |> ignore
        Assert.Throws<EvalException>(TestDelegate hidden) |> ignore

    [<Test>]
    member _.``script_host exports recursive groups when marked exported`` () =
        let externs = Registry.all { RootDirectory = Directory.GetCurrentDirectory() }
        let source = "[<export>] let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)"
        let loaded = ScriptHost.loadSource externs source
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("even"))
        Assert.That(ScriptHost.listFunctions loaded, Does.Contain("odd"))
        match ScriptHost.invoke loaded "even" [ VInt 4L ] with
        | VBool true -> ()
        | _ -> Assert.Fail("Expected even 4 to be true")
