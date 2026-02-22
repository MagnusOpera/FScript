namespace FScript.Language.Tests

open NUnit.Framework
open FsUnit
open FScript.Language

[<TestFixture>]
type ExecutableTests () =
    [<Test>]
    member _.``compile and execute returns same value as eval`` () =
        let program = FScript.parse "let add x y = x + y\nadd 20 22"
        let typed = FScript.infer program
        let compiled = FScript.compile typed
        let executed = FScript.execute compiled
        let interpreted = FScript.eval typed

        match executed, interpreted with
        | VInt 42L, VInt 42L -> ()
        | _ -> Assert.Fail("Expected compile/execute and eval to return 42")

    [<Test>]
    member _.``compileWithExterns and executeWithState support extern calls`` () =
        let ext: ExternalFunction =
            { Name = "answer"
              Scheme = Forall([], TFun(TUnit, TInt))
              Arity = 1
              Impl =
                fun _ ->
                    function
                    | [ VUnit ] -> VInt 42L
                    | _ -> failwith "unexpected args" }

        let typed =
            FScript.parse "answer ()"
            |> FScript.inferWithExterns [ ext ]
        let compiled = FScript.compileWithExterns [ ext ] typed
        let state = FScript.executeWithState compiled

        match state.LastValue with
        | VInt 42L -> ()
        | _ -> Assert.Fail("Expected 42")

    [<Test>]
    member _.``compiled executable can be executed multiple times`` () =
        let program = FScript.parse "let add x y = x + y\nadd 1 2"
        let typed = FScript.infer program
        let executable = FScript.compile typed

        let first = FScript.execute executable
        let second = FScript.execute executable

        match first, second with
        | VInt 3L, VInt 3L -> ()
        | _ -> Assert.Fail("Expected stable compiled execution result")

    [<Test>]
    member _.``compiled executable exposes exported metadata`` () =
        let source = "[<export>] let add x y = x + y\n[<export>] let version = \"1.0\""
        let typed = FScript.parse source |> FScript.infer
        let executable = FScript.compile typed
        let names = Executable.exportedNames executable
        let signatures = Executable.functionSignatures executable

        Assert.That(names, Does.Contain("add"))
        Assert.That(names, Does.Contain("version"))
        Assert.That(signatures.ContainsKey("add"), Is.True)
        Assert.That(signatures.ContainsKey("version"), Is.False)

    [<Test>]
    member _.``loadExecutable materializes exported invokers`` () =
        let source = "[<export>] let add x y = x + y\n[<export>] let version = \"1.0\""
        let typed = FScript.parse source |> FScript.infer
        let executable = FScript.compile typed
        let loaded = FScript.loadExecutable executable

        match Executable.invoke loaded "add" [ VInt 1L; VInt 2L ] with
        | Some (VInt 3L) -> ()
        | _ -> Assert.Fail("Expected add invoker to be materialized")

        match Executable.getValue loaded "version" with
        | Some (VString "1.0") -> ()
        | _ -> Assert.Fail("Expected exported value")
