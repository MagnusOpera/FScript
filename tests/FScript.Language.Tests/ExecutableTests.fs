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
