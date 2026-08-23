namespace FScript.Compiler.Tests

open System
open System.Text
open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type IrArtifactTests() =
    let compile source =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Ok value -> value
        | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``IR artifact is deterministic and executable after round trip`` () =
        let compiled = compile "let add x y = x + y\n(Some (add 1 2), [1] @ [2])"
        let first = IrArtifact.serialize compiled.Lir
        let second = IrArtifact.serialize compiled.Lir
        Assert.That((first = second), Is.True)
        match IrArtifact.deserialize first with
        | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
        | Ok loaded ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } loaded with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VTuple [ LirValue.VOption(Some(LirValue.VInt 3L)); LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 2L ] ]))
            | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``IR artifact rejects incompatible versions`` () =
        let bytes = compile "1" |> _.Lir |> IrArtifact.serialize
        let invalid = Encoding.UTF8.GetString(bytes).Replace("\"irVersion\": 1", "\"irVersion\": 999") |> Encoding.UTF8.GetBytes
        match IrArtifact.deserialize invalid with
        | Ok _ -> Assert.Fail("Expected incompatible-version diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5002"))

    [<Test>]
    member _.``IR artifact rejects corrupt JSON`` () =
        match IrArtifact.deserialize (Encoding.UTF8.GetBytes("{")) with
        | Ok _ -> Assert.Fail("Expected corrupt-artifact diagnostic")
        | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSA5003"))

    [<Test>]
    member _.``IR artifact round trips lexical let and fail nodes`` () =
        for source in [ "let f x = (let y = x + 1\n    y\n)\nf 2"; "raise \"failure\""; "match Some 2 with\n    | Some x when x > 0 -> x\n    | None -> 0" ] do
            let bytes = compile source |> _.Lir |> IrArtifact.serialize
            match IrArtifact.deserialize bytes with
            | Ok _ -> ()
            | Error diagnostics -> Assert.Fail(String.Join("; ", diagnostics |> List.map _.Message))
