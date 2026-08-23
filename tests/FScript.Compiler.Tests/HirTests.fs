namespace FScript.Compiler.Tests

open FScript.Compiler
open FScript.Language
open NUnit.Framework

[<TestFixture>]
type HirTests() =
    [<Test>]
    member _.``Lowers deterministic typed add program`` () =
        let source = "let add x y = x + y\nadd 1 2"
        let input = { Program = Parser.parseProgram source; Externs = [] }
        match Compiler.compile input with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            let actual = Hir.format compiled.Hir
            Assert.That(actual, Does.Contain("decl %"))
            Assert.That(actual, Does.Contain("primitive IntAdd"))
            Assert.That(actual, Does.Contain("entry"))
            Assert.That(Hir.format compiled.Hir, Is.EqualTo(actual))
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 3L))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes aggregate expressions in LIR`` () =
        let input = { Program = Parser.parseProgram "[1] @ [2; 3]"; Externs = [] }
        match Compiler.compile input with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 2L; LirValue.VInt 3L ]))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<TestCase("(1, true)")>]
    [<TestCase("Some 4")>]
    [<TestCase("{| Age = 42 |}.Age")>]
    member _.``Lowers tuples options and records`` (source: string) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Pass()
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes lexical let bindings and recursive closures`` () =
        let source = "let f x = (let rec loop n = if n = 0 then x else loop (n - 1)\n    loop 2\n)\nf 7"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 7L))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Raise preserves an interpreter diagnostic`` () =
        match Compiler.compile { Program = Parser.parseProgram "raise \"boom\""; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected failure")
            | Error diagnostics ->
                Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSI3011"))
                Assert.That(diagnostics |> List.map _.Message, Does.Contain("boom"))

    [<TestCase("match [1;2] with\n    | x::xs -> x\n    | [] -> 0", 1L)>]
    [<TestCase("match Some 3 with\n    | Some x -> x + 1\n    | None -> 0", 4L)>]
    [<TestCase("match (1, true) with\n    | (x, true) -> x\n    | _ -> 0", 1L)>]
    [<TestCase("match [1] with\n    | x::xs when x < 0 -> x\n    | _ -> 42", 42L)>]
    member _.``Executes deterministic pattern decision trees`` (source: string, expected: int64) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt expected))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes ranges indexing updates and interpolation`` () =
        let cases =
            [ "[1..3]", LirValue.VList [ LirValue.VInt 1L; LirValue.VInt 2L; LirValue.VInt 3L ]
              "[1; 2][1]", LirValue.VOption(Some(LirValue.VInt 2L))
              "let value = {| Count = 1 |}\n{| value with Count = 2 |}.Count", LirValue.VInt 2L
              "$\"value {1 + 2}\"", LirValue.VString "value 3" ]
        for source, expected in cases do
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
            | Ok compiled ->
                match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
                | Ok result -> Assert.That(result.Value, Is.EqualTo(expected), source)
                | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes declared union construction and matching`` () =
        let source = "type Shape = | Point | Circle of int\nlet radius shape =\n    match shape with\n    | Point -> 0\n    | Circle r -> r\nradius (Circle 3)"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 3L))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Executes ordered map construction spread and indexing`` () =
        let source = "let tail = { [\"a\"] = 2; [\"b\"] = 3 }\nlet values = { [\"a\"] = 1; ..tail }\nvalues[\"b\"]"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VOption(Some(LirValue.VInt 3L))))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Initializes mutual recursive groups atomically`` () =
        let source = "let rec even n =\n    if n = 0 then true else odd (n - 1)\nand odd n =\n    if n = 0 then false else even (n - 1)\n\neven 10"
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VBool true))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<TestCase("String.toUpper \"hello\"", "HELLO")>]
    [<TestCase("Int.toString (List.length [1; 2; 3])", "3")>]
    [<TestCase("Int.toString (Option.defaultValue 9 None)", "9")>]
    [<TestCase("Int.toString (Map.count { [\"a\"] = 1 })", "1")>]
    member _.``Executes portable scalar and collection intrinsics`` (source: string, expected: string) =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VString expected))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))

    [<Test>]
    member _.``Retains host linkage but rejects only reachable external calls`` () =
        let external : ExternalFunction =
            { Name = "Host.echo"
              Scheme = Forall([], TFun(TString, TString))
              Arity = 1
              Impl = fun _ values -> values.Head }
        let compile source = Compiler.compile { Program = Parser.parseProgram source; Externs = [ external ] }
        match compile "1" with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok result -> Assert.That(result.Value, Is.EqualTo(LirValue.VInt 1L))
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        match compile "Host.echo \"value\"" with
        | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
        | Ok compiled ->
            match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
            | Ok _ -> Assert.Fail("Expected unsupported external diagnostic")
            | Error diagnostics -> Assert.That(diagnostics |> List.map _.Code, Does.Contain("FSI3021"))

    [<Test>]
    member _.``Executes local binding patterns iteration and type tokens`` () =
        let cases =
            [ "let sum pair = (let (left, right) = pair\n    left + right\n)\nsum (1, 2)", LirValue.VInt 3L
              "for value in [1; 2; 3] do ignore value", LirValue.VUnit
              "type Package = { Name: string }\ntypeof Package", LirValue.VTypeToken(TRecord(Map.ofList [ "Name", TString ])) ]
        for source, expected in cases do
            match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
            | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
            | Ok compiled ->
                match LirInterpreter.execute { ScriptName = None; Arguments = [] } compiled.Lir with
                | Ok result -> Assert.That(result.Value, Is.EqualTo(expected), source)
                | Error diagnostics -> Assert.Fail(System.String.Join("; ", diagnostics |> List.map _.Message))
