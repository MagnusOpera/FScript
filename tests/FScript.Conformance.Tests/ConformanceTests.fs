namespace FScript.Conformance.Tests

open System
open FScript.Compiler
open FScript.Language
open NUnit.Framework

module private Harness =
    type Outcome = { Value: string; Stdout: string list }
    type FailureCategory = Raised | MatchFailure | DivisionByZero

    let private quoteString (value: string) =
        "\"" + value.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t") + "\""

    let rec private formatLirValue = function
        | LirValue.VUnit -> "()"
        | LirValue.VInt value -> string value
        | LirValue.VFloat value -> value.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | LirValue.VBool value -> string value |> _.ToLowerInvariant()
        | LirValue.VString value -> quoteString value
        | LirValue.VOption None -> "None"
        | LirValue.VOption(Some value) -> "Some " + formatLirValue value
        | LirValue.VList values -> "[" + (values |> List.map formatLirValue |> String.concat ";") + "]"
        | LirValue.VTuple values -> "(" + (values |> List.map formatLirValue |> String.concat ", ") + ")"
        | LirValue.VRecord fields -> "{ " + (fields |> Map.toList |> List.map (fun (name, value) -> name + " = " + formatLirValue value) |> String.concat "; ") + " }"
        | LirValue.VUnion(_, caseName, None) -> caseName
        | LirValue.VUnion(_, caseName, Some value) -> caseName + " " + formatLirValue value
        | LirValue.VMap entries -> "map { " + (entries |> List.map (fun (key, value) -> formatLirValue key + " => " + formatLirValue value) |> String.concat "; ") + " }"
        | LirValue.VTypeToken value -> "<type " + Types.typeToString value + ">"
        | LirValue.VClosure _ | LirValue.VIntrinsic _ | LirValue.VExternal _ | LirValue.VTargetImport _ -> "<function>"
        | LirValue.VRuntimeContext -> "<runtime-context>"
    let formatLir value = formatLirValue value

    let compile source =
        match Compiler.compile { Program = Parser.parseProgram source; Externs = [] } with
        | Ok value -> value
        | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))

    let legacy source =
        let output = ResizeArray<string>()
        let console : ExternalFunction =
            { Name = "Console.writeLine"; Scheme = Forall([], TFun(TString, TUnit)); Arity = 1
              Impl = fun _ values -> match values with [ VString value ] -> output.Add(value); VUnit | _ -> VUnit }
        let program = Parser.parseProgram source
        let value = program |> TypeInfer.inferProgramWithExterns [ console ] |> Eval.evalProgramWithExterns [ console ] |> Pretty.valueToString
        { Value = value; Stdout = List.ofSeq output }

    let lir source =
        let artifact = compile source |> _.Lir |> IrArtifact.serialize
        let loaded = match IrArtifact.deserialize artifact with Ok value -> value | Error diagnostics -> failwith (String.Join("; ", diagnostics |> List.map _.Message))
        match LirInterpreter.execute { ScriptName = None; Arguments = [] } loaded with
        | Ok result -> Ok { Value = formatLir result.Value; Stdout = result.Stdout }
        | Error diagnostic -> Error diagnostic.Code

    let legacyFailure source =
        try
            legacy source |> ignore
            None
        with
        | :? DivideByZeroException -> Some DivisionByZero
        | EvalException error when error.Message = "No match cases matched" -> Some MatchFailure
        | EvalException _ -> Some Raised

    let lirFailure source =
        match lir source with
        | Ok _ -> None
        | Error "FSI3005" -> Some DivisionByZero
        | Error "FSI3012" -> Some MatchFailure
        | Error "FSI3011" -> Some Raised
        | Error "FSI3022" -> Some Raised
        | Error code -> failwith $"Unexpected LIR failure category {code}"

[<TestFixture>]
type ConformanceTests() =
    [<TestCase("[Some 1; None] = [Some 1; None]")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet age (person: Person) = person.Age\nage {| Name = \"Ada\"; Age = 42 |}")>]
    [<TestCase("type Address = { City: string; Zip: int }\nlet check value = match value with | {| City: string |} -> 1 | _ -> 2\ncheck { City=\"Paris\"; Zip=75000 }")>]
    [<TestCase("type Shape = | Point | Circle of int\nmatch Circle 3 with | Point -> 0 | Circle radius -> radius")>]
    [<TestCase("let identity value = value\nidentity 42")>]
    [<TestCase("(1, 2)")>]
    [<TestCase("List.fold (fun state value -> state + value) 0 [1; 2; 3]")>]
    [<TestCase("let rec even n = if n = 0 then true else odd (n - 1)\nand odd n = if n = 0 then false else even (n - 1)\neven 10")>]
    [<TestCase("match { [\"answer\"] = 42 }[\"answer\"] with | Some value -> value | None -> 0")>]
    [<TestCase("Console.writeLine \"hello\"")>]
    [<TestCase("[Some (1, [2; 3]); None] = [Some (1, [2; 3]); None]")>]
    [<TestCase("type Person = { Name: string; Age: int }\n{ Name = \"Ada\"; Age = 42 } = {| Name = \"Ada\"; Age = 42 |}")>]
    [<TestCase("type Person = { Name: string; Age: int }\nlet person = { Name = \"Ada\"; Age = 42 }\n{ person with Age = 43 }.Age")>]
    [<TestCase("type Person = { Name: string }\ntype Lookup = | Missing | Found of Person\nFound { Name = \"Ada\" } = Found { Name = \"Ada\" }")>]
    [<TestCase("{ [\"b\"] = [2]; [\"a\"] = [1] } = { [\"a\"] = [1]; [\"b\"] = [2] }")>]
    [<TestCase("let applyTwice fn value = fn (fn value)\nlet add amount = fun value -> value + amount\napplyTwice (add 2) 3")>]
    [<TestCase("let identity value = value\n(identity [1; 2], identity (Some \"ok\"))")>]
    [<TestCase("match [1; 2] with | head::tail when List.length tail = 1 -> head | _ -> 0")>]
    [<TestCase("match { [\"a\"] = \"a\" } with | { [same] = same } -> true | _ -> false")>]
    [<TestCase("match { [\"a\"] = 1; [\"b\"] = 2 } with | { [k] = 2; ..rest } -> (k, Map.count rest) | _ -> (\"\",0)")>]
    [<TestCase("match { [1] = 2; [2] = 2 } with | { [x] = x; ..rest } -> (x,Map.containsKey 1 rest) | _ -> (0,false)")>]
    [<TestCase("(Int.tryParse \"42\", Int.tryParse \"x\", Float.tryParse \"1.5\", Bool.tryParse \"TRUE\", Bool.tryParse \"1\", Int.toString 42, Float.toString 1.5, Bool.toString false, String.replace \"a\" \"o\" \"banana\", String.indexOf \"x\" \"éx\", String.toLower \"ABC\", String.toUpper \"abc\", String.substring 1 1 \"aéz\", String.substring 8 1 \"short\", String.concat \",\" [\"a\"; \"b\"], String.split \",\" \"a,b\", String.endsWith \"bar\" \"foobar\")")>]
    [<TestCase("(List.map (fun value -> value + 1) [1; 2], List.iter (fun value -> ignore value) [1; 2], List.choose (fun value -> if value > 1 then Some value else None) [1; 2], List.collect (fun value -> [value; value]) [1; 2], List.exists (fun value -> value = 2) [1; 2], List.contains 2 [1; 2], List.rev [1; 2], List.distinct [1; 1; 2], List.fold (fun state value -> state + value) 0 [1; 2], List.filter (fun value -> value > 1) [1; 2], List.length [1; 2], List.tryFind (fun value -> value > 1) [1; 2], List.tryGet (fun value -> value = 2) [1; 2], List.tryItem 1 [1; 2], List.tryHead [1; 2], List.tail [1; 2], List.append [1] [2])")>]
    [<TestCase("(Option.defaultValue 7 None, Option.defaultWith (fun _ -> 8) None, Option.isNone None, Option.isSome (Some 1), Option.map (fun value -> value + 1) (Some 1))")>]
    [<TestCase("(Map.count Map.empty, Map.tryGet \"a\" (Map.ofList [(\"a\", 1)]), Map.containsKey \"a\" (Map.ofList [(\"a\", 1)]), Map.add \"b\" 2 (Map.ofList [(\"a\", 1)]), Map.fold (fun state key value -> state + value) 0 (Map.ofList [(\"a\", 1); (\"b\", 2)]), Map.filter (fun key value -> value > 1) (Map.ofList [(\"a\", 1); (\"b\", 2)]), Map.choose (fun key value -> if value > 1 then Some value else None) (Map.ofList [(\"a\", 1); (\"b\", 2)]), Map.map (fun value -> value + 1) (Map.ofList [(\"a\", 1)]), Map.iter (fun key value -> ignore value) (Map.ofList [(\"a\", 1)]), Map.remove \"a\" (Map.ofList [(\"a\", 1); (\"b\", 2)]))")>]
    [<TestCase("Map.ofList [(\"b\", 2); (\"a\", 1)]")>]
    [<TestCase("[\"a\"; \"b\"] |> String.concat \",\"")>]
    [<TestCase("type Contact = { City: string; Zip: int; Country: string }\nlet city (value: {| City: string |}) = value.City\ncity { City = \"Paris\"; Zip = 75000; Country = \"FR\" }")>]
    [<TestCase("type rec Node = { Value: int; Next: Node option }\nlet rec sum (node: Node) = match node.Next with | Some next -> node.Value + sum next | None -> node.Value\nsum { Value = 1; Next = Some { Value = 2; Next = None } }")>]
    [<TestCase("let value = 1\nlet value = value + 1\nvalue")>]
    [<TestCase("false && raise \"and-right\"")>]
    [<TestCase("true || raise \"or-right\"")>]
    [<TestCase("(fun value -> value) = (fun value -> value)")>]
    [<TestCase("type Package = { Name: string }\ntypeof Package = typeof Package")>]
    [<TestCase("[1; 2] = [1; 3]")>]
    [<TestCase("(1.5 < 2.0, 2.0 > 1.5, 1.5 <= 1.5, 2.0 >= 2.0)")>]
    [<TestCase("(1.0, [2.0], 1.0 / 0.0, 0.0 / 0.0)")>]
    member _.``Legacy and LIR artifact results conform`` (source: string) =
        let expected = Harness.legacy source
        match Harness.lir source with
        | Ok actual ->
            Assert.That(actual.Value, Is.EqualTo(expected.Value), source)
            Assert.That(System.String.Join("\n", actual.Stdout), Is.EqualTo(System.String.Join("\n", expected.Stdout)), source)
        | Error code -> Assert.Fail($"LIR failed with {code}: {source}")

    [<TestCase("raise \"boom\"")>]
    [<TestCase("match 1 with | 2 -> 0")>]
    [<TestCase("1 / 0")>]
    [<TestCase("List.tail []")>]
    member _.``Failure categories conform`` (source: string) =
        let expected = Harness.legacyFailure source
        Assert.That(expected.IsSome, Is.True, source)
        Assert.That(Harness.lirFailure source, Is.EqualTo(expected), source)
