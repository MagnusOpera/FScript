namespace FScript.Runtime.Tests

open System
open FsCheck
open FsCheck.FSharp
open FScript.Language
open FScript.Runtime
open NUnit.Framework

module internal RuntimePropertySupport =
    let defaultReplay seed = { Rnd = Rnd seed; Size = None }
    let parseReplay (value: string) =
        match value.Split(',') with
        | [| seed; gamma |] -> { Rnd = Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size = None }
        | [| seed; gamma; size |] ->
            let size = Int32.Parse size
            if size < 0 then invalidArg "value" "Replay size must be nonnegative"
            { Rnd = Rnd(UInt64.Parse seed, UInt64.Parse gamma); Size = Some size }
        | _ -> invalidArg "value" "FSCRIPT_RUNTIME_REPLAY must be seed,gamma or seed,gamma,size"

    let run replay count arbitrary property =
        let mutable result = None
        let runner =
            { new IRunner with
                member _.OnStartFixture _ = ()
                member _.OnArguments(_, _, _) = ()
                member _.OnShrink(_, _) = ()
                member _.OnFinished(_, value) = result <- Some value }
        let config = Config.Quick.WithMaxTest(count).WithStartSize(0).WithEndSize(32).WithReplay(Some replay).WithRunner(runner)
        Check.One(config, Prop.forAll arbitrary property)
        result |> Option.defaultWith (fun () -> failwith "FsCheck did not report a result")

    let report name describe result =
        let details = Runner.onFinishedToStringWithFormatter (fun value -> "Runtime FFI generator version: 1\n" + describe value) name result
        match result with
        | TestResult.Failed(_, _, _, _, _, seed, size) ->
            details + $"\nReplay:\nFSCRIPT_RUNTIME_REPLAY='{seed.Seed},{seed.Gamma},{size}' dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter 'FullyQualifiedName~{name}'"
        | _ -> details

    let verify name count arbitrary seed property =
        let replay =
            match Environment.GetEnvironmentVariable "FSCRIPT_RUNTIME_REPLAY" with
            | null | "" -> defaultReplay seed
            | value -> parseReplay value
        match run replay count arbitrary property with
        | TestResult.Passed _ -> ()
        | result -> Assert.Fail(report name (sprintf "%A") result)

    // NUnit assertions record failures in TestContext even when FsCheck catches
    // their exceptions during shrinking. Report only the final result to NUnit.
    let equal (expected: 'T) (actual: 'T) =
        if not (Unchecked.equals expected actual) then failwithf "Expected: %A\nActual: %A" expected actual
    let raises<'T when 'T :> exn> action =
        let error = try action (); None with error -> Some error
        match error with
        | Some (:? 'T as error) -> error
        | Some error -> failwithf "Expected %s, received %s: %s" typeof<'T>.Name (error.GetType().Name) error.Message
        | None -> failwithf "Expected %s, but the operation succeeded" typeof<'T>.Name
    let contains (fragment: string) (text: string) =
        if not (text.Contains(fragment, StringComparison.Ordinal)) then failwithf "Expected %A in %A" fragment text
    type private Observed =
        | UnitData | IntegerData of int64 | FloatData of float | BooleanData of bool | StringData of string
        | ListData of Observed list | TupleData of Observed list | RecordData of (string * Observed) list
        | MapData of (MapKey * Observed) list | OptionData of Observed option | UnionData of string * string * Observed option

    let value (expected: Value) (actual: Value) =
        // Value contains callable/session fields and does not provide structural
        // CLR equality. Preserve both the data and its runtime union tag.
        let rec observe = function
            | VUnit -> UnitData | VInt n -> IntegerData n | VFloat n -> FloatData n
            | VBool b -> BooleanData b | VString text -> StringData text
            | VList items -> ListData(List.map observe items) | VTuple items -> TupleData(List.map observe items)
            | VRecord fields -> RecordData(fields |> Map.toList |> List.map (fun (k, v) -> k, observe v))
            | VMap entries -> MapData(entries |> Map.toList |> List.map (fun (k, v) -> k, observe v))
            | VOption item -> OptionData(Option.map observe item)
            | VUnionCase(name, case, item) -> UnionData(name, case, Option.map observe item)
            | other -> failwithf "Unexpected non-data property result: %A" other
        equal (observe expected) (observe actual)
    let some value = VOption(Some value)
    let invoke = HostTestHelpers.invoke
    let invokePure name args = PureExterns.functions |> List.find (fun fn -> fn.Name = name) |> fun fn -> invoke fn args
    let quote (text: string) = System.Text.Json.JsonSerializer.Serialize text
    let integerLiteral (number: int64) = if number < 0L then $"(0 - {-number})" else string number
    let integers values = values |> List.map (int64 >> VInt) |> VList

    let shrinkInt value = seq {
        if value <> 0 then yield 0
        let half = value / 2
        if half <> 0 && half <> value then yield half
    }

    let shrinkText (text: string) = seq {
        if text <> "" then yield ""
        // Remove Unicode scalars, never split a surrogate pair during shrinking.
        let scalars = text.EnumerateRunes() |> Seq.map string |> Seq.toArray
        for index in 0 .. scalars.Length - 1 do
            yield scalars |> Array.removeAt index |> String.concat ""
    }

    let shrinkList shrinkItem items =
        seq {
            if not (List.isEmpty items) then yield []
            for index in 0 .. List.length items - 1 do
                yield List.removeAt index items
                for smaller in shrinkItem items[index] do yield List.updateAt index smaller items
        } |> Seq.distinct

    let textWith tokens = Gen.sized (fun size -> gen {
        let! count = Gen.choose (0, min 16 size)
        let! pieces = Gen.listOfLength count (Gen.elements tokens)
        return String.concat "" pieces
    })
    let xmlText = textWith ["a"; "Z"; "0"; " "; "\n"; "\t"; "é"; "中"; "🙂"; "&"; "<"; ">"; "\""; "'"; "\\"]
    let text = textWith ["a"; "Z"; "0"; " "; "\n"; "\r"; "\t"; "\000"; "é"; "中"; "🙂"; "&"; "<"; ">"; "\""; "'"; "\\"; "%"; "+"]

    type Input = { Text: string; Bytes: int list; Numbers: int list; Number: int }
    let inputs =
        let generator = Gen.sized (fun size -> gen {
            let! text = text
            let! count = Gen.choose (0, min 8 size)
            let! numbers = Gen.listOfLength count (Gen.choose (-20, 20))
            let! byteCount = Gen.choose (0, min 64 (size * 2))
            let! bytes = Gen.listOfLength byteCount (Gen.choose (0, 255))
            let! number = Gen.choose (-100, 100)
            return { Text = text; Bytes = bytes; Numbers = numbers; Number = number }
        })
        let shrink sample = seq {
            for text in shrinkText sample.Text do yield { sample with Text = text }
            for bytes in shrinkList shrinkInt sample.Bytes do yield { sample with Bytes = bytes }
            for numbers in shrinkList shrinkInt sample.Numbers do yield { sample with Numbers = numbers }
            for number in shrinkInt sample.Number do yield { sample with Number = number }
        }
        Arb.fromGenShrink(generator, shrink)

open RuntimePropertySupport

[<TestFixture>]
type RuntimePropertyHarnessTests() =
    [<Test>]
    member _.CounterexamplesShrinkAndReplay() =
        let arb = Arb.fromGenShrink(Gen.constant [8; 3; 1], shrinkList shrinkInt)
        let execute replay = run replay 100 arb List.isEmpty<int>
        match execute (defaultReplay 104729UL) with
        | TestResult.Failed(data, original, reduced, _, _, seed, size) as failure ->
            equal true (data.NumberOfShrinks > 0)
            equal [0] (unbox<int list> (List.exactlyOne reduced))
            match execute (parseReplay $"{seed.Seed},{seed.Gamma},{size}") with
            | TestResult.Failed(_, again, shrunkAgain, _, _, _, _) -> equal original again; equal reduced shrunkAgain
            | _ -> Assert.Fail("Runtime replay did not reproduce the counterexample")
            let message = report "FfiPropertyTests.EncodingRoundTrips" (sprintf "%A") failure
            Assert.That(message, Does.Contain("FSCRIPT_RUNTIME_REPLAY="))
            Assert.That(message, Does.Contain("Runtime FFI generator version: 1"))
        | _ -> Assert.Fail("Injected failure was not detected")

    [<TestCase("1,2")>]
    [<TestCase("1,3,-1")>]
    [<TestCase("1,3,0,4")>]
    member _.InvalidReplayIsRejected(input: string) =
        Assert.Throws<ArgumentException>(Action(fun () -> parseReplay input |> ignore)) |> ignore
