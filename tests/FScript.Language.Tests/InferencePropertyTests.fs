namespace FScript.Language.Tests

open System
open FsCheck
open FsCheck.FSharp
open FScript.Language
open NUnit.Framework
open InferenceProperties

[<TestFixture>]
type InferencePropertyTests() =
    let verify name scenario seed =
        let replay =
            match Environment.GetEnvironmentVariable "FSCRIPT_INFERENCE_REPLAY" with
            | null | "" -> defaultReplay seed
            | value -> parseReplay value
        let result = run replay 150 arbitrary (fun sample -> programs scenario sample |> List.iter check)
        match result with
        | TestResult.Passed _ -> ()
        | _ -> Assert.Fail(failureMessage name scenario result)

    static member GenericAggregateCases =
        [ for index, aggregate in List.indexed aggregates do
              for seed in [104729UL; 130363UL] do
                  yield TestCaseData(seed, index).SetName($"{{m}}({seed},{aggregate})") ]

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.PureBindingsRemainPolymorphic(seed: uint64) = verify "PureBindingsRemainPolymorphic" PureBinding seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.CapturedParametersRemainMonomorphic(seed: uint64) = verify "CapturedParametersRemainMonomorphic" CapturedParameter seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.CapturedOpaqueInstancesRemainMonomorphic(seed: uint64) = verify "CapturedOpaqueInstancesRemainMonomorphic" SharedOpaque seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.SeparateFactoryCallsRemainPolymorphic(seed: uint64) = verify "SeparateFactoryCallsRemainPolymorphic" FreshFactories seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.UnrelatedCallbacksRemainPolymorphic(seed: uint64) = verify "UnrelatedCallbacksRemainPolymorphic" IndependentCallback seed

    [<TestCaseSource(nameof InferencePropertyTests.GenericAggregateCases)>]
    member _.GenericValuesRemainPolymorphic(seed: uint64, aggregateIndex: int) =
        verify "GenericValuesRemainPolymorphic" (GenericValues aggregates[aggregateIndex]) seed

    [<TestCaseSource(nameof InferencePropertyTests.GenericAggregateCases)>]
    member _.GenericAggregatesPreserveCapturedOpaqueTypes(seed: uint64, aggregateIndex: int) =
        verify "GenericAggregatesPreserveCapturedOpaqueTypes" (GenericCapturedOpaque aggregates[aggregateIndex]) seed

    [<TestCaseSource(nameof InferencePropertyTests.GenericAggregateCases)>]
    member _.GenericFactoriesInstantiateIndependently(seed: uint64, aggregateIndex: int) =
        verify "GenericFactoriesInstantiateIndependently" (GenericFactories aggregates[aggregateIndex]) seed

    [<TestCaseSource(nameof InferencePropertyTests.GenericAggregateCases)>]
    member _.GenericCallbacksStayIndependentOfOpaqueInstances(seed: uint64, aggregateIndex: int) =
        verify "GenericCallbacksStayIndependentOfOpaqueInstances" (GenericCallbacks aggregates[aggregateIndex]) seed

    [<Test>]
    member _.ShrinkingPreservesCaptureAndConflictingUses() =
        // Check every immediate reduction of a case using all wrappers. Both
        // acceptance controls and the conflicting uses must retain their oracle.
        let original =
            { First = Boolean; Second = Integer; Shape = PairOf(ListOf Hole, OptionOf Hole)
              Transports = transports; Explicit = true }
        for sample in seq { yield original; yield! shrink original } do
            for scenario in [CapturedParameter; SharedOpaque; FreshFactories; IndependentCallback; PureBinding] do
                programs scenario sample |> List.iter check

    [<Test>]
    member _.GenericAggregateShrinkingPreservesTheInteraction() =
        let original =
            { First = Boolean; Second = Text; Shape = BoxOf(ChoiceOf(ListOf Hole))
              Transports = transports; Explicit = true }
        for sample in seq { yield original; yield! shrink original } do
            for aggregate in aggregates do
                for scenario in [GenericValues aggregate; GenericCapturedOpaque aggregate; GenericFactories aggregate; GenericCallbacks aggregate] do
                    programs scenario sample |> List.iter check

    [<Test>]
    member _.FailuresShrinkAndReplayWithSourceAndExpectations() =
        // Simulate a bug that loses an instance's type when transported through
        // a record. FsCheck must preserve that trigger while removing noise.
        let faulty sample = not (List.contains Record sample.Transports)
        let execute replay = run replay 150 arbitrary faulty
        let initial = execute (defaultReplay 104729UL)
        match initial with
        | TestResult.Failed(data, originals, reduced, _, _, seed, size) ->
            let original = unbox<Sample> (List.exactlyOne originals)
            let smallest = unbox<Sample> (List.exactlyOne reduced)
            Assert.That(data.NumberOfShrinks, Is.GreaterThan(0))
            Assert.That(smallest.Transports, Is.EqualTo([Record]))
            Assert.That(smallest.Shape, Is.EqualTo(Hole))
            Assert.That(smallest.Explicit, Is.False)
            Assert.That((smallest.First, smallest.Second), Is.EqualTo((Integer, Text)))
            let replay = parseReplay $"{seed.Seed},{seed.Gamma},{size}"
            match execute replay with
            | TestResult.Failed(_, replayOriginals, replayReduced, _, _, _, _) ->
                Assert.That(unbox<Sample> (List.exactlyOne replayOriginals), Is.EqualTo(original))
                Assert.That(unbox<Sample> (List.exactlyOne replayReduced), Is.EqualTo(smallest))
            | _ -> Assert.Fail("Replay did not reproduce the injected failure")
            let message = failureMessage "CapturedOpaqueInstancesRemainMonomorphic" SharedOpaque initial
            Assert.That(message, Does.Contain("FSCRIPT_INFERENCE_REPLAY="))
            Assert.That(message, Does.Contain("Generator version: 2"))
            Assert.That(message, Does.Contain("Expected: Reject"))
            Assert.That(message, Does.Contain("Cell.create []"))
            programs SharedOpaque smallest |> List.iter check
        | _ -> Assert.Fail("The injected fault was not detected")

    [<TestCase(true)>]
    [<TestCase(false)>]
    member _.GenericFailuresShrinkAndReplayWithTheirContainer(recordPayload: bool) =
        let rec containsTarget = function
            | BoxOf _ when recordPayload -> true
            | ChoiceOf _ when not recordPayload -> true
            | BoxOf inner | ChoiceOf inner | ListOf inner | OptionOf inner -> containsTarget inner
            | PairOf(left, right) -> containsTarget left || containsTarget right
            | Hole -> false
        let scenario = GenericCapturedOpaque (if recordPayload then RecordInUnion else UnionInRecord)
        // Inject a fault requiring a generic payload shape. Shrinking must keep
        // that shape as well as the scenario's mandatory opaque container.
        let execute replay = run replay 150 arbitrary (fun sample -> not (containsTarget sample.Shape))
        match execute (defaultReplay 104729UL) with
        | TestResult.Failed(data, originals, reduced, _, _, seed, size) as failure ->
            let original = unbox<Sample> (List.exactlyOne originals)
            let smallest = unbox<Sample> (List.exactlyOne reduced)
            Assert.That(data.NumberOfShrinks, Is.GreaterThan(0))
            Assert.That(smallest.Shape, Is.EqualTo(if recordPayload then BoxOf Hole else ChoiceOf Hole))
            Assert.That(smallest.Transports, Is.Empty)
            match execute (parseReplay $"{seed.Seed},{seed.Gamma},{size}") with
            | TestResult.Failed(_, replayOriginals, replayReduced, _, _, _, _) ->
                Assert.That(unbox<Sample> (List.exactlyOne replayOriginals), Is.EqualTo(original))
                Assert.That(unbox<Sample> (List.exactlyOne replayReduced), Is.EqualTo(smallest))
            | _ -> Assert.Fail("Generic replay did not reproduce the injected failure")
            let message = failureMessage "GenericAggregatesPreserveCapturedOpaqueTypes" scenario failure
            Assert.That(message, Does.Contain("Generator version: 2"))
            Assert.That(message, Does.Contain(if recordPayload then "&Name~RecordInUnion" else "&Name~UnionInRecord"))
            Assert.That(message, Does.Contain("Expected: Reject"))
            programs scenario smallest |> List.iter check
        | _ -> Assert.Fail("The injected generic payload fault was not detected")

    [<TestCase("Carry")>]
    [<TestCase("Envelope.Carry")>]
    [<TestCase("alias")>]
    member _.UnionConstructorsForwardOpaqueFactories(constructor: string) =
        let source =
            "type Envelope<'a> = | Carry of 'a\nlet alias = Carry\n"
            + "let make () =\n    let cell = Cell.create []\n    fun () -> Cell.read cell\n"
            + $"let packed = {constructor} make\nlet factory = match packed with | Carry value -> value\n"
            + "let first = factory ()\nlet second = factory ()\n"
            + "let ints (value: int list) = value\nlet words (value: string list) = value\n"
            + "(ints (first ()), words (second ()))"
        check { Source = source; Expected = Accept(TTuple [TList TInt; TList TString]) }

    [<TestCase("let Carry factory = factory ()\nlet read = Carry make\n")>]
    [<TestCase("let allocate Carry = Carry make\nlet read = allocate (fun factory -> factory ())\n")>]
    [<TestCase("let allocate () =\n    let Carry factory = factory ()\n    Carry make\nlet read = allocate ()\n")>]
    member _.ShadowedConstructorsCannotForwardAllocatedInstances(binding: string) =
        let prefix =
            "type Envelope<'a> = | Carry of 'a\n"
            + "let make () =\n    let cell = Cell.create []\n    fun () -> Cell.read cell\n"
            + "let ints (value: int list) = value\nlet words (value: string list) = value\n"
            + binding
        check { Source = prefix + "(ints (read ()), ints (read ()))"; Expected = Accept(TTuple [TList TInt; TList TInt]) }
        check { Source = prefix + "(ints (read ()), words (read ()))"; Expected = Reject }

    [<TestCase("1,2")>]
    [<TestCase("1,3,-1")>]
    [<TestCase("1,3,0,4")>]
    member _.InvalidReplayIsRejected(value: string) =
        Assert.Throws<ArgumentException>(Action(fun () -> parseReplay value |> ignore)) |> ignore
