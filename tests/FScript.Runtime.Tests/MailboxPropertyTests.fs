namespace FScript.Runtime.Tests

open System
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open MailboxProperties

[<TestFixture>]
type MailboxPropertyTests() =
    let verify name scenario count seed =
        let replay =
            match Environment.GetEnvironmentVariable "FSCRIPT_MAILBOX_REPLAY" with
            | null | "" -> defaultReplay seed
            | value -> parseReplay value
        match run replay count arbitrary (check scenario) with
        | TestResult.Passed _ -> ()
        | result -> Assert.Fail(report name scenario result)

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.OperationSequencesMatchReference(seed: uint64) = verify "OperationSequencesMatchReference" Sequential 100 seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.HandlerFailuresMatchReference(seed: uint64) = verify "HandlerFailuresMatchReference" HandlerFailure 100 seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.FinishDrainsWaitingPosts(seed: uint64) = verify "FinishDrainsWaitingPosts" Backpressure 30 seed

    [<TestCase(104729UL)>]
    [<TestCase(130363UL)>]
    member _.HandlerFailureWakesWaitingOperations(seed: uint64) = verify "HandlerFailureWakesWaitingOperations" BackpressureFailure 30 seed

    [<Test>]
    member _.ReferenceModelTracksOrderAndLifecycle() =
        let sample = { Capacity = 1; Initial = 3; Prefix = [2; -1]; Operations = [Finish; Finish; Post 7; Finish] }
        let state = { Total = 15L; Seen = [2; -1] }
        Assert.That(expected Sequential sample, Is.EqualTo([Posted; Posted; Finished state; Finished state; RejectedClosed; Finished state; Finished state]))
        let failed = { sample with Operations = [Post 1; Finish; Post 2; Finish] }
        Assert.That(expected HandlerFailure failed, Is.EqualTo([Posted; Posted; Posted; HandlerFailed; HandlerFailed; RejectedClosed; HandlerFailed; HandlerFailed; HandlerFailed; RejectedClosed]))
        let blockedFailure = { sample with Capacity = 2; Operations = [Post 0; Finish] }
        Assert.That(expected BackpressureFailure blockedFailure, Is.EqualTo([Posted; Posted; Posted; HandlerFailed; HandlerFailed; RejectedClosed; RejectedClosed; HandlerFailed; HandlerFailed]))
        Assert.That(expectedAttempts BackpressureFailure blockedFailure, Is.EqualTo([99]))

    [<Test>]
    member _.ShrinkingKeepsTheMailboxScenarioValid() =
        let original = { Capacity = 4; Initial = -7; Prefix = [3; -2; 8; 1]; Operations = [Post 3; Finish; Post -5; Finish] }
        for sample in seq { yield original; yield! shrink original } do
            Assert.That(sample.Capacity, Is.InRange(1, 4))
            for scenario in [Sequential; HandlerFailure] do check scenario sample
        // The gated setup has mandatory capacity-filling posts and a waiting
        // post, even when every optional operation has disappeared.
        check Backpressure { Capacity = 1; Initial = 0; Prefix = []; Operations = [] }
        check BackpressureFailure { Capacity = 1; Initial = 0; Prefix = []; Operations = [] }

    [<Test>]
    member _.LostPendingPostCounterexamplesShrinkAndReplay() =
        let noisy = generator |> Gen.map (fun sample -> { sample with Capacity = 4; Initial = 7; Prefix = [8; -3]; Operations = Finish :: sample.Operations })
        let arb = Arb.fromGenShrink(noisy, shrink)
        // Simulate dropping the already-waiting post at shutdown. The history
        // makes this observable even after messages and initial state become zero.
        let faulty sample =
            let operations = commands Backpressure sample
            let lost =
                modelObservations sample (List.removeAt sample.Capacity operations)
                |> List.insertAt sample.Capacity Posted
            let predicted = expected Backpressure sample
            if predicted <> lost then failwithf "Injected lost pending post.\nReference: %A\nActual: %A" predicted lost
        let execute replay = run replay 100 arb faulty
        match execute (defaultReplay 104729UL) with
        | TestResult.Failed(data, originals, reduced, _, _, seed, size) as failure ->
            let original = unbox<Sample> (List.exactlyOne originals)
            let smallest = unbox<Sample> (List.exactlyOne reduced)
            Assert.That(data.NumberOfShrinks, Is.GreaterThan(0))
            Assert.That(smallest, Is.EqualTo({ Capacity = 1; Initial = 0; Prefix = []; Operations = [] }))
            match execute (parseReplay $"{seed.Seed},{seed.Gamma},{size}") with
            | TestResult.Failed(_, replayOriginals, replayReduced, _, _, _, _) ->
                Assert.That(unbox<Sample> (List.exactlyOne replayOriginals), Is.EqualTo(original))
                Assert.That(unbox<Sample> (List.exactlyOne replayReduced), Is.EqualTo(smallest))
            | _ -> Assert.Fail("Replay did not reproduce the lost-post counterexample")
            let message = report "FinishDrainsWaitingPosts" Backpressure failure
            Assert.That(message, Does.Contain("Mailbox generator version: 1"))
            Assert.That(message, Does.Contain("Reference observations:"))
            Assert.That(message, Does.Contain("FSCRIPT_MAILBOX_REPLAY="))
            check Backpressure smallest
        | _ -> Assert.Fail("The injected lost-post fault was not detected")

    [<Test>]
    member _.FailureDescriptionsKeepTheCompleteSequence() =
        let sample = { Capacity = 1; Initial = 0; Prefix = []; Operations = [for value in -8 .. 7 -> Post value] }
        let description = describe Sequential sample
        Assert.That(description, Does.Contain("Post 7"))
        Assert.That(description, Does.Not.Contain("..."))

    [<TestCase("1,2")>]
    [<TestCase("1,3,-1")>]
    [<TestCase("1,3,0,4")>]
    member _.InvalidReplayIsRejected(value: string) =
        Assert.Throws<ArgumentException>(Action(fun () -> parseReplay value |> ignore)) |> ignore
