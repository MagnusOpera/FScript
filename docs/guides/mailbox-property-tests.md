# Mailbox property tests

The runtime suite compares the real `MailboxProvider` operations with a pure state
model. Run the campaign with:

```sh
dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter FullyQualifiedName~MailboxPropertyTests
```

The fixed seeds are `104729` and `130363`, matching the inference campaigns.
Each seed runs 100 ordinary operation sequences, 100 handler-failure sequences,
30 gated backpressure sequences, and 30 gated failure sequences, for 520 generated
cases.

## Bounds and reference state

Each sample has capacity 1 through 4, initial state -9 through 9, up to eight
prefix messages, and up to sixteen optional `Post` or `Finish` commands. Ordinary
messages range from -9 through 9. FsCheck grows samples from size zero to forty.
Scenario setup adds required commands, giving at most 28 commands per case.

The reference state records the admitted messages in order, an integer total,
and whether the mailbox is closed or its handler has failed. A successful message
updates the total to `2 * total + message`. The bounds keep this calculation
within `int64`. The history detects drops, duplicates, and reordered messages even
when totals happen to coincide.

The model predicts every operation's observation: a successful post, a finished
state snapshot, a post rejected after closure, or a handler failure. Once closed,
a new post is rejected without changing state. Repeated finishes return the same
snapshot or failure. The model does not read the runtime queue, worker flags, or
mailbox payload. The test handler has a separate implementation of state updates.

## Deterministic execution

Ordinary cases use one producer and real asynchronous workers. The model applies
successful posts in producer order; it observes state only after `Finish` has
drained admitted work. Worker scheduling does not change the expected results.

Handler-failure cases insert message `99`, which is outside the generated message
range and causes the handler to throw a known exception. The harness joins the
worker after each command, so the failure has occurred before the next operation
is observed. Shrinking cannot remove this mandatory fault. Unexpected exceptions
and unexpected worker failures fail the property.

Backpressure cases hold the worker behind an event, fill the queue to its exact
capacity, and start one additional post. A callback from the provider's wait loop
confirms that the post is waiting. Another callback confirms that `Finish` is
waiting after closing admission. A later post must be rejected. Releasing the
worker must complete the already-waiting post and return a state containing it.
The reference order follows admission before closure, even though the pending
post's call returns later. Optional commands then exercise the closed mailbox.

Gated failure cases hold the faulting handler after it dequeues its message, then
fill the queue and start the pending post and finish call. Holding the handler
prevents it from freeing another queue slot before failure. All queue-filling
posts must be admitted before release. Once the handler fails,
both the already-waiting post and `Finish` must wake and report that failure.
The model's processing phase then enters a closed, failed state. An independent
callback trace checks that later queued messages were not handled. All scenarios
compare the complete handler-attempt trace as well as operation observations.

No sleep durations or guesses about thread scheduling determine an expectation.
Five-second cancellation and wait limits detect lack of progress. Every case
releases held workers, closes admission, and joins its tasks before disposing
synchronization objects, including on assertion failures.

These campaigns test the runtime provider and its shared mailbox queue. They do
not enumerate arbitrary producer schedules or certify the interpreter's task
scope, SDK bridge, or generated backends. Existing mailbox integration tests and
sample smoke tests exercise those surrounding paths. See the
[mailbox contract](../specs/stdlib-functions.md#mailbox).

## Shrinking and replay

Shrinking removes optional command sequences and prefix messages, reduces
message and initial values toward zero, and reduces capacity toward one. Mandatory
scenario setup remains: handler-failure cases retain the fault, and backpressure
cases still fill the queue and start a waiting post. Gated failure cases also keep
the faulting handler in flight before filling the queue. No candidate is accepted or
discarded based on runtime output.

Failures print the mailbox generator version, scenario, original and reduced
samples, commands, reference observations, and actual discrepancy or exception.
Copy the emitted replay command, for example:

```sh
FSCRIPT_MAILBOX_REPLAY='123,4567,20' dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter 'FullyQualifiedName~MailboxPropertyTests.FinishDrainsWaitingPosts'
```

The fields are the failing generation seed, its odd gamma, and the size. With all
three fields, FsCheck regenerates that one case and repeats shrinking. Both seed
variants of the selected NUnit test use the override. Omitting the size replays
a complete campaign using another seed:

```sh
FSCRIPT_MAILBOX_REPLAY='123,4567' dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter FullyQualifiedName~MailboxPropertyTests
```

Unset the variable to restore the fixed seeds. Mailbox generator version 1 has
its own replay variable and does not change the
[inference generator](inference-property-tests.md). Replaying requires the same
generator and FsCheck version. Save a discovered reduced sequence as a focused
regression test rather than relying indefinitely on its seed.

Harness tests check the model against handwritten outcomes, run reduced cases
against the provider, and inject a dropped pending post. They verify that this
failure shrinks to capacity one with zero-valued messages and that replay
reproduces both the original and reduced counterexamples.
