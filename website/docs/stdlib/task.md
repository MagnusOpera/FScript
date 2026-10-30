---
id: stdlib-task
title: Task Module
slug: /stdlib/task
---

The Task module creates and composes concurrent work. All functions are optional .NET runtime externs: the CLI provides them by default; embedding hosts can register `TaskExterns.all` or a subset. `--no-default-externs` removes them. Canonical task execution is unsupported in Fable/JavaScript and in the standalone LIR interpreter; both compiled .NET backends implement the API.

A task has the native postfix type `'a task`. Lists are homogeneous: every candidate in a task list must return the same type. Callbacks return ordinary values, and their return value becomes the new task's result. No operation automatically flattens a task returned by a callback.

| Function | Signature |
| --- | --- |
| `Task.spawn` | `(unit -> 'a) -> 'a task` |
| `Task.delay` | `int -> unit task` |
| `Task.fromResult` | `'a -> 'a task` |
| `Task.continueWith` | `('a -> 'b) -> 'a task -> 'b task` |
| `Task.whenAll` | `('a list -> 'b) -> 'a task list -> 'b task` |
| `Task.whenAny` | `('a -> 'b) -> 'a task list -> 'b task` |
| `Task.await` | `'a task -> 'a` |
| `Task.awaitAll` | `'a task list -> 'a list` |

## `Task.spawn`

Schedules a `unit -> 'a` function concurrently and returns a task immediately.

```fsharp
let pending = Task.spawn (fun () -> 40 + 2)
Task.await pending
// 42
```

## `Task.delay`

Creates a task that completes with `()` after the specified milliseconds. It uses a timer, without occupying a worker thread during the delay. Allowed values are `0..2147483647`; negative or larger values raise an error. Zero produces a completed task. There is no infinite-delay or script cancellation overload. Stopping a debugger session cancels its outstanding timers so a long delay does not hold the session open.

```fsharp
let pending = Task.delay 100
Task.await pending
// ()
```

## `Task.fromResult`

Creates an already-completed task containing the supplied value. The value is evaluated before the call. The task still needs to be awaited, directly or through a consuming combinator.

```fsharp
Task.fromResult "ready" |> Task.await
// "ready"
```

## `Task.continueWith`

Takes the callback first and the task last, so it composes naturally with pipelines. The callback is scheduled asynchronously after successful completion of the input and receives its result directly. Its value becomes the returned task's result. Prerequisite failure skips the callback and propagates failure.

```fsharp
let next =
    Task.fromResult 41
    |> Task.continueWith (fun value -> value + 1)
Task.await next
// 42
```

Unlike .NET's task-handle-based `ContinueWith`, this callback receives the completed **value** and runs only on success. It has no scheduler, status-filter, or cancellation overloads. A separate `Task.map` would duplicate this operation and is not provided.

## `Task.whenAll`

Takes the callback first and a list of already-created tasks last. It waits asynchronously for every input, then schedules the callback once with results in **input order**, independently of completion order. The function does not start the input work again. Input failure skips the callback. An empty input invokes the callback with `[]`.

```fsharp
let tasks = [Task.fromResult 20; Task.fromResult 22]
let total =
    tasks
    |> Task.whenAll (fun values ->
        List.fold (fun sum value -> sum + value) 0 values)
Task.await total
// 42
```

```fsharp
List.init 0 Task.fromResult
|> Task.whenAll List.length
|> Task.await
// 0
```

## `Task.whenAny`

Takes the callback first and a nonempty list of already-created tasks last. The first observed completion selects a winner; the callback receives the winning **value**, rather than a task handle. It runs once after successful completion of that winner. A failed winner propagates failure without calling the callback. This selects the first completion, not the first success. Empty input raises an error because there is no value to supply.

If multiple inputs are already complete when the race is registered, the first completed input in list order is selected. Otherwise selection depends on scheduling. Other candidates keep running, are not cancelled, and must still be awaited. Their failures remain fatal under the normal background-failure policy.

```fsharp
let timers = [Task.delay 10; Task.delay 20]
let winner = timers |> Task.whenAny (fun () -> "a timer completed")
let message = Task.await winner
Task.awaitAll timers |> ignore
message
```

A list of `unit task` is valid: its callback can be `fun () -> ...`. For tasks producing different types, map their results into a user-declared discriminated union using `continueWith`:

```fsharp
type Completion =
    | Downloaded of string
    | TimedOut

let download = Task.fromResult "content"
let candidates =
    [ download |> Task.continueWith Downloaded
      Task.delay 100 |> Task.continueWith (fun () -> TimedOut) ]

let handled =
    candidates
    |> Task.whenAny (fun result ->
        match result with
        | Downloaded text -> $"received:{text}"
        | TimedOut -> "timed out")

let message = Task.await handled
// Observe the remaining work before the program finishes.
Task.awaitAll candidates |> ignore
message
```

No synthetic union or special matcher is needed. The usual type checker verifies a common `Completion` result type, and the callback uses ordinary pattern matching. This example does not cancel the download or abandon work on timeout.

## `Task.await`

Waits for one task and returns its result. Waiting synchronizes script execution; inside evaluator workers and the debugger, it suspends the continuation instead of occupying a scheduler worker. An input task can be awaited more than once.

```fsharp
let pending = Task.spawn (fun () -> 42)
Task.await pending
// 42
```

## `Task.awaitAll`

Waits for all input tasks and returns their results in input order. It returns a list, matching the homogeneous list input; it does not construct a runtime-sized tuple. Empty input returns `[]`.

```fsharp
Task.awaitAll [Task.fromResult "first"; Task.fromResult "second"]
// ["first";"second"]
```

## Nested tasks

A callback returning `'b task` produces `('b task) task`. Await both layers explicitly:

```fsharp
let outer =
    Task.fromResult 41
    |> Task.continueWith (fun value ->
        Task.fromResult (value + 1))
let inner = Task.await outer
Task.await inner
// 42
```

The same rule applies to `spawn`, `fromResult`, `whenAll`, and `whenAny`.

## Failure, lifetime, and sandbox behavior

- Errors remain fatal; these operations add no error recovery or cancellation mechanism. The evaluator's global background-failure checks can abort execution as soon as a spawned task fails, including a losing `whenAny` candidate. `whenAll` and `awaitAll` wait for all inputs when reached, but cannot override an earlier fatal background failure.
- Finishing a script with unawaited tasks is an error, even if those tasks have already completed. This applies to the evaluator and generated program `Run` boundaries. Native exported functions invoked separately by a .NET host retain host-owned CLR task lifetimes.
- `continueWith` consumes its prerequisite; `whenAll` consumes every prerequisite. Awaiting the returned task therefore covers those inputs. An unawaited returned task still causes an error.
- `whenAny` consumes only its selected winner. Keep references to other candidates and await them separately, for example with `awaitAll`.
- Tasks returned inside callbacks are separate results and still require their own awaits. Completing an outer task does not implicitly await its nested result.
- Callbacks run concurrently and may interleave effects. Their captured FFI state must be safe to share; use the task-sharing analysis to check potential races.
- These are runtime scheduling capabilities, not pure core functions. Delay observes time and `whenAny` observes completion order. They expose no filesystem or network IO, and hosts may omit the entire module for deterministic sandboxed execution.
