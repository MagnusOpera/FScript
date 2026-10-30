---
id: tasks-and-concurrency
title: Tasks and Concurrency
slug: /language/tasks-and-concurrency
---

FScript supports concurrent thunk execution through the default `Task` module surface.

The `Task` functions are provided by the default runtime extern registry, so an embedding host may choose not to expose them.

## Task type

Task values use the native postfix type form:

```fsharp
'a task
```

Examples:

```fsharp
int task
string list task
```

Tasks are opaque handles. Create concurrent work with `Task.spawn`, completed values with `Task.fromResult`, and timers with `Task.delay`. Compose tasks with `continueWith`, `whenAll`, and `whenAny`; observe results with `await` or `awaitAll`.

## Spawning work

`Task.spawn` schedules a thunk for concurrent execution and returns immediately.

```fsharp
let work =
  Task.spawn (fun () -> 21 * 2)
let answer = Task.await work
```

Signature:

```fsharp
Task.spawn : (unit -> 'a) -> 'a task
```

FScript writes `unit -> 'a` in the type, and `fun () -> ...` is the preferred way to express that thunk.

## Awaiting results

Use `Task.await` to wait for completion and extract the result value.

```fsharp
let work = Task.spawn (fun () -> 21 * 2)
let answer = Task.await work
Console.writeLine $"{answer}"
```

Signature:

```fsharp
Task.await : 'a task -> 'a
```

## Concurrency model

- Spawned thunks run concurrently on the host runtime thread pool.
- Side effects may interleave across tasks and with the main script.
- `Task.await` is the explicit synchronization point for a task result.

## Failure model

FScript errors are fatal. Tasks follow the same rule.

- If a spawned thunk fails, the task has failed.
- `Task.await` on that task fails the script with the same fatal runtime error.
- Finishing a program with unawaited tasks is also a runtime error.

There is no catch/recovery mechanism in FScript today.

## Example

```fsharp
let a = Task.spawn (fun () -> 40 + 2)
let b = Task.spawn (fun () -> 10 * 3)

let result = (Task.await a, Task.await b)
Console.writeLine $"{result}"
```

For a larger example, see [`samples/parallel-quicksort.fss`](https://github.com/MagnusOpera/FScript/blob/main/samples/parallel-quicksort.fss).

Continue with [Guided Examples](../examples/guided-examples) to combine these features, or consult the [Stdlib Overview](../stdlib/overview) for available operations.

## Sharing FFI values between tasks

Immutable bindings can refer to mutable host resources. Use `fscript analyze program.fss`
to check potential cross-task instance and function sharing before execution. See
[Checking FFI Task Sharing](../learn/task-sharing-analysis.md) for the warning codes,
ordering examples, and the limits of this optional static check.

## Task composition

Callbacks receive values and return the result of the newly created task:

```fsharp
let delayed = Task.delay 10 |> Task.continueWith (fun () -> 20)
let ready = Task.fromResult 22
let total = [delayed; ready] |> Task.whenAll (List.fold (fun sum value -> sum + value) 0)
Task.await total
// 42
```

`whenAll` and `awaitAll` retain input order and allow empty lists. `whenAny` requires a nonempty list, selects the first completion, and passes that result to its callback. Use a user-defined discriminated union to combine different source result types. Other candidates keep running and must still be awaited. Nested task results require separate awaits. See the [Task reference](../stdlib/task) for every signature, unit-task examples, failure rules, and lifetime behavior.
