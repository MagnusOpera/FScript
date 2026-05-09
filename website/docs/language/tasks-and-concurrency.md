---
id: tasks-and-concurrency
title: Tasks and Concurrency
slug: /language/tasks-and-concurrency
---

FScript supports concurrent thunk execution through the built-in `Task` module.

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

Tasks are opaque handles. You create them with `Task.spawn` and observe them with `Task.await`.

## Spawning work

`Task.spawn` schedules a thunk for concurrent execution and returns immediately.

```fsharp
let work =
  Task.spawn (fun _ -> 21 * 2)
```

Signature:

```fsharp
Task.spawn : (unit -> 'a) -> 'a task
```

FScript writes `unit -> 'a` in the type, and you typically express that thunk as `fun _ -> ...`.

## Awaiting results

Use `Task.await` to wait for completion and extract the result value.

```fsharp
let work = Task.spawn (fun _ -> 21 * 2)
let answer = Task.await work
print $"{answer}"
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
let a = Task.spawn (fun _ -> 40 + 2)
let b = Task.spawn (fun _ -> 10 * 3)

let result = (Task.await a, Task.await b)
print $"{result}"
```

For a larger example, see [`samples/parallel-quicksort.fss`](https://github.com/MagnusOpera/FScript/blob/main/samples/parallel-quicksort.fss).
