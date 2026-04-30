---
id: stdlib-task
title: Task Module
slug: /stdlib/task
---

The `Task` module provides the built-in task API for concurrent execution.

## Task type

Task values use the native postfix type form:

```fsharp
'a task
```

## `Task.spawn : (unit -> 'a) -> 'a task`

Schedules a thunk for concurrent execution and returns an opaque task handle immediately.

```fsharp
let pending =
  Task.spawn (fun _ -> "done")
```

Notes:

- The thunk is represented as a `unit -> 'a` function.
- In script code that usually appears as `fun _ -> ...`.
- The returned task can be awaited later.

## `Task.await : 'a task -> 'a`

Waits for a task to complete and returns its result.

```fsharp
let pending = Task.spawn (fun _ -> 40 + 2)
let answer = Task.await pending
```

## Runtime behavior

- Spawned thunks run concurrently.
- Side effects such as `print` may interleave.
- Task failures are fatal runtime errors.
- A script cannot finish with unawaited tasks.

## Example

```fsharp
let left = Task.spawn (fun _ -> [1; 2; 3] |> List.map (fun n -> n * 2))
let right = [10; 20; 30] |> List.map (fun n -> n + 1)

let combined = (Task.await left) @ right
print $"{combined}"
```
