---
title: Checking FFI Task Sharing
slug: /learn/task-sharing-analysis
---

FScript bindings and collections are immutable. A host object passed through FFI
can still contain a mutable cache, an open connection, or a native handle. Two
tasks can hold the same object and call into it concurrently, even though neither
task changes an FScript binding. If the host API cannot handle that access, the
script can fail at runtime.

`fscript analyze` helps find potential sharing before running a script. It reports
where tasks may share host instances or call host functions whose thread safety
is unknown. It is an optional static check, separate from compilation and execution.

## Run the check

```sh
fscript analyze program.fss
```

The command parses and type-checks the script, resolves its imports, and reads the
same configured FFI environment used for execution. It supports assembly and
NuGet providers selected through `fscript.toml`:

```sh
fscript analyze program.fss --config fscript.toml
fscript analyze program.fss --extern-assembly ./MyProvider.dll
```

It does not evaluate script initializers, launch script tasks, or invoke registered
FFI implementations. Loading a provider can still run its registration factory
and contract metadata methods. Use trusted providers, just as you would when
running or compiling the script.

Warnings are written to stderr. They do not make the command fail: its exit status
is `0` when analysis completes, including when warnings are reported. Invalid
syntax, type errors, and environment-loading failures return a nonzero status.
A clean check does not prove that a script is free of concurrency problems.

To check before compiling, run the commands separately:

```sh
fscript analyze program.fss
fscript compile program.fss --target dotnet --output out/program
```

Compilation does not run this analysis automatically and has no task-sharing flag.

## Try the runnable samples

The repository includes a small provider and four scripts under
[`samples/task-sharing`](https://github.com/MagnusOpera/FScript/tree/main/samples/task-sharing).
From a checkout, build the provider and run analysis:

```sh
dotnet build samples/task-sharing/TaskSharingSample.fsproj -c Release
dotnet run --project src/FScript -- analyze samples/task-sharing/unsafe-shared.fss
dotnet run --project src/FScript -- analyze samples/task-sharing/hidden-state.fss
dotnet run --project src/FScript -- analyze samples/task-sharing/ordered.fss
dotnet run --project src/FScript -- analyze samples/task-sharing/thread-safe.fss
```

The first script reports `FST1001` and `FST1002`; the hidden-state example reports
only `FST1002`. Ordered access and the thread-safe counter produce no warnings.
The provider declares the safe counter **thread-safe** and leaves its purity
contracts empty: increments change state. Its implementation uses a lock; the
declaration itself adds no synchronization.

To execute the safe examples:

```sh
dotnet run --project src/FScript -- samples/task-sharing/ordered.fss
dotnet run --project src/FScript -- --no-result samples/task-sharing/thread-safe.fss
```

Ordered access returns `(1, 2)`. The concurrent safe counter prints `(1, 2)` or
`(2, 1)`, depending on which task reaches it first. The explicit console output
remains visible with `--no-result`.

## What the warnings mean

| Code | What was identified | What remains unknown |
| --- | --- | --- |
| `FST1001` | A host instance may be shared by tasks that can run concurrently, or by a task and its parent. | Whether the instance supports concurrent access. |
| `FST1002` | A host function may be called concurrently. | Whether its implementation or hidden shared state is thread-safe. |

Each warning includes a source location and a related access or capture location.
A warning identifies possible sharing; it does not establish that either access
writes memory or that a race will occur.

### A shared host instance: `FST1001`

Suppose a provider exposes a `Client` type and registers `Host.createClient` and
`Host.fetch`:

```fsharp
let client = Host.createClient ()
let alias = client

let first = Task.spawn (fun () -> Host.fetch client "first")
let second = Task.spawn (fun () -> Host.fetch alias "second")

(Task.await first, Task.await second)
```

Both closures retain the same client. Giving it another binding does not create a
second instance. Unless its type is declared shareable, analysis can report:

```text
warning FST1001: Host value 'Client' may be shared by concurrent tasks; its thread safety is unknown.
```

The check follows aliases through known helper functions, closure captures,
records, collections, and pattern bindings. Access from the parent while a task
may still be running can also warn.

### Hidden function state: `FST1002`

An instance argument is not required for a concurrency problem:

```fsharp
let first = Task.spawn (fun () -> Host.nextNumber ())
let second = Task.spawn (fun () -> Host.nextNumber ())

(Task.await first, Task.await second)
```

`Host.nextNumber` might use a shared counter or another resource hidden inside the
provider. Without a concurrency contract, the check can report:

```text
warning FST1002: Host function 'Host.nextNumber' may be called by concurrent tasks; its thread safety is unknown.
```

The implementation might already use a lock and be safe. This warning asks for a
review or a trusted provider contract; it does not claim the implementation is broken.
A shared instance and an unknown function can produce both warning codes.

## When waiting establishes an order

A straightforward `Task.await` establishes that one task has finished before the
next operation:

```fsharp
let client = Host.createClient ()
let first = Task.spawn (fun () -> Host.fetch client "first")
let firstResult = Task.await first

let second = Task.spawn (fun () -> Host.fetch client "second")
let secondResult = Task.await second

(firstResult, secondResult)
```

These two uses are ordered, so they do not need a thread-safety guarantee for
concurrent access. Waiting on both tasks **after** spawning both, as in the first
example, does not prevent their work from overlapping.

The check recognizes task-handle aliases and straightforward waits. A conditional
wait does not establish ordering on paths where the wait is skipped. Use waiting
when sequential access is the intended behavior; it also removes the parallelism
between those operations.

## Effects do not imply unsafe sharing

The built-in `Console.writeLine` and `Console.readLine` implementations are known
to be thread-safe and do not trigger a warning merely because tasks call them
concurrently. .NET synchronizes console I/O; see the
[Console documentation](https://learn.microsoft.com/dotnet/api/system.console#thread-safety).
The order of messages or which task receives an input line can still vary.

These functions remain effectful. Thread-safety information does not mark them
`Pure`, allow the compiler to discard their calls, or make output deterministic.
A provider that replaces a console binding does not automatically inherit the
built-in implementation's guarantee.

## Declaring thread safety in a provider

A provider can expose optional public static parameterless methods returning
`IEnumerable<string>` of registered script names:

```csharp
public static IEnumerable<string> FScriptThreadSafeTypes()
    => new[] { "Client" };

public static IEnumerable<string> FScriptThreadSafeFunctions()
    => new[] { "Host.fetch", "Host.nextNumber" };
```

A type contract promises that its instances and supported operations can be
shared across tasks. A function contract covers concurrent invocation, including
initialization and hidden state. The two contracts address different warnings:
marking a function thread-safe does not classify an otherwise unknown captured
instance, and marking an instance type shareable does not classify a function's
hidden global state.

Declare these guarantees only after checking the implementation. FScript trusts
them; it does not insert locks or verify the provider's internals. Unknown binding
names are rejected. Existing SDK registration records do not need to change.

Thread safety is separate from purity. A logger can be thread-safe and still have
observable effects. Existing `PureTypes` contracts imply immutable, shareable
instances; `PureFunctions` contracts classify the function's own behavior without
making every argument type shareable.

## Limits of the check

The analysis follows the FScript code it can see. It cannot inspect arbitrary
.NET implementations for shared caches, callbacks scheduled on other threads, or
aliases hidden behind different FFI bindings.

It also uses approximations: loops receive two symbolic iterations, recursive
expansion stops at an active lambda, and collections can combine several possible
aliases. It does not prove that separate factory calls return separate objects.
This means safe code can warn and unsafe code can pass without a warning.

Review a warning by checking whether the value is genuinely shared, whether the
operations can overlap, and what the provider guarantees. Possible resolutions
include ordered access, a thread-safe implementation, or a concurrency contract
that documents an existing guarantee. Analysis does not change task scheduling.

## Optional checks in an embedding host

A .NET host can request the same check before interpretation:

```fsharp
open FScript.Language
open FScript.Runtime

// registration and source are supplied by the embedding application.
let loaded =
    ScriptHost.loadSourceWithTaskSharingAnalysis
        registration
        PurityContracts.empty
        TaskSharingAnalysis.defaults
        (fun warning -> eprintfn "%s: %s" warning.Code warning.Message)
        source
```

The report callback receives warnings before evaluation starts. The host decides
whether to display them or reject execution by throwing from the callback. File
and parsed-program variants are `loadFileWithTaskSharingAnalysis` and
`loadProgramWithTaskSharingAnalysis`.

For provider-based environments, `TaskSharingContracts.fromProviders` reads the
optional thread-safety companions from provider `MethodInfo` values. Hosts can
also supply `ThreadSafeTypes` and `ThreadSafeFunctions` sets directly in analysis
options. `TaskSharingContracts.withRuntime registration options` adds known runtime
guarantees when calling the low-level analyzer directly; the CLI and analyzed
`ScriptHost` loaders apply these automatically. These APIs do not require a
compiler assembly reference.

Existing `ScriptHost` APIs skip analysis. The check adds no runtime tracking,
synchronization, or task instrumentation to interpreted or compiled programs.
