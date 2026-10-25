# Static task-sharing analysis

These examples use a small .NET provider with an opaque mutable counter. They
show what `fscript analyze` reports without executing the script, and how ordering
or a thread-safety guarantee changes the result.

Run all commands from the repository root. Build the provider first:

```sh
dotnet build samples/task-sharing/TaskSharingSample.fsproj -c Release
```

The adjacent `fscript.toml` loads that provider automatically. Use the CLI from
this checkout so it includes the analysis command:

```sh
dotnet run --project src/FScript -- analyze samples/task-sharing/unsafe-shared.fss
dotnet run --project src/FScript -- analyze samples/task-sharing/hidden-state.fss
dotnet run --project src/FScript -- analyze samples/task-sharing/ordered.fss
dotnet run --project src/FScript -- analyze samples/task-sharing/thread-safe.fss
```

With a CLI version supporting analysis installed, replace
`dotnet run --project src/FScript --` with `fscript`.

| Script | Expected analysis |
| --- | --- |
| `unsafe-shared.fss` | `FST1001` for the shared `Counter`, plus `FST1002` for `Host.next`. |
| `hidden-state.fss` | `FST1002` for `Host.nextGlobal`; no host instance is passed as an argument. |
| `ordered.fss` | No warnings: the first task is awaited before the second is started. |
| `thread-safe.fss` | No warnings: the shared `SafeCounter` and `Host.nextSafe` have trusted thread-safety contracts. Built-in console output is also classified thread-safe. |

All four analysis commands return status `0`, including the examples that warn.
They produce no script output. Syntax, type, and environment errors still return
a nonzero status. Unsafe operations are deliberately unsynchronized; analyze
those examples rather than relying on a racy execution to demonstrate the problem.

## Example diagnostics

An excerpt from `unsafe-shared.fss`, with directory prefixes omitted:

```text
unsafe-shared.fss(5,14): warning FST1001: Host value 'Counter' may be shared by concurrent tasks; its thread safety is unknown.
  Related task access or capture: unsafe-shared.fss(4,13)
unsafe-shared.fss(5,36): warning FST1002: Host function 'Host.next' may be called by concurrent tasks; its thread safety is unknown.
  Related task access or capture: unsafe-shared.fss(4,35)
```

Additional pairs of captures and accesses can report the same shared value.
The hidden-state example reports:

```text
hidden-state.fss(3,36): warning FST1002: Host function 'Host.nextGlobal' may be called by concurrent tasks; its thread safety is unknown.
  Related task access or capture: hidden-state.fss(2,35)
```

## Run the safe examples

```sh
dotnet run --project src/FScript -- samples/task-sharing/ordered.fss
dotnet run --project src/FScript -- --no-result samples/task-sharing/thread-safe.fss
```

The ordered example returns `(1, 2)`. The thread-safe example writes `(1, 2)` or
`(2, 1)`: both increments occur, but either task can acquire the lock first.
`--no-result` suppresses the CLI's final-value display, leaving the explicit
`Console.writeLine` visible.

`Provider.fs` declares `SafeCounter` and `Host.nextSafe` thread-safe and leaves
both purity lists empty. The counter changes state, so it is **not pure**. No
locks are inserted by FScript; the provider implements synchronization itself.
The unsynchronized counter has no such guarantee. Counter factories are declared
safe to call concurrently, without claiming purity or a freshness contract.

Warnings identify potential sharing, not proven data races. No warning is not a
proof of safety. See the [analysis specification](../../docs/specs/static-task-sharing-analysis.md)
for the model and its limits.
