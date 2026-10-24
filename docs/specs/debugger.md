# Debugger

FScript provides a .NET debugger through the CLI and VS Code extension. The runtime
session API is independent of the Debug Adapter Protocol (DAP). Fable execution
does not expose debugging APIs.

## Launch

The extension starts the CLI selected by the resource-scoped `fscript.cli.path`
setting with `--debug-adapter`; an empty setting uses `fscript` from PATH. The
setting accepts one executable path or command name, or a managed CLI DLL run
through `dotnet`. Relative file paths resolve against the owning workspace
folder, or script directory for standalone files. Capability probing and Run
Current Script use the same selection. Workspace-defined CLI settings are
restricted to trusted workspaces. The adapter implements
DAP over stdio and requests an integrated-terminal launch of the same CLI in its
internal script-host mode. An authenticated loopback channel carries debugger
control; script input, output, and native-library output stay in the terminal.
`fscript --debug-capabilities` reports the supported launch protocol version.

The program's owning workspace folder determines the selected configuration.
Selected manifests use the same workspace-root fallback as editor analysis.
Selecting no configuration explicitly disables CLI manifest discovery. Debugging
requires workspace trust and saved FScript files. Source and provider changes
require a new session. No attach mode or provider hot reload is supported.

## Execution

Evaluation completes ordinary expressions eagerly and builds explicit continuations
when suspension is needed, including exception scopes. Synchronous
execution and scheduled tasks share the evaluator. A suspended task retains its
continuation and releases its scheduler worker; independent tasks remain runnable,
even on a scheduler limited to one concurrent worker. Task identities are logical
session identities, unrelated to managed thread IDs.

The main script runs on its process main thread. Synchronous host calls execute
on that thread, or on a task-owned host thread for background tasks. This preserves
native callback affinity. A callback breakpoint holds its native caller until
resumption, without occupying a shared scheduler worker. A window whose event
callback is paused may stop responding. Host implementation code is atomic and
cannot be interrupted by the debugger.

Task awaits suspend the continuation. Background failures preserve the existing
script-wide failure behavior after their exception stop is resumed. Completion
without awaiting every spawned task remains an error.

## Stops and inspection

Breakpoints bind executable expressions by source file, line, and optional column.
Blank/nonexecutable locations remain unverified. Semantic stepping distinguishes
binding initializers, calls, pipeline stages, branch/match decisions and bodies,
and loop iterations. Calls and pipeline stages also expose return boundaries,
with `$result` available in Locals. Name lookups, parentheses, and partial function
application are skipped by stepping. Explicit breakpoints still take precedence.

Step In enters FScript callbacks, including those invoked by standard-library and
native functions. Step Over skips deeper frames unless a breakpoint is reached.
Step Out resumes to a caller boundary; from the main frame it runs to completion.
Pause takes effect at the next FScript expression boundary.

Each task has a logical stack with parameters, lexical locals, and captures.
Frame and variable references expire when their task resumes or exits. Resuming
one task does not invalidate another paused task's references. Collections expand
in pages (100 entries by default, at most 1,000 per request). Values are read-only.
Host values reveal only descriptor-backed visible fields; payloads and native
objects are never reflected or serialized into inspection responses.

Safe watches accept names, imported qualified names, visible field access,
parentheses, and primitive literals. Function calls, pipelines, declarations, and
other expressions are rejected without execution. Watches do not change live
bindings, the program counter, or the stack.

## Protocol and lifecycle

DAP tasks are threads. Stops report `allThreadsStopped: false`. Execution requests
honor `singleThread`: true targets only the selected task; false or omitted also
resumes other paused tasks. Already-running tasks remain free to run in either
case. Uncaught exception stops are enabled by default and can be disabled through
exception breakpoint settings.

The adapter supports initialize, launch, configurationDone, setBreakpoints,
breakpointLocations, threads, stackTrace, scopes, variables, evaluate, continue,
next, stepIn, stepOut, pause, setExceptionBreakpoints, exceptionInfo, disconnect,
and terminate. Unsupported features are not advertised. Conditional breakpoints, hit counts,
and logpoints are rejected explicitly.

Disconnect and terminate cancel the launched session and release pause gates.
Cooperative unwinding permits host cleanup; a host call that does not return is
terminated with the launched process after a two-second grace period. Embedders
own their process and must handle noncooperative native code themselves.

## Public inspection structures

All structures below belong to `FScript.Runtime` and contain display data or
session-scoped references. None contains an evaluator value or opaque payload.

| Structure | Fields | Purpose |
| --- | --- | --- |
| `DebugLocation` | `File`, `Line`, `Column`, `EndLine`, `EndColumn` | One-based source range. |
| `DebugBreakpoint` | `Id`, `Verified`, `Location`, `Message` | Result of binding a requested source position. |
| `DebugThread` | `Id`, `Name`, `State` | Live logical task: running, waiting, or paused. |
| `DebugStackFrame` | `Id`, `Name`, `Location` | Paused FScript frame; its reference expires on resumption. |
| `DebugScope` | `Name`, `VariablesReference`, `Expensive` | Parameters, locals, or captures for a frame. |
| `DebugVariable` | `Name`, `Value`, `Type`, `VariablesReference`, `NamedVariables`, `IndexedVariables` | Safe summary and paged expansion handle. |
| `DebugEvent` | `Kind`, `TaskId`, `Reason`, `Message` | Task start/exit, stopped/continued, or session termination notification. |

`DebugSession` owns these identities. `Run` is synchronous and belongs on the
execution/main thread; control and inspection calls run on another thread.
`Resume` accepts `continue`, `in`, `over`, or `out`; its `singleThread` argument
controls whether other paused tasks also resume. `Cancel` releases pending gates,
and disposal cancels the session. Interpreter observer hooks are internal to the
Language/Runtime bridge, keeping raw environments out of the public debugger API.

## Normal execution and performance checks

Ordinary evaluation has no debugger observer. Frame tracking, local-variable
snapshots, stop events, and host invocation lanes run only inside a debug session.
The shared evaluator completes synchronously when possible; it allocates a
continuation only when execution suspends. Normal CLI startup defers adapter
loading until a debugger mode is selected.

`make benchmark-evaluation` measures warmed evaluation without a debugger, keeping
parsing and inference outside the timing. The benchmark README describes how to
compare the same runner against a published engine. CLI startup should be measured
separately with matching managed or native executables.
