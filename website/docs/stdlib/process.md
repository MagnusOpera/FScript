---
id: stdlib-process
title: Process Module
slug: /stdlib/process
---

`Process.exec` launches a program directly and returns a `ProcessResult task`. Hosted interpretation requires both **Process** and **Threading**. CLI execution and compiled programs support it without interpreter permission checks. JavaScript/Fable and the standalone LIR interpreter have no built-in process implementation.

Child programs have the host's operating-system privileges, including filesystem, network, and environment access. `RootDirectory` and denied paths do not sandbox child code. Granting `IO`, `Threading`, or `Native` alone does not expose `Process.exec`.

```fscript
let pending = Process.exec "dotnet" ["--version"] Process.defaults
match Task.await pending with
| ProcessResult.Exited result -> Console.writeLine result.stdout
| ProcessResult.Failed error -> Console.writeLine error.message
```

## API

```fscript
type ProcessOptions = {
  workingDirectory: string option
  environment: Map<string, string>
  stdin: string option
  timeoutMilliseconds: int option
  maxOutputBytes: int
}
type ProcessResult =
  | Exited of { exitCode: int; stdout: string; stderr: string }
  | Failed of { message: string; stdout: string; stderr: string }
```

| Name | Signature | Host permissions |
| --- | --- | --- |
| `Process.defaults` | `ProcessOptions` | None |
| `Process.exec` | `string -> string list -> ProcessOptions -> ProcessResult task` | Process, Threading |

`Process.defaults` and both types are available without effect permissions. Defaults are no directory override, an empty environment map, no stdin text, no timeout, and **1,048,576 bytes per output stream**.

```fscript
let options = {
  Process.defaults with
    stdin = Some "hello\n"
    timeoutMilliseconds = Some 5000
    maxOutputBytes = 65536
}
```

Executable arguments are passed individually, without shell expansion or shell quoting. Launch a shell explicitly if shell syntax is needed. Bare executable names use platform lookup. Explicit relative executable paths resolve against the effective working directory. The default directory is the interpreter host's `RootDirectory`, or the current directory in compiled programs. Directory overrides may be anywhere; relative overrides resolve against that default directory.

The child inherits the host environment with supplied entries overridden, without changing the parent's environment. Environment names must be nonempty and contain neither `=` nor NUL. Executables, arguments, directory paths, and environment values cannot contain NUL.

Standard input is UTF-8 without a BOM and closes after writing; `None` closes it immediately. A child that closes its input early is allowed. Standard output and error drain concurrently and decode as UTF-8 with replacement for invalid sequences. Line endings are preserved. Each stream retains at most `maxOutputBytes` raw bytes, including on failure; truncation may end with a replacement character.

`Exited` includes nonzero exit codes. `Failed` contains partial stdout/stderr and a message beginning with `InvalidOptions:`, `StartFailed:`, `TimedOut:`, `OutputLimitExceeded:`, or `IOFailed:`. Timeout and output limits must be positive integers no greater than 2,147,483,647. Timeout begins after successful launch and covers process exit and collection of output, including pipes inherited by descendants.

Timeout, output overflow, and evaluator cancellation terminate the child and close local pipes. Process-tree termination is attempted on supporting runtimes, with direct-child termination as fallback; descendant containment is not guaranteed. Permission denial and evaluator cancellation remain runtime errors. Other operational failures are union values and do not fail the task scope.

Process tasks start when called and participate in the existing task scope: even unawaited calls are joined before the host returns, including after foreground failure. No timeout means a nonterminating child can keep the host call running indefinitely. There is no terminal, streaming, process-handle, or explicit cancellation API.
