---
id: stdlib-fscript
title: FScript Module
slug: /stdlib/fscript
---

`FScript.exec` runs a `.fss` file asynchronously in a fresh interpreter environment and returns an `FScriptResult task`. The caller requires **Threading**. Loading the child source and its imports does **not** require IO: execution itself authorizes these source reads within the sandbox.

```fscript
let options = {
  FScript.defaults with
    rootDirectory = Some "jobs"
    permissions = Some []
    timeoutMilliseconds = Some 5000
}
match FScript.exec "jobs/child.fss" ["argument"] options |> Task.await with
| FScriptResult.Completed result -> Console.writeLine result.value
| FScriptResult.Failed error -> Console.writeLine error.message
```

## API

```fscript
type FScriptOptions = {
  rootDirectory: string option
  permissions: string list option
  timeoutMilliseconds: int option
}
type FScriptResult =
  | Completed of { value: string; exitCode: int }
  | Failed of { message: string }
```

| Name | Signature | Host permissions |
| --- | --- | --- |
| `FScript.defaults` | `FScriptOptions` | None |
| `FScript.exec` | `string -> string list -> FScriptOptions -> FScriptResult task` | Threading |

Defaults inherit the parent root and active permissions, with no timeout. The data types and defaults need no capabilities. String arguments become the child's `Env.Arguments`; `Env.ScriptName` contains its resolved file path. Parent arguments and variables are not inherited.

The child's final expression must be a string. Serialization and deserialization belong to the callee and caller; there is no implicit formatting or task unwrapping. `Completed` includes the child's exit code, including nonzero codes, without changing the parent's exit code. Non-string final values produce `ResultTypeMismatch:` failures.

## Roots and permissions

Resolve entry paths and root overrides from the **parent root**, not the entry file's directory or the overridden child root. Absolute paths must satisfy the same boundaries. A child root can equal or narrow its parent root. Entry files and all imports must remain inside the effective child root. Imports resolve relative to the importing file, so sibling imports remain possible when the inherited root contains them.

Source paths must end in `.fss`. Traversal, symlink/reparse-point, import-cycle, and denied-path checks remain enforced. Ancestor denied patterns keep their original root-relative meaning for source loading, imports, and every child filesystem operation, including enumeration pruning.

`permissions = None` inherits the **active invocation's** grants; `Some []` grants none. Accepted names are `None`, `Interactive`, `Threading`, `IO`, and `Native`. Unknown names fail. Any requested capability the parent lacks produces `PermissionDenied:`; requests are not silently intersected. The caller needs Threading to launch a child, but the child may receive no Threading and therefore cannot itself start tasks or nested executions.

Children receive a fresh standard Runtime registration, rebuilt for their root and permissions. They inherit no custom functions, providers, or parent locals/exports. Execution does not discover manifests or restore packages. Interpreter bindings, task scopes, and exit codes are separate. Authorized console operations use the normal shared console and may interleave; no global console redirection is performed.

These are interpreter boundaries. Inherited Native and Threading grants permit child programs with host OS access. Native code remains trusted; the nested standard-library registration does not automatically load native providers. Side effects are not rolled back on failure, and memory/OS containment requires host governance.

## Failure and lifetime

`Failed.message` starts with one of `InvalidOptions:`, `PermissionDenied:`, `PathDenied:`, `LoadFailed:`, `ParseFailed:`, `TypeFailed:`, `ExecutionFailed:`, `ResultTypeMismatch:`, `TimedOut:`, or `DepthLimitExceeded:`. Diagnostics include source locations where available. Child runtime/background-task errors become result values; caller permission denial and parent cancellation remain runtime errors.

Calls are eagerly scheduled and tracked. Unawaited child calls are joined before the parent returns, including after parent failure. The child joins all its own tasks before returning a result, and no closures, tasks, host handles, or other child values cross the boundary.

At most **32 child executions** may be nested. Attempting the next level fails before loading source. Ordinary imports do not consume this depth. Positive timeouts must be in `1..2147483647` milliseconds and cover loading, parsing, inference, execution, and task draining. Descendants inherit cancellation and cannot extend an ancestor's timeout. Cancellation is cooperative: uninterruptible host operations, including blocking source I/O, can delay completion, and tracked workers are joined before return.

Both compiled .NET backends call the Runtime library through FFI. An unrestricted compiled caller starts from the current working directory, all capabilities, and no denied paths; options can narrow its first child. Every child executes in the interpreter. Browser/Fable and standalone LIR execution provide no built-in implementation.
