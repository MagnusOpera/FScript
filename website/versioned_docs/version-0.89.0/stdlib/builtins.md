---
id: stdlib-builtins
title: Built-ins and Execution Metadata
slug: /stdlib/builtins
---

`ignore` belongs to the core language library. `Env` is execution metadata supplied by the CLI/REPL; embedding hosts decide whether to inject it. The `Environment` and `FsKind` type declarations are built into the language. For host console I/O, see the [Console module](./console).

## Values and functions

| Name | Signature | Description |
| --- | --- | --- |
| `Env` | `Environment` | Host-injected execution metadata for CLI file runs, stdin runs, and the REPL. |
| `ignore` | `'a -> unit` | Discards a value when only the side effect of an expression matters. |

## `type Environment`

```fsharp
type Environment =
  { ScriptName: string option
    Arguments: string list }
```

The host exposes `Env` with this shape:

- `Env.ScriptName` is `Some "script.fss"` for file execution.
- `Env.ScriptName` is `None` for stdin execution and the REPL.
- `Env.Arguments` contains arguments passed after `--`.

```fsharp
match Env.Arguments with
| name :: _ -> Console.writeLine $"hello {name}"
| [] -> Console.writeLine "hello"
```

## `type FsKind`

```fsharp
type FsKind =
  | File of string
  | Directory of string
  | Missing
```

`Fs.kind` returns `FsKind`, which lets scripts branch on filesystem state with pattern matching:

```fsharp
match Fs.kind "samples" with
| Directory path -> Console.writeLine $"dir: {path}"
| File path -> Console.writeLine $"file: {path}"
| Missing -> Console.writeLine "missing"
```

## `ignore : 'a -> unit`

Use `ignore` when an expression returns a value you intentionally do not need. It evaluates its argument and returns `()`. It is a normal, always-available core function, not special syntax or a provider-supplied capability.

```fsharp
42 |> ignore
String.indexOf "x" "text" |> ignore
```

## Env.setExitCode

`Env.setExitCode : int -> unit` is an always-available core operation, including in embedding and JavaScript. It coexists with host-provided `Env.ScriptName` and `Env.Arguments` without adding a field to the `Environment` record. The qualified operation is reserved even when an `Env` value is in scope.

On .NET and compiled targets, the code must be in the portable shell range `0..255`; other values fail with an evaluation error. It selects the eventual shell status and returns unit normally. It never calls process exit, cancels tasks, or changes a host process's exit status during embedded evaluation. The default is `0`, repeated calls use the last completed write, and unhandled execution failures still produce failure rather than a selected success code.

This operation is effectful and is not `Pure`. Its writes survive pruning and common-subexpression elimination. Writes from tasks share the execution status and are synchronized, but their order is scheduling-dependent. For deterministic status, await tasks and set it afterward in the main flow. Existing task lifetime and failure rules are preserved.

```fsharp
Env.setExitCode 7
42 // still evaluated; shell status is 7
```

Result printing is independent of this status. `--no-result` suppresses automatic display in the CLI and can be baked into compiled applications.


Fable/JavaScript deliberately treats this operation as a no-op (including shell-range validation). Arguments still evaluate normally, returned values remain unchanged, and the website displays the returned value as before.
