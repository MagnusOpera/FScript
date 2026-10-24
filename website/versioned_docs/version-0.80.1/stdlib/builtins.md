---
id: stdlib-builtins
title: Built-ins
slug: /stdlib/builtins
---

This page covers the top-level built-ins that are always in scope even though they are not module members.
For host console I/O, see the separate [Console module](./console).

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

- `Env.ScriptName` is `Some "path/to/script.fss"` for file execution.
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

Use `ignore` when an expression returns a value you intentionally do not need.

```fsharp
String.indexOf "x" "text" |> ignore
```
