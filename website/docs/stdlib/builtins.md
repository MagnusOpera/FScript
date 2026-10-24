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
