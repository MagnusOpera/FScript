---
id: stdlib-overview
title: Stdlib Overview
slug: /stdlib/overview
---

FScript ships with a built-in standard library.

It is loaded automatically and available in every script.

## Modules

- `List`
- `Option`
- `Map`
- `String`
- `Int`
- `Float`
- `Bool`

## Core rules

- Functions are curried.
- Pipe-friendly usage is preferred.
- Map keys are always `string` (map indexer type is fixed to string).
- Many parsing/indexing operations return `option` instead of throwing.

## Built-in types and values

FScript also provides a small set of always-available built-in types and values alongside the module functions.

### `type Environment`

```fsharp
type Environment =
  { ScriptName: string option
    Arguments: string list }
```

This is the shape of the CLI/REPL environment value exposed as `Env`.

### `let Env : Environment`

`Env` is injected by the CLI host and REPL:

- `Env.ScriptName` is `Some "..."` when running a script file.
- `Env.ScriptName` is `None` in stdin mode and the REPL.
- `Env.Arguments` contains the script arguments passed after `--`.

```fsharp
match Env.Arguments with
| name :: _ -> print $"hello {name}"
| [] -> print "hello"
```

### `type FsKind`

```fsharp
type FsKind =
  | File of string
  | Directory of string
  | Missing
```

`Fs.kind` returns this union so scripts can pattern match on filesystem lookups without separate boolean probes.

Continue with the module pages for full reference and examples.
