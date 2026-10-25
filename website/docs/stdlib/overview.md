---
id: stdlib-overview
title: Stdlib Overview
slug: /stdlib/overview
---

Start with lists, options, and maps for data transformations. Add runtime modules when a script needs files, console I/O, serialization, or concurrency.

## Availability

| Surface | Modules or names | Available when |
| --- | --- | --- |
| Core functions and values | `List`, `Option`, `Map`, `String`, `Int`, `Float`, `Bool`, `ignore` | Always, including JavaScript hosts |
| Default .NET runtime externs | `Console`, `Task`, `Fs`, `Json`, `Regex`, `Xml`, `Hash`, `Guid` | The host registers them; the CLI enables them by default |
| Exit status | `Env.setExitCode : int -> unit` | Effectful core operation; never terminates the host |
| Execution metadata | `Env : Environment` | Injected by the CLI/REPL or supplied by an embedding host |

No imports are needed for registered functions. An embedding host may expose a subset of the runtime catalog, and the CLI's `--no-default-externs` disables that catalog without removing core functions. The browser playground supplies its own capabilities; it does not load the .NET runtime.

`ignore` is a core function with type `'a -> unit`: `42 |> ignore` returns `()`. It needs no external provider. `Env` is a host-supplied value, not a function. See [Built-ins and Execution Metadata](./builtins).

## Choose a module

| Task | Module |
| --- | --- |
| Transform, filter, or fold a sequence | [List](./list) |
| Handle optional values and defaults | [Option](./option) |
| Look up and transform keyed values | [Map](./map) |
| Split, join, replace, or search text | [String](./string) |
| Parse and format scalar values | [Int, Float, Bool](./scalars) |
| Read or write console text | [Console](./console) |
| Run concurrent work and await results | [Task](./task) |
| Read, write, and discover files | [Fs](./fs) |
| Decode and encode JSON | [Json](./json) |
| Extract regular-expression capture groups | [Regex](./regex) |
| Query XML element text and attributes | [Xml](./xml) |
| Compute an MD5 digest | [Hash](./hash) |
| Create a GUID string | [Guid](./guid) |

## Reading the signatures

Functions are curried: `Map.add key value map` takes three arguments. Pipelines supply the final argument: `map |> Map.add key value`. Repeated type variables must agree; for example, `'a -> 'a option -> 'a` uses one type throughout a call.

An `option` result distinguishes success (`Some value`) from absence or a supported recoverable failure (`None`). It does not promise that all failures are caught: for example, `List.tail []` fails, and denied filesystem reads raise an error. Each module describes its boundary behavior.

Use the [complete signature index](../reference/stdlib-reference.md) for lookup, [Native Types](../reference/native-types-reference.md) for literals and indexers, and [Stdlib Recipes](./recipes) for small compositions.
