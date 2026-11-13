---
id: modules-imports-exports
title: Imports and Exports
slug: /language/imports-exports
---

Use imports to split script code into files. Use exports to define the entry points an embedding application may call. These solve different problems: imported bindings do not need an export modifier.

## Put helpers in another file

Create `shared/math.fss`:

```fsharp
let double value = value * 2
```

Create `main.fss` alongside the `shared` directory:

```fsharp
import "shared/math.fss" as Math

let answer = Math.double 21
Console.writeLine $"{answer}"
```

Run `fscript main.fss`. The alias is explicit: `Math.double` is available, while `double` is not added to the importing file's top-level scope. Imported types use the same alias, such as `Common.Project`.

## Import rules

- Place imports before types, bindings, and expressions.
- Import `.fss` files; paths resolve relative to the importing file.
- Resolved paths must stay inside the host's import root.
- Imports load recursively, deduplicate repeated files, and reject cycles.
- Diagnostics include the imported source's filename and location.

The CLI supports imports in file mode. Stdin and REPL submissions do not resolve file imports. Embedding hosts can provide a resolver for memory, database, or file-backed sources; see [Resolver and Includes](../embedding/resolver-and-includes).

## Export an application contract

```fsharp
export let answer = 42

export let quote (seats: int) = seats * 29
```

`ScriptHost` and the JavaScript host facade list/read/invoke explicitly exported top-level bindings. Ordinary helpers remain usable inside the script and through script imports. `export let` is case-sensitive and cannot mark a local binding, type declaration, or destructuring binding.

`export` is contextual rather than reserved. These are both valid:

```fsharp
let export = 42
export let identity export = export
```

Only a top-level `export` immediately followed by `let` changes visibility. The exported name can also be `export`, as in `export let export value = value`. `export let rec ... and ...` exports every function in the recursive group. There is no general script attribute syntax.

Use [Real-World Embedding](../embedding/real-world-embedding) for .NET invocation or [JavaScript Hosts](../embedding/fable-javascript) for browser/Node invocation.

Next: [Tasks and Concurrency](./tasks-and-concurrency).
