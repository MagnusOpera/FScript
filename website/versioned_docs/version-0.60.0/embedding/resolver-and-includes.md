---
id: resolver-and-includes
title: Resolver and Includes
slug: /embedding/resolver-and-includes
---

When scripts use `import`, you can choose how included source is resolved.

## Resolver-backed loading API

Use `ScriptHost.loadSourceWithIncludes` when the entry script is not read directly from disk and imports must be resolved by the host:

```fsharp
ScriptHost.loadSourceWithIncludes
    externs
    rootDirectory
    entryFile
    entrySource
    resolveImportedSource
```

`resolveImportedSource` has type:

```fsharp
string -> string option
```

It receives a normalized full path and should return source text (`Some`) or missing (`None`).

## Resolver strategies

- in-memory map (tests, cached scripts),
- database-backed content,
- object storage,
- controlled virtual filesystem.

## Minimal resolver example

```fsharp
let sources =
  Map [
    "/virtual/shared/common.fss", "let inc x = x + 1"
  ]

let resolver path =
  sources |> Map.tryFind path
```

## Guidance

- Keep resolver deterministic: same path should produce same source for one run.
- Normalize and constrain import roots to avoid path escape behavior.
- Treat missing includes as normal host errors and surface clear diagnostics.
