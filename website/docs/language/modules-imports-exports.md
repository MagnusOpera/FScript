---
id: modules-imports-exports
title: Imports and Exports
slug: /language/imports-exports
---

Split scripts with `import` and expose host-callable functions with `[<export>]`.

FScript also ships with built-in modules such as `Task`, `List`, `Option`, `Map`, and `String`.
These are always available and do not need `import`.

## Built-in modules

```fsharp
let pending = Task.spawn (fun _ -> 42)
let doubled = [1; 2; 3] |> List.map (fun n -> n * 2)
```

Use `import` only for your own script files.

## Import

```fsharp
import "shared/math.fss" as Math

let value = Math.double 21
```

Imports are file-relative and use explicit aliases.

## Export

```fsharp
[<export>]
let greet name = $"hello {name}"
```

Host applications can invoke exported functions through runtime APIs.
