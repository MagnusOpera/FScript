---
id: modules-imports-exports
title: Imports and Exports
slug: /language/imports-exports
---

Split scripts with `import` and expose host-callable functions with `[<export>]`.

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
