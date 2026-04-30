---
id: modules-imports-exports
title: Imports
slug: /language/imports-exports
---

Split scripts with `import` and reference imported bindings through an explicit alias.

## Import scripts with an alias

```fsharp
import "shared/math.fss" as Math

let value = Math.double 21
```

Imports are file-relative and use explicit aliases. Imported bindings stay under the alias selected by the importing file, so `Math.double` is available but `double` is not injected into the top-level scope.
