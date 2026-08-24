---
id: stdlib-regex
title: Regex Module
slug: /stdlib/regex
---

The default runtime exposes one regular-expression helper:

- `Regex.matchGroups : string -> string -> string list option`

It returns captured groups, excluding the complete match, or `None` when the input does not match or the expression is invalid.

```fsharp
Regex.matchGroups "^name:(.*)$" "name:Ada"
// Some ["Ada"]
```

The .NET host uses .NET regular expressions. Generated Go uses Go regular expressions and translates non-capturing groups `(?:...)` while preserving the returned capture ordering. Go-unsupported constructs such as lookbehind return `None`.
