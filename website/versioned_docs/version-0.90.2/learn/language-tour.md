---
id: language-tour
title: Language Tour
slug: /learn/language-tour
---

This is a fast pass over the language.

```fsharp
type Result =
  | Ok of string
  | Error of string

let classify score =
  if score >= 10 then
    Ok "pass"
  else
    Error "fail"

let message =
  match classify 12 with
  | Ok text -> $"Result: {text}"
  | Error text -> $"Result: {text}"

Console.writeLine message
```

What this introduces:

- `type` for union declarations,
- `if/else` as an expression,
- `match` for branching on union cases,
- string interpolation with `$"..."`.

The function `classify` returns a union value; the `match` turns it into a string. Both branches produce the same type, so the whole expression has one inferred type.

Next: start [Values and Bindings](../language/values-and-bindings). The Language chapters build from expressions and functions to data types, matching, imports, and concurrency.
