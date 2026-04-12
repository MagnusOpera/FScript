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

print message
```

What this introduces:

- `type` for union declarations,
- `if/else` as an expression,
- `match` for exhaustive branching,
- string interpolation with `$"..."`.

Next: continue into the language chapters for each feature in detail.
