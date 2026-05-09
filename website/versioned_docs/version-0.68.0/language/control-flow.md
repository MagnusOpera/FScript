---
id: control-flow
title: Control Flow
slug: /language/control-flow
---

FScript control flow is expression-based.

## `if / elif / else`

```fsharp
let classify n =
  if n < 0 then
    "negative"
  elif n = 0 then
    "zero"
  else
    "positive"
```

## `for ... in ... do`

```fsharp
for x in [1; 2; 3] do
  print $"value={x}"
```

## `match`

```fsharp
let describe opt =
  match opt with
  | Some value -> $"some:{value}"
  | None -> "none"
```
