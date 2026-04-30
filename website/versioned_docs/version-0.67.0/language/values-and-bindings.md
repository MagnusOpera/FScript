---
id: values-and-bindings
title: Values and Bindings
slug: /language/values-and-bindings
---

FScript uses `let` bindings and immutable values.

## Primitive values

```fsharp
let project = "FScript"
let retries = 3
let ratio = 0.8
let enabled = true
let nothing = ()
```

## Expressions

Everything computes to a value.

```fsharp
let answer = (40 + 2) * 2
```

## Immutability by default

Bindings cannot be reassigned. Model state transitions by creating new values.
