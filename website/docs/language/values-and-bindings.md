---
id: values-and-bindings
title: Values and Bindings
slug: /language/values-and-bindings
---

A script computes values and gives them names with `let`. This chapter introduces the building blocks used by every later example. Run snippets with the CLI or in the browser playground; console examples need a host that supplies `Console.writeLine`.

## Primitive values

```fsharp
let project = "FScript" // string
let retries = 3 // signed 64-bit int
let ratio = 0.8 // float
let enabled = true // bool
let nothing = () // unit
```

The type checker infers each type. Use integer or float operands consistently: `3 + 2` and `3.0 + 2.0` work, while `3 + 2.0` is a type error.

## Expressions and blocks

An expression produces a value. Parentheses group expressions; indentation groups a block, whose final expression supplies its result.

```fsharp
let answer = (40 + 2) * 2
let total =
  let subtotal = 29 * 12
  let handling = 5
  subtotal + handling

Console.writeLine $"total={total}"
```

`subtotal` and `handling` are local to the block. `total` is available afterward. The string `$"total={total}"` inserts an expression's value; `//` starts a line comment.

## Immutable bindings

Bindings cannot be assigned a new value. Name the next state instead:

```fsharp
let original = 40
let updated = original + 2
```

This leaves `original` equal to `40`. Records and collections also use immutable updates; see [Collections](./collections).

## Unit and effects

`()` is the unit value: it carries no data. Effecting functions such as `Console.writeLine` return unit. Use the core function `ignore` when a computed result is intentionally discarded:

```fsharp
42 |> ignore
```

The argument is evaluated before being discarded. Use a named binding when the result will be needed later.

Next: [Functions](./functions).
