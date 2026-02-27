---
id: type-system
title: Type System
slug: /language/type-system
---

FScript uses static type inference.

## Inference first

You often write no type annotations:

```fsharp
let add x y = x + y
```

The compiler infers `int -> int -> int`.

## Optional annotations

Use annotations for clarity or constraints:

```fsharp
let scale (x: float) (y: float) : float = x * y
```

## Function types

Curried function types are right-associative:

- `int -> int -> int`
- equivalent to `int -> (int -> int)`

## Common type forms

- `int`, `float`, `bool`, `string`, `unit`
- `'a list`
- `'a option`
- tuples like `(int * string)`
- record and union types

## Next

Read [Structural vs Named Annotations](./structural-vs-named-annotations) for record annotation styles and guidance.
