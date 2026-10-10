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

The checker infers `(x: numeric) -> (y: numeric) -> numeric`. Each call specializes the shared numeric type to `int` or `float`:

```fsharp
(add 1 2, add 1.5 2.6) // (3, 4.1), with type int * float
```

`numeric` is an inference meta-type, restricted to `int` and `float`. It has no separate runtime representation. Both operands of `+` must have the same concrete type, and the result has that type too. Mixed calls such as `add 1 2.6`, and nonnumeric calls such as `add "a" "b"`, are type errors. There are no implicit numeric conversions.

The same rule applies to `-`, `*`, `/`, and numeric comparisons. Comparisons return `bool`. Remainder (`%`) accepts only integers.

## Type hierarchy

This tree shows how inference variables can specialize. It describes inference and type families; FScript does not use class inheritance for these types.

```text
'a — unconstrained inference variable
├── numeric — constrained to one numeric type
│   ├── int
│   └── float
├── bool
├── string
├── unit
├── Containers
│   ├── 't list
│   ├── 't option
│   ├── 't map (string keys)
│   └── 't task
├── Structured values
│   ├── tuples: (t1 * t2 * ...)
│   ├── records: { Field: t; ... }
│   └── declared unions
├── Functions: argument -> result
└── Type tokens: typeof DeclaredType
```

An unconstrained variable can resolve to any compatible type. A `numeric` variable can resolve only to `int` or `float`. Neither numeric type implicitly converts to the other. Container element types may themselves be inferred or constrained.

Numeric variables have separate identities: the repeated `numeric` labels in `add` share a type because `x + y` ties them together. Unrelated numeric parameters need not specialize alike. For example, `let pair x y = (x + x, y + y)` accepts `pair 1 2.6`.

For concrete values and operations, see [Native Types](../reference/native-types-reference.md).

## Optional annotations

Use annotations for clarity or constraints:

```fsharp
let scale (x: float) (y: float) : float = x * y
let add x (y: int) = x + y
```

The second function infers `(x: int) -> (y: int) -> int`: the annotation specializes both operands and the result. `add 1.5 2` is rejected.

You can also annotate a parameter or return as `numeric`:

```fsharp
let add (x: numeric) (y: numeric) : numeric = x + y
```

Each annotation starts with its own numeric variable; the addition operation unifies them. The function remains usable with integers or floats on separate calls.

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
