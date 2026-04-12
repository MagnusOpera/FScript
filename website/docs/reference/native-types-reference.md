---
id: native-types-reference
title: Native Types
slug: /reference/native-types
---

This page describes the built-in data shapes that are part of the language itself rather than stdlib modules.

## Lists

Type form: `'a list`

| Form | Signature | Description |
| --- | --- | --- |
| `[]` | `'a list` | Empty list literal. |
| `head :: tail` | `'a -> 'a list -> 'a list` | Prepends one item to a list. |
| `left @ right` | `'a list -> 'a list -> 'a list` | Concatenates two lists. |
| `values[index]` | `'a list -> int -> 'a option` | Zero-based optional indexer. Negative or out-of-range indices return `None`. |

```fsharp
let xs = [10; 20; 30]
let maybeSecond = xs[1]
```

## Maps

Type form: `'v map`

Map keys are always `string`.

| Form | Signature | Description |
| --- | --- | --- |
| `{}` | `'v map` | Empty map literal. |
| `{ ["key"] = value }` | `string -> 'v -> 'v map` | Map literal entry syntax. |
| `values[key]` | `'v map -> string -> 'v option` | Optional key lookup. Missing keys return `None`. |
| `{ ..tail }` | `'v map -> 'v map` | Spread syntax used when building or updating a map. |

```fsharp
let scores = { ["math"] = 18; ["english"] = 16 }
let maybeMath = scores["math"]
```

## Options

Type form: `'a option`

| Form | Signature | Description |
| --- | --- | --- |
| `Some value` | `'a -> 'a option` | Wraps a present value. |
| `None` | `'a option` | Represents the absence of a value. |

Use pattern matching or `Option.*` helpers to consume options.

## Tuples

Type form: `(t1 * t2 * ...)`

| Form | Signature | Description |
| --- | --- | --- |
| `(a, b)` | `'a * 'b` | Tuple literal. |
| `let (a, b) = value` | `('a * 'b) -> unit` | Tuple destructuring in bindings. |
| tuple pattern matching | `('a * 'b) -> 'r` | Use `match value with (a, b) -> ...` to decompose tuples. |

Tuples do not have a native indexer. Destructure them instead.

## Records

Type form: `{ Field: t; ... }`

| Form | Signature | Description |
| --- | --- | --- |
| `{ Name = value }` | field-set dependent | Record literal. |
| `record.Field` | `{ Field: 'a; ... } -> 'a` | Field access by name. |
| `{ record with Field = value }` | same record type -> same record type | Record update syntax. |

## Unions

Type form: `type Shape = | Point | Circle of int`

| Form | Signature | Description |
| --- | --- | --- |
| `Case` | case dependent | Union constructor without payload. |
| `Case value` | payload type -> union type | Union constructor with payload. |
| union pattern matching | `union type -> 'r` | Use `match value with Case x -> ...` to branch on union cases. |

## Scalars

Primitive built-in types:

- `int`
- `float`
- `bool`
- `string`
- `unit`

These do not use indexers. For parsing and formatting helpers, see [Int, Float, Bool Modules](../stdlib/scalars) and [String Module](../stdlib/string).
