---
id: native-types-reference
title: Native Types
slug: /reference/native-types
---

This page describes the built-in data shapes that are part of the language itself rather than stdlib modules.

## Tasks

Type form: `'a task`

| Form | Signature | Description |
| --- | --- | --- |
| `Task.spawn (fun () -> expr)` | `(unit -> 'a) -> 'a task` | Starts concurrent work and returns a task handle. |
| `Task.await taskValue` | `'a task -> 'a` | Waits for completion and returns the task result. |

```fsharp
let pending = Task.spawn (fun () -> 40 + 2)
let answer = Task.await pending
```

Tasks are opaque values. They do not expose indexers or fields.
The `'a task` type is native to the language, while `Task.spawn` and `Task.await` come from the default runtime extern registry and may be omitted by an embedding host.

## Lists

Type form: `'a list`

| Form | Signature | Description |
| --- | --- | --- |
| `[first..last]` | `int list` | Inclusive integer range; steps by +1 or -1 toward the final bound. |
| `[]` | `'a list` | Empty list literal. |
| `head :: tail` | `'a -> 'a list -> 'a list` | Prepends one item to a list. |
| `left @ right` | `'a list -> 'a list -> 'a list` | Concatenates two lists. |
| `values[index]` | `'a list -> int -> 'a option` | Zero-based optional indexer. Negative or out-of-range indices return `None`. |

```fsharp
let xs = [10; 20; 30]
let maybeSecond = xs[1]
```

## Maps

Type forms: `'v map` for string keys, or `'v map['key]` for explicit key types.

Map keys may be `string`, `int`, `bool`, or a union whose cases all have no payload. Use `value map[key]` for explicit key annotations; `value map` remains the string-keyed shorthand.

| Form | Signature | Description |
| --- | --- | --- |
| `{}` | `'v map['key]` | Empty map literal. |
| `{ [key] = value }` | `'key -> 'v -> 'v map['key]` | Map literal entry syntax. |
| `values[key]` | `'v map['key] -> 'key -> 'v option` | Optional key lookup. Missing keys return `None`. |
| `{ ..tail }` | `'v map['key] -> 'v map['key]` | Spread syntax used when building or updating a map. |

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
| `let (a, b) = value` | binding syntax | Introduces two bindings from a tuple; this is not a function returning unit. |
| tuple pattern matching | `('a * 'b) -> 'r` | Use `match value with (a, b) -> ...` to decompose tuples. |

Tuples do not have a native indexer. Destructure them instead.

## Records

Type form: `{ Field: t; ... }`

| Form | Signature | Description |
| --- | --- | --- |
| `{ Name = value }` | field-set dependent | Record literal requiring an unambiguous declared record field set. |
| `record.Field` | `{ Field: 'a; ... } -> 'a` | Field access by name. |
| `{ record with Field = value }` | same record type -> same record type | Copy a record and replace existing fields. |
| `{\| Field = value \|}` | structural record | Structural record literal. |
| `{\| record with Field = value \|}` | record-shape dependent | Structural record update; may add fields; existing field types must remain compatible. |

## Host-managed types

A .NET host can register named types with immutable visible fields and an opaque payload. Scripts can read and match visible fields and transport complete values, including as collection values. They cannot construct these types from record literals or use either record-update syntax on them. Equality, formatting, and JSON serialization use visible fields only; JSON decoding cannot restore host-managed state.

These are host-supplied types rather than built-in record declarations. Hosts can also register script-constructible records; those use ordinary record construction and update rules. See [Host-Provided Types](../embedding/host-managed-types) for registration and invocation examples.

## Unions

Type form: `type Shape = | Point | Circle of int`

| Form | Signature | Description |
| --- | --- | --- |
| `Case` | case dependent | Union constructor without payload. |
| `Case value` | payload type -> union type | Union constructor with payload. |
| union pattern matching | `union type -> 'r` | Use `match value with Case x -> ...` to branch on union cases. |

## Scalars

Primitive built-in types:

- `int`: signed 64-bit integer
- `float`: double-precision floating-point number
- `bool`
- `string`
- `unit`

These do not use indexers. For parsing and formatting helpers, see [Int, Float, Bool Modules](../stdlib/scalars) and [String Module](../stdlib/string).

## Numeric meta-type

`numeric` constrains inference to `int` or `float`. It is available in function annotations and inferred editor signatures, with no additional runtime value shape.

| Operation | Operand types | Result |
| --- | --- | --- |
| `+`, `-`, `*`, `/` | Two `int` values or two `float` values | Same concrete type as the operands |
| `<`, `>`, `<=`, `>=` | Two `int` values or two `float` values | `bool` |
| `%` | Two `int` values | `int` |

Mixed numeric types and nonnumeric operands are rejected. `+` does not concatenate strings, lists, or maps. Use `String.concat` for strings and `@` for lists.

See [Type System](../language/type-system) for the type hierarchy and specialization examples.

## Function values and type tokens

Functions use `argument -> result`; arrows associate to the right. A function is applied with whitespace, passed as a value, or captured in a closure. `unit -> 'a` functions are called with `()`. Function values have no list/map-style indexer.

`typeof Name` produces a runtime `type` token for a known declared type. Tokens are intended for host functions such as `Json.deserialize`; they do not expose CLR reflection or construct instances. `nameof binding` produces a `string` after checking that the binding exists. See [Syntax and Operators](./syntax-reference.md) for annotations, expressions, and precedence.
