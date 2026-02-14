# FScript Language Choices

## Purpose
This document describes core language choices and conventions used in FScript.

## Binding and block style
- Local and nested bindings use layout blocks.
- `let` expressions are indentation-based and compose naturally in blocks.

Example:
```fsharp
let x =
    let y = 1
    y + 1
```

## Type syntax conventions
- Container types use postfix syntax:
  - `'a list`
  - `'a option`
  - `'a map`
- Map values use native literals:
  - `{ ["a"] = 1; ["b"] = 2 }`
  - `{ [1] = "one"; [2] = "two" }`
- Map update/merge is available directly in literals:
  - `{ ["a"] = 1; ..tail }`
- Function types use arrow syntax:
  - `int -> string`
  - `int -> int -> int`

## Type declarations
- Top-level record declarations:
  - `type Name = { ... }`
  - `type rec Name = { ... }`
- Top-level discriminated union declarations:
  - `type Shape = | Point | Circle of int`
  - `type rec Tree = | Empty | Node of (int * Tree list)`
- Recursive records are declared explicitly with `type rec`.
- Recursive unions are declared explicitly with `type rec`.

## Type annotation style
- Parameter annotations use parenthesized form:
  - `let f (x: int) = ...`
  - `fun (x: int) -> ...`
- Parameter annotations support two inline record forms:
  - structural: `let f (x: {| Name: string; Zip: int |}) = ...`
  - declared-type-by-shape: `let f (x: { Name: string; Zip: int }) = ...`

## Pattern-matching style
- `match` supports wildcard, literal, tuple, list-cons, option, record, and union-case patterns.
- Record patterns are used in `match` case heads.

## Host integration style
- Language core stays minimal.
- Host capabilities are exposed through explicit extern functions.
- Typed decoding workflows use `typeof Name` tokens with host externs.
- Capability maps can use `nameof identifier` for stable script-side function keys.
- Capability maps use string keys in map literals (`[expr]` where `expr : string`).
- Maps also support integer keys (`[expr]` where `expr : int`) for numeric indexing scenarios.

## Formatting and layout choices
- `match` case columns align.
- Multiline record type fields align.
- Multiline union case lines align in their block.
- These layout rules keep parser behavior explicit and predictable.
