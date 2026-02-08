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
- Map values can be built from tuple lists with `Map.ofList`:
  - `Map.ofList [("a", 1); ("b", 2)]`
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
- Parameter annotations also support inline structural record shapes:
  - `let f (x: { Name: string; Zip: int }) = ...`
  - `fun (x: { Id: int; Tags: string list }) -> ...`

## Pattern-matching style
- `match` supports wildcard, literal, tuple, list-cons, option, record, and union-case patterns.
- Record patterns are used in `match` case heads.

## Host integration style
- Language core stays minimal.
- Host capabilities are exposed through explicit extern functions.
- Typed decoding workflows use `typeof Name` tokens with host externs.
- Capability maps can use `nameof identifier` for stable script-side function keys.

## Formatting and layout choices
- `match` case columns align.
- Multiline record type fields align.
- Multiline union case lines align in their block.
- These layout rules keep parser behavior explicit and predictable.
