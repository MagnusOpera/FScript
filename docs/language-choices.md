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
- Function types use arrow syntax:
  - `int -> string`
  - `int -> int -> int`

## Type declarations
- Top-level record declarations:
  - `type Name = { ... }`
  - `type rec Name = { ... }`
- Recursive records are declared explicitly with `type rec`.

## Type annotation style
- Parameter annotations use parenthesized form:
  - `let f (x: int) = ...`
  - `fun (x: int) -> ...`

## Pattern-matching style
- `match` supports wildcard, literal, tuple, list-cons, option, and record-case patterns.
- Record patterns are used in `match` case heads.

## Host integration style
- Language core stays minimal.
- Host capabilities are exposed through explicit extern functions.
- Typed decoding workflows use `typeof Name` tokens with host externs.

## Formatting and layout choices
- `match` case columns align.
- Multiline record type fields align.
- These layout rules keep parser behavior explicit and predictable.
