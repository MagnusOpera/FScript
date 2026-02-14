# FScript Supported Types Specification

## Purpose
This document specifies the value and type system used by the interpreter.

## Primitive types
- `unit`
- `int` (`int64` at runtime)
- `float`
- `bool`
- `string`

## Composite/container types
- List: `'a list`
- Tuple: `(t1 * t2 * ...)`
- Option: `'a option`
- Map (string-keyed alias): `'a map`
- Record: structural record types
- Discriminated union: named union types with cases

## Map literals
- Native map literal syntax:
  - empty: `{}`
  - populated: `{ ["a"] = 1; ["b"] = 2 }`
  - int keys: `{ [1] = "one"; [2] = "two" }`
  - spread update: `{ ["a"] = 1; ..tail }`
  - multiline entries are supported in an indented block.
- Keys are bracketed expressions and must have type `string` or `int` (for example `{ [keyExpr] = value }`).
- Record literals and map literals share `{ ... }` braces:
  - map entries use `[expr] = value`
  - record entries use `Field = value`
  - `{}` denotes an empty map.
- Values are inferred and unified to a single value type.
- See `./map-matching-reference.md` for focused map-pattern examples.

## Function types
- Functions use curried arrow types:
  - `t1 -> t2`
  - `t1 -> t2 -> t3` is right-associative
- Function type syntax is available in parameter annotations.
- Parameter annotations support two inline record type forms:
  - structural: `let f (x: {| A: int; B: string |}) = ...`
  - declared-type-by-shape: `let f (x: { A: int; B: string }) = ...`
- `{ ... }` resolves only if exactly one declared record type matches that shape.
- Inline record annotation fields are semicolon-separated in single-line form.

## Named record declarations
- Top-level record declarations:
  - `type Name = { ... }`
  - `type rec Name = { ... }`
- Recursive record references use `type rec`.
- Named record types and matching record shapes unify structurally.

## Discriminated union declarations
- Top-level union declarations are supported:
  - `type Shape = | Point | Circle of int`
  - `type rec Node = | Empty | Branch of (int * Node list)`
- Union case names start with an uppercase identifier.
- A case payload is optional:
  - no payload: `| Point`
  - payload: `| Circle of int`
- Payloads are single-type payloads; tuple payloads are used for multi-value payloads.

## Union constructors and matching
- Union cases are constructors in expression position:
  - `let x = Circle 3`
  - `let p = Point`
- `match` supports union-case patterns:
  - `| Circle r -> ...`
  - `| Point -> ...`
- `option` values (`Some`/`None`) are also represented as union-style constructors and patterns.

## Type inference model
- Hindley–Milner style inference.
- Let-polymorphism.
- Structural typing for record values.
- Field access is typed when the target expression is inferred as a record.

## Runtime value representation (high level)
- `VUnit`, `VInt`, `VFloat`, `VBool`, `VString`
- `VList`, `VTuple`
- `VRecord`
- `VMap`
- `VOption`
- `VUnionCase`, `VUnionCtor`
- `VClosure`
- `VExternal`
- `VTypeToken`

## Reflection
- `typeof Name` yields a type token.
- `nameof identifier` yields the bound identifier name as `string`.
- Type tokens are consumed by host externs (for example JSON/XML decoding).
