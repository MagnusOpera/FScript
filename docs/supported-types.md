# FScript Supported Types Specification

## Purpose
This document specifies the value and type system supported by the current interpreter.

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
- String-keyed map: `'a map`
- Record: structural record types

## Function types
- Functions use curried arrow types:
  - `t1 -> t2`
  - `t1 -> t2 -> t3` is right-associative.
- Function type syntax is currently used in parameter annotations.

## Named types and declarations
- Top-level record declarations:
  - `type Name = { ... }`
  - `type rec Name = { ... }`
- Recursive self-reference requires `type rec`.
- Mutual recursive declarations are not supported.

## Type inference model
- Hindley–Milner style inference is used.
- Let-polymorphism is supported.
- Inferred record types are structural.
- Some operations require concrete constraints:
  - field access requires the target to be inferred/constrained as a record type.

## Runtime value representation (high level)
- `VUnit`, `VInt`, `VFloat`, `VBool`, `VString`
- `VList`, `VTuple`
- `VRecord`
- `VStringMap`
- `VOption`
- `VClosure`
- `VExternal`
- `VTypeToken`

## Reflection
- `typeof Name` yields a type token.
- Type tokens are used by host externs (for example JSON/XML decoding).

## Current type-system limitations
- No user-declared union/discriminated union types.
- No mutual recursive type groups.
- No higher-kinded type features.
- No explicit return type annotations.
