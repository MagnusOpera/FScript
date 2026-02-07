# Function Parameter Annotations Specification

## Purpose
This document specifies optional type annotations for function parameters in FScript.

## Supported forms

### Let-bound functions
- Unannotated:
  - `let f x = x`
- Annotated parameter:
  - `let f (x: int) = x + 1`
- Mixed parameters:
  - `let f (x: int) y = y`

### Lambdas
- Unannotated:
  - `fun x -> x`
- Annotated:
  - `fun (x: int) -> x + 1`

## Syntax rule
- Annotated parameters must be parenthesized:
  - valid: `(x: T)`
  - invalid: `x: T`
- Bare parameter-annotation syntax is intentionally rejected.

## Supported annotation type syntax
- Named/basic: `int`, `string`, `Node`
- Postfix containers:
  - `int list`
  - `Node option`
  - `Node map`
- Tuple:
  - `(int * string)`
- Function:
  - `int -> string`
  - `(int -> string) list`

## Type-checking semantics
- An annotation constrains the parameter type.
- Type inference must unify body usage with the declared annotation.
- Mismatch produces a type error at compile/inference time.

## Why annotations matter
- They enable cases where pure inference is not enough, especially record field access on function arguments.
- Example:
  - `let display_node (node: Node) = $"{node.Value}"`

## Current scope and limits
- Supported only for function parameters (`let`/`fun`).
- Not supported yet:
  - variable binding annotations (`let x: int = ...`)
  - return type annotations
  - pattern-level type annotations
