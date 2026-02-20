# Function Annotations Specification

## Purpose
This document specifies type annotations for function parameters and returns in FScript.

## Annotation positions
- Let-bound function parameters.
- Let-bound function returns.
- Lambda parameters.

## Supported forms

### Let-bound functions
- Unannotated:
  - `let f x = x`
- Annotated parameter:
  - `let f (x: int) = x + 1`
- Annotated return:
  - `let f x : int = x + 1`
- Annotated parameter + return:
  - `let f (x: int) : int = x + 1`
- Mixed parameters:
  - `let f (x: int) y = y`

### Lambdas
- Unannotated:
  - `fun x -> x`
- Annotated:
  - `fun (x: int) -> x + 1`

## Syntax rule
- Annotated parameters use parenthesized form `(name: Type)`.
- Let-bound function returns use `: Type` after all parameters and before `=`.
  - `let f x y : int = x + y`
- Return annotations are valid only for function bindings (bindings with at least one parameter).
  - `let value : int = 1` is not valid.

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
- Structural record (single-line):
  - `{| City: string; Zip: int |}`
  - `{| Meta: string map; Next: Node option |}`
- Declared-type-by-shape record (single-line):
  - `{ City: string; Zip: int }`
  - this form must resolve to exactly one declared record type with matching shape.
- Inline record annotation fields are parsed as single-line, semicolon-separated field lists.

## Type-checking semantics
- An annotation constrains parameter type during inference.
- A return annotation constrains the inferred function result type.
- Body usage unifies against the declared annotation.
- Inferred function types include annotated parameter and return constraints.

## Practical use
- Annotations are useful for making function argument intent explicit.
- Example:
  - `let display_node (node: Node) = $"{node.Value}"`
  - `let format_address (address: {| City: string; Zip: int |}) = $"{address.City} ({address.Zip})"`
  - `let increment x : int = x + 1`
