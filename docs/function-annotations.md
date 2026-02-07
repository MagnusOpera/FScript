# Function Parameter Annotations Specification

## Purpose
This document specifies type annotations for function parameters in FScript.

## Annotation positions
- Let-bound function parameters.
- Lambda parameters.

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
- Annotated parameters use parenthesized form `(name: Type)`.

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
- An annotation constrains parameter type during inference.
- Body usage unifies against the declared annotation.
- Inferred function types include the annotated parameter type.

## Practical use
- Annotations are useful for making function argument intent explicit.
- Example:
  - `let display_node (node: Node) = $"{node.Value}"`
