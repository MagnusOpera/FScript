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
- Unit parameter:
  - `let f () = 42`
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
- Unit parameter:
  - `fun () -> 42`
- Annotated:
  - `fun (x: int) -> x + 1`

## Syntax rule
- `()` is valid as a unit parameter in both let-bound functions and lambdas.
- Annotated parameters use parenthesized form `(name: Type)`.
- Let-bound function returns use `: Type` after all parameters and before `=`.
  - `let f x y : int = x + y`
- Return annotations are valid only for function bindings (bindings with at least one parameter).
  - `let value : int = 1` is not valid.

## Generic functions

A function can introduce a named type variable by using it in an annotation, as in
`let identity (value: 'a) : 'a = value`. Occurrences of the same name share one type
within the function's annotation scope.

A declaration can give its type parameters explicitly.

```fsharp
let pair<'left, 'right> (left: 'left) (right: 'right) = (left, right)
let inferred = pair 42 "answer"
let explicit = pair<int, string> 42 "answer"
```

Type arguments follow the declared parameter order. The opening angle bracket
in an expression touches the function name. Ordinary calls infer the arguments.
Explicitly declared parameters are unconstrained and distinct. A body that
forces one to be numeric or equates two declared parameters is a type error.
Recursive bindings use monomorphic references within their own recursive group.

## Supported annotation type syntax
- Named/basic and applied types: `int`, `float`, `numeric`, `string`, `Node`, `Box<int>`
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
- `numeric` annotations introduce fresh constrained variables, accepting only `int` or `float`. Body usage establishes relationships between them; `let add (x: numeric) (y: numeric) : numeric = x + y` ties both arguments and the result to one concrete type per application.
- A concrete numeric annotation narrows a shared numeric relationship without conversion: `let add x (y: int) = x + y` infers `(x: int) -> (y: int) -> int` and rejects float arguments.

## Practical use
- Annotations are useful for making function argument intent explicit.
- Example:
  - `let display_node (node: Node) = $"{node.Value}"`
  - `let format_address (address: {| City: string; Zip: int |}) = $"{address.City} ({address.Zip})"`
  - `let increment x : int = x + 1`
