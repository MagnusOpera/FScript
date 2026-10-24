---
id: control-flow
title: Control Flow
slug: /language/control-flow
---

Choose between values with `if` or `match`. Use `for` for effects over a list. Unlike statement-oriented languages, each form has a result type.

## `if / elif / else`

```fsharp
let classify n =
  if n < 0 then
    "negative"
  elif n = 0 then
    "zero"
  else
    "positive"
```

Both `if` branches must have compatible types. An omitted `else` means a unit result, so it is suitable for an effect such as writing a line.

## `for ... in ... do`

```fsharp
for x in [1; 2; 3] do
  Console.writeLine $"value={x}"
```

The source must be a list and the body must return unit. Use `List.map` to produce a transformed list instead.

## `match`

```fsharp
let describe opt =
  match opt with
  | Some value -> $"some:{value}"
  | None -> "none"
```

Cases are tested in order. A `when` guard can refine a pattern; an unmatched value fails at runtime. See [Pattern Matching](./pattern-matching) after learning the collection and union types.

## Conditional evaluation

Use `if` when an expression must run only in one branch. `&&` and `||` evaluate both operands; they are not short-circuit guards.

```fsharp
let divide numerator denominator =
  if denominator = 0 then None
  else Some (numerator / denominator)
```

## Fatal errors

`raise "message"` stops evaluation. There is no script-level exception handler; represent expected absence with options and domain failures with unions.

Next: [Collections](./collections).
