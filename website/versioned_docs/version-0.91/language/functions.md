---
id: functions
title: Functions
slug: /language/functions
---

Functions are values: pass them to other functions, return them, or bind them to a name. Curried functions take one argument at a time, which makes partial application and pipelines useful.

## Declaring functions

```fsharp
let add x y = x + y
let result = add 2 3
```

`add 2 3` means `(add 2) 3`, not a tuple argument. Arithmetic infers a shared `numeric` type; one call uses integers or floats consistently.

Zero-argument functions use `()` as their parameter and call syntax:

```fsharp
let ping () = "pong"
let message = ping ()
```

## Anonymous functions

```fsharp
let inc = fun x -> x + 1
```

Anonymous functions can also use a unit parameter:

```fsharp
let getAnswer = fun () -> 42
let answer = getAnswer ()
```

## Partial application

```fsharp
let add10 = add 10
let twelve = add10 2
```

## Recursive functions

```fsharp
let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)
```

## Pipeline style

```fsharp
let text =
  "fscript"
  |> fun s -> String.toUpper s
  |> fun s -> $"{s}!"
```

## Closures

A function captures the bindings it uses from its surrounding scope:

```fsharp
let makeAdder amount = fun value -> value + amount
let addFive = makeAdder 5
let answer = addFive 37
```

## Mutual recursion

Use `and` to keep mutually recursive functions in one group:

```fsharp
let rec isEven n =
  if n = 0 then true else isOdd (n - 1)
and isOdd n =
  if n = 0 then false else isEven (n - 1)

let even = isEven 4
```

## Explicit contracts

Annotations belong on function parameters and returns:

```fsharp
let label (count: int) : string = $"count={count}"
```

A value binding such as `let count : int = 3` is not supported. When a fixed input type matters, annotate a function parameter or receiving function instead. See [Type System](./type-system) for generalization and numeric constraints.

Next: [Indentation and Layout](./indentation-and-layout).
