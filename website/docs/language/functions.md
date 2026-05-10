---
id: functions
title: Functions
slug: /language/functions
---

Functions are first-class and curried.

## Declaring functions

```fsharp
let add x y = x + y
let result = add 2 3
```

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
