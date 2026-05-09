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

## Anonymous functions

```fsharp
let inc = fun x -> x + 1
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
