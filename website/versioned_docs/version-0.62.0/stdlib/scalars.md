---
id: stdlib-scalars
title: Int, Float, Bool Modules
slug: /stdlib/scalars
---

## Int

### `Int.tryParse : string -> int option`

```fsharp
let maybeN = Int.tryParse "42"
```

### `Int.toString : int -> string`

```fsharp
let text = Int.toString 42
```

## Float

### `Float.tryParse : string -> float option`

```fsharp
let maybePi = Float.tryParse "3.14"
```

### `Float.toString : float -> string`

```fsharp
let text = Float.toString 3.14
```

## Bool

### `Bool.tryParse : string -> bool option`

```fsharp
let maybeFlag = Bool.tryParse "true"
```

### `Bool.toString : bool -> string`

```fsharp
let text = Bool.toString true
```

## Parsing pattern

Use parsing helpers with options:

```fsharp
let port =
  Int.tryParse "8080"
  |> Option.defaultValue 80
```
