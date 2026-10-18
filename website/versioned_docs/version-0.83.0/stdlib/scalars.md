---
id: stdlib-scalars
title: Int, Float, Bool Modules
slug: /stdlib/scalars
---

## Int

### `Int.tryParse : string -> int option`

Parses an invariant integer string.

```fsharp
let maybeN = Int.tryParse "42"
```

### `Int.toString : int -> string`

Formats an integer using invariant formatting.

```fsharp
let text = Int.toString 42
```

## Float

### `Float.tryParse : string -> float option`

Parses an invariant floating-point string.

```fsharp
let maybePi = Float.tryParse "3.14"
```

### `Float.toString : float -> string`

Formats a float using a round-trippable representation.

```fsharp
let text = Float.toString 3.14
```

## Bool

### `Bool.tryParse : string -> bool option`

Parses `true` or `false`.

```fsharp
let maybeFlag = Bool.tryParse "true"
```

### `Bool.toString : bool -> string`

Formats a boolean as lowercase text.

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
