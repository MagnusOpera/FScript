---
id: stdlib-option
title: Option Module
slug: /stdlib/option
---

## `Option.defaultValue : 'a -> 'a option -> 'a`

Returns option value or a fallback.

```fsharp
let port = Some 8080 |> Option.defaultValue 80
```

## `Option.defaultWith : (unit -> 'a) -> 'a option -> 'a`

Computes fallback lazily.

```fsharp
let value = None |> Option.defaultWith (fun () -> 42)
```

## `Option.isNone : 'a option -> bool`

Returns `true` when option is `None`.

```fsharp
let missing = None |> Option.isNone
```

## `Option.isSome : 'a option -> bool`

Returns `true` when option is `Some`.

```fsharp
let present = Some "x" |> Option.isSome
```

## `Option.map : ('a -> 'b) -> 'a option -> 'b option`

Transforms value inside `Some`.

```fsharp
let upper = Some "fscript" |> Option.map String.toUpper
```
