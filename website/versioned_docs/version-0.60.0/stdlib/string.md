---
id: stdlib-string
title: String Module
slug: /stdlib/string
---

## `String.replace : string -> string -> string -> string`

Argument order is `oldValue -> newValue -> source`.

```fsharp
let text = String.replace "-" "_" "a-b-c"
```

## `String.indexOf : string -> string -> int option`

Argument order is `value -> source`.

```fsharp
let maybeIndex = String.indexOf "script" "fscript"
```

## `String.toLower : string -> string`

```fsharp
let lower = String.toLower "FSCRIPT"
```

## `String.toUpper : string -> string`

```fsharp
let upper = String.toUpper "fscript"
```

## `String.substring : int -> int -> string -> string option`

Argument order is `start -> length -> source`.
Returns `None` for invalid ranges.

```fsharp
let maybeSub = String.substring 0 4 "fscript"
```

## `String.concat : string -> string list -> string`

```fsharp
let joined = String.concat "," ["a"; "b"; "c"]
```

## `String.split : string -> string -> string list`

Argument order is `separator -> source`.

```fsharp
let parts = String.split "," "a,b,c"
```
