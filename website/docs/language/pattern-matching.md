---
id: pattern-matching
title: Pattern Matching
slug: /language/pattern-matching
---

Pattern matching is central in FScript.

## Matching options

```fsharp
let describe opt =
  match opt with
  | Some x -> $"value={x}"
  | None -> "missing"
```

## Matching lists

```fsharp
let firstOrZero xs =
  match xs with
  | head :: _ -> head
  | [] -> 0
```

## Matching records

```fsharp
type User = { Name: string; Role: string }

let greet u =
  match u with
  | { Name = n; Role = "admin" } -> $"Welcome admin {n}"
  | { Name = n } -> $"Welcome {n}"
```

## Matching unions

```fsharp
type Result =
  | Ok of int
  | Error of string

let format r =
  match r with
  | Ok value -> $"ok:{value}"
  | Error msg -> $"error:{msg}"
```

## Matching maps

Map patterns use string keys in indexer form.

```fsharp
let remove key m =
  match m with
  | { [k] = _; ..rest } when k = key -> rest
  | _ -> m
```

`k` is always a `string` key because map indexers in FScript are string-only.
