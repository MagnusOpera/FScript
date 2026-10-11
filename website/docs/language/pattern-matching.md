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

Map patterns use bracketed keys with the same types as map literals: string, int, bool, or payload-free union cases.

```fsharp
let remove key m =
  match m with
  | { [k] = _; ..rest } when k = key -> rest
  | _ -> m
```

`k` has the map's inferred key type. It can be a string, int, bool, or payload-free union value.

A pattern can also select a specific union key:

```fsharp
type Priority = | Low | High
let describe m =
  match m with
  | { [High] = value } -> value
  | _ -> "missing"
let message = describe { [Low] = "later"; [High] = "now" }
```

This infers `describe` as `string map[Priority] -> string`. Extra entries are permitted in a map match.
