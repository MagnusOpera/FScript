---
id: collections
title: Collections
slug: /language/collections
---

FScript has lists, tuples, options, records, maps, and unions.

## Lists

```fsharp
let numbers = [1; 2; 3]
let doubled = numbers |> List.map (fun n -> n * 2)
```

## Tuples

```fsharp
let pair = ("api", 3)
```

## Options

```fsharp
let maybeName = Some "Ada"
let name = maybeName |> Option.defaultValue "unknown"
```

## Records

```fsharp
type Person = { Name: string; Age: int }
let p = { Name = "Ada"; Age = 37 }
let older = { p with Age = 38 }
```

## Maps

```fsharp
let scores = { ["math"] = 18; ["science"] = 20 }
let maybeMath = scores |> Map.tryGet "math"
```

Keys may be `string`, `int`, `bool`, or a discriminated union whose cases all have no payload. A map has one key type and one value type, inferred from its entries.

```fsharp
type Priority = | Low | High
let byId = { [0] = "first"; [1] = "second" }
let byFlag = { [true] = "enabled"; [false] = "disabled" }
let byPriority = { [Low] = "later"; [High] = "now" }
let maybeHigh = byPriority[High]
```

When an annotation is useful, put the value type first: `string map[int]`, `string map[bool]`, or `string map[Priority]`. For string keys, use `int map`; `int map[string]` is equivalent and is displayed as `int map`.

Function arguments also participate in inference:

```fsharp
let getZero m = m |> Map.tryGet 0
let maybeFirst = byId |> getZero
```

`getZero` is inferred as `'v map[int] -> 'v option`, with no argument annotation. An empty map (`{}` or `Map.empty`) gets its key and value types from later use.

See the [Map module](../stdlib/map) for operations and key restrictions, and [Pattern Matching](./pattern-matching) for map patterns.
