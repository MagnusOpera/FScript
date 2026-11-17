---
id: collections
title: Collections
slug: /language/collections
---

Choose lists for ordered items, tuples for a fixed group of values, options for absence, records for named fields, and maps for keyed lookup. This chapter introduces their construction; the next chapter names records and models choices with unions.

## Lists

```fsharp
let numbers = [1; 2; 3]
let doubled = numbers |> List.map (fun n -> n * 2)
let maybeSecond = numbers[1] // Some 2
let descending = [3..1] // [3; 2; 1]
```

Lists hold one element type. Indexing returns an option; an invalid index returns `None`. Ranges include both endpoints and step by one in either direction.

## Tuples

```fsharp
let pair = ("api", 3)
let (name, count) = pair
```

A tuple can combine different types. Destructure it instead of indexing it.

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
let age = older.Age
```

An update creates a new record and preserves `p`. The ordinary update form replaces existing fields with compatible values. Explicit structural records use `{| ... |}`; see [Structural vs Named Annotations](./structural-vs-named-annotations) for shape-based helpers and updates.

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

Next: [Type Declarations](./type-declarations).
