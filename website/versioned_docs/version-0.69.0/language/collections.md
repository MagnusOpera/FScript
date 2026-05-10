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

Maps always use a `string` indexer key type.

- Valid map indexing/key usage uses strings: `scores |> Map.tryGet "math"`.
- Invalid key types (like `int`, `bool`) are not allowed.

```fsharp
// Invalid: numeric key type
// let invalid = { [1] = "one" }
```
