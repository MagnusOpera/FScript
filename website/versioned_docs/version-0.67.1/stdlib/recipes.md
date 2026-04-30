---
id: stdlib-recipes
title: Stdlib Recipes
slug: /stdlib/recipes
---

## Parse all valid integers from text list

```fsharp
let numbers = ["1"; "x"; "3"] |> List.choose Int.tryParse
```

## Normalize and deduplicate names

```fsharp
let names =
  ["Ada"; "ada"; "BOB"]
  |> List.map String.toLower
  |> List.distinct
```

## Build a config map with defaults

```fsharp
let config = Map.add "host" "localhost" Map.empty
let host = config |> Map.tryGet "host" |> Option.defaultValue "127.0.0.1"
```

## Keep only positive values in a map

```fsharp
let filtered =
  { ["a"] = 1; ["b"] = -1 }
  |> Map.filter (fun _ value -> value > 0)
```
