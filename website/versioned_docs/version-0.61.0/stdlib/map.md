---
id: stdlib-map
title: Map Module
slug: /stdlib/map
---

FScript maps are string-keyed.

Key takeaway: map indexers are always `string`.

## `Map.empty : 'v map`

Empty map.

```fsharp
let m = Map.empty
```

## `Map.tryGet : string -> 'v map -> 'v option`

Looks up a key.

```fsharp
let m = { ["a"] = 1 }
let maybeA = m |> Map.tryGet "a"
```

## `Map.containsKey : string -> 'v map -> bool`

Checks key existence.

```fsharp
let hasA = { ["a"] = 1 } |> Map.containsKey "a"
```

## `Map.add : string -> 'v -> 'v map -> 'v map`

Adds or replaces a key value.

```fsharp
let updated = Map.add "port" 8080 Map.empty
```

## `Map.ofList : (string * 'v) list -> 'v map`

Builds a map from key-value tuples.

```fsharp
let m = [("a", 1); ("b", 2)] |> Map.ofList
```

## `Map.fold : ('state -> string -> 'v -> 'state) -> 'state -> 'v map -> 'state`

Folds over key-value entries.

```fsharp
let total =
  { ["a"] = 1; ["b"] = 2 }
  |> Map.fold (fun acc _ value -> acc + value) 0
```

## `Map.count : 'v map -> int`

Returns entry count.

```fsharp
let n = { ["a"] = 1; ["b"] = 2 } |> Map.count
```

## `Map.filter : (string -> 'v -> bool) -> 'v map -> 'v map`

Keeps entries that satisfy predicate.

```fsharp
let small =
  { ["a"] = 1; ["b"] = 10 }
  |> Map.filter (fun _ v -> v < 5)
```

## `Map.choose : (string -> 'v -> 'u option) -> 'v map -> 'u map`

Transforms entries and drops `None` results.

```fsharp
let parsed =
  { ["a"] = "1"; ["b"] = "x" }
  |> Map.choose (fun _ v -> Int.tryParse v)
```

## `Map.map : ('v -> 'u) -> 'v map -> 'u map`

Transforms values.

```fsharp
let doubled = { ["x"] = 2 } |> Map.map (fun v -> v * 2)
```

## `Map.iter : (string -> 'v -> unit) -> 'v map -> unit`

Iterates entries for side effects.

```fsharp
{ ["x"] = 2; ["y"] = 3 } |> Map.iter (fun k v -> print $"{k}={v}")
```

## `Map.remove : string -> 'v map -> 'v map`

Removes a key.

```fsharp
let m = { ["x"] = 2 } |> Map.remove "x"
```

## Invalid key examples

Only string keys are valid.

```fsharp
// Invalid: int key
// let bad = { [1] = "one" }

// Invalid: bool key
// let alsoBad = { [true] = "yes" }
```
