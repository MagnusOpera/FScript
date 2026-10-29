---
id: stdlib-map
title: Map Module
slug: /stdlib/map
---

FScript maps support string, int, bool, and unions whose cases all have no payload.

Use `valueType map[keyType]` for key annotations, such as `string map[int]`. The legacy `valueType map` suffix means string keys. Keys and values can be inferred without annotations.

## Key types and inference

The type before `map` is the value type. The optional brackets specify the key type.

| Annotation | Key type | Value type |
| --- | --- | --- |
| `int map` | `string` | `int` |
| `string map[int]` | `int` | `string` |
| `string map[bool]` | `bool` | `string` |
| `string map[Priority]` | `Priority` | `string` |

`int map[string]` is also accepted and displayed as `int map`. Prefer the shorter form for string keys. Map types compose with other postfix types: `int list map[bool]` maps booleans to integer lists, and `string map[int] list` is a list of integer-keyed maps.

```fsharp
type Priority = | Low | High

let names = { ["owner"] = "Ada" }
let byId = { [0] = "toto"; [1] = "titi" }
let byFlag = Map.ofList [(true, "enabled"); (false, "disabled")]
let byPriority = Map.empty |> Map.add Low "later" |> Map.add High "now"

let getZero m = m |> Map.tryGet 0
let first = byId |> getZero |> Option.defaultValue "missing"
```

`getZero` infers `'v map[int] -> 'v option`: the lookup constrains the key type to `int`, while the value type remains polymorphic. The same helper can read maps of strings or integers. Function arguments do not need annotations for this inference.

All keys in a map must share one type, and all values must share one type. `Map.fold`, `Map.filter`, and `Map.iter` receive keys of the map's actual key type. `Map.map` and `Map.choose` preserve that key type. Empty maps acquire their key and value types from use.

An unresolved `numeric` key is constrained to `int`. This does not constrain numeric map values:

```fsharp
let make k = { [k + k] = "value" } // k is inferred as int
let numericValues v = { [true] = v + v }
let floatValues = numericValues 2.5
```

When the collection type is unknown, `xs[0]` infers a list argument. Use `Map.tryGet 0` to establish an integer-keyed map without an annotation.

## Native map access

| Form | Signature | Description |
| --- | --- | --- |
| `values[key]` | `'v map['key] -> 'key -> 'v option` | Optional lookup by the map key type. Missing keys return `None`. |

```fsharp
let maybeA = { ["a"] = 1 }["a"]
```

## `Map.empty : 'v map['key]`

Empty map.

```fsharp
let m = Map.empty
```

## `Map.tryGet : 'key -> 'v map['key] -> 'v option`

Looks up a key.

```fsharp
let m = { ["a"] = 1 }
let maybeA = m |> Map.tryGet "a"
```

## `Map.containsKey : 'key -> 'v map['key] -> bool`

Checks key existence.

```fsharp
let hasA = { ["a"] = 1 } |> Map.containsKey "a"
```

## `Map.add : 'key -> 'v -> 'v map['key] -> 'v map['key]`

Adds or replaces a key value.

```fsharp
let updated = Map.add "port" 8080 Map.empty
```

## `Map.ofList : ('key * 'v) list -> 'v map['key]`

Builds a map from key-value tuples. If a key appears more than once, the last value wins.

```fsharp
let m = [("a", 1); ("b", 2)] |> Map.ofList
```

## `Map.fold : ('state -> 'key -> 'v -> 'state) -> 'state -> 'v map['key] -> 'state`

Folds over key-value entries in key order, rather than insertion order. The other map traversals use the same ordering.

```fsharp
let total =
  { ["a"] = 1; ["b"] = 2 }
  |> Map.fold (fun acc _ value -> acc + value) 0
```

## `Map.count : 'v map['key] -> int`

Returns entry count.

```fsharp
let n = { ["a"] = 1; ["b"] = 2 } |> Map.count
```

## `Map.filter : ('key -> 'v -> bool) -> 'v map['key] -> 'v map['key]`

Keeps entries that satisfy predicate.

```fsharp
let small =
  { ["a"] = 1; ["b"] = 10 }
  |> Map.filter (fun _ v -> v < 5)
```

## `Map.choose : ('key -> 'v -> 'u option) -> 'v map['key] -> 'u map['key]`

Transforms entries and drops `None` results.

```fsharp
let parsed =
  { ["a"] = "1"; ["b"] = "x" }
  |> Map.choose (fun _ v -> Int.tryParse v)
```

## `Map.map : ('v -> 'u) -> 'v map['key] -> 'u map['key]`

Transforms values.

```fsharp
let doubled = { ["x"] = 2 } |> Map.map (fun v -> v * 2)
```

## `Map.iter : ('key -> 'v -> unit) -> 'v map['key] -> unit`

Iterates entries for side effects.

```fsharp
{ ["x"] = 2; ["y"] = 3 } |> Map.iter (fun k v -> Console.writeLine $"{k}={v}")
```

## `Map.remove : 'key -> 'v map['key] -> 'v map['key]`

Removes a key.

```fsharp
let m = { ["x"] = 2 } |> Map.remove "x"
```

## Key restrictions

Float, record, list, tuple, and option keys are unsupported. Mixing key types in one map is also invalid. A union key type must have no payload in **any** of its cases, even if the map uses only a payload-free case.

```fsharp
// Invalid: float key
// let badFloat = { [1.5] = "one" }

// Invalid: mixed key types
// let mixed = { [0] = "zero"; [true] = "yes" }

type Result = | Empty | Value of int
// Invalid: Result has a payload-bearing case
// let badUnion = { [Empty] = "empty" }
```

`Json.serialize` encodes string, integer, and boolean keys as JSON object property names. It returns `None` for maps containing union keys.

## Pure support additions

### `Map.keys`

```fsharp
Map.keys : 'a map['c] -> 'c list
```

Returns keys in the map’s existing deterministic key order.

```fsharp
Map.keys { ["b"]=2; ["a"]=1 }
// ["a";"b"]
```

### `Map.values`

```fsharp
Map.values : 'a map['c] -> 'a list
```

Returns values in the corresponding deterministic key order.

```fsharp
Map.values { ["b"]=2; ["a"]=1 }
// [1;2]
```

### `Map.toList`

```fsharp
Map.toList : 'a map['c] -> (('c * 'a)) list
```

Returns `(key, value)` pairs in deterministic key order.

```fsharp
Map.toList { ["a"]=1 }
// [("a", 1)]
```

### `Map.exists`

```fsharp
Map.exists : ('c -> 'a -> bool) -> 'a map['c] -> bool
```

Tests entries in key order and stops at the first true predicate result. Empty maps return false.

```fsharp
Map.exists (fun k v -> k="a" && v=1) { ["a"]=1 }
// true
```

### `Map.forall`

```fsharp
Map.forall : ('c -> 'a -> bool) -> 'a map['c] -> bool
```

Tests entries in key order and stops at the first false predicate result. Empty maps return true.

```fsharp
Map.forall (fun k v -> v>0) { ["a"]=1 }
// true
```

### `Map.update`

```fsharp
Map.update : 'c -> ('a option -> 'a option) -> 'a map['c] -> 'a map['c]
```

Calls the updater once with the current value option. `Some value` inserts or replaces the entry; `None` removes it.

```fsharp
Map.update "a" (fun v -> Some (Option.defaultValue 0 v + 1)) { ["a"]=1 } |> Map.tryGet "a"
// Some 2
```

### `Map.mergeWith`

```fsharp
Map.mergeWith : ('c -> 'a -> 'a -> 'a) -> 'a map['c] -> 'a map['c] -> 'a map['c]
```

Combines both maps. The resolver is called only for shared keys, in right-map key order, with key, left value, then right value. Unique entries are retained.

```fsharp
Map.mergeWith (fun k a b -> a+b) { ["a"]=1 } { ["a"]=2; ["b"]=4 } |> Map.toList
// [("a", 3);("b", 4)]
```
