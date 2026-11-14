---
id: stdlib-list
title: List Module
slug: /stdlib/list
---

## Native list access

| Form | Signature | Description |
| --- | --- | --- |
| `values[index]` | `'a list -> int -> 'a option` | Zero-based optional indexer for lists. |

```fsharp
let maybeSecond = [10; 20; 30][1]
```

## `List.empty : 'a list`

Returns an empty list.

```fsharp
let xs = List.empty
```

## `List.map : ('a -> 'b) -> 'a list -> 'b list`

Transforms each item.

```fsharp
let doubled = [1; 2; 3] |> List.map (fun n -> n * 2)
```

## `List.iter : ('a -> unit) -> 'a list -> unit`

Applies a side-effecting function to each item.

```fsharp
["a"; "b"] |> List.iter Console.writeLine
```

## `List.choose : ('a -> 'b option) -> 'a list -> 'b list`

Maps and keeps only `Some` results.

```fsharp
let parsed = ["1"; "x"; "2"] |> List.choose Int.tryParse
```

## `List.collect : ('a -> 'b list) -> 'a list -> 'b list`

Maps each item to a list, then concatenates.

```fsharp
let pairs = [1; 2] |> List.collect (fun n -> [n; n])
```

## `List.exists : ('a -> bool) -> 'a list -> bool`

Returns `true` if any item matches.

```fsharp
let hasEven = [1; 3; 4] |> List.exists (fun n -> n % 2 = 0)
```

## `List.contains : 'a -> 'a list -> bool`

Checks scalar membership (`unit`, `int`, `float`, `bool`, or `string`). The current implementation does not compare composite values here; use `List.exists (fun item -> item = needle)` for records, options, tuples, lists, maps, or union values.

```fsharp
let hasTwo = [1; 2; 3] |> List.contains 2
```

## `List.rev : 'a list -> 'a list`

Reverses a list.

```fsharp
let reversed = [1; 2; 3] |> List.rev
```

## `List.distinct : 'a list -> 'a list`

Removes duplicates while preserving the first occurrence order. Equality is recursive for data values; host-managed values compare their visible fields, excluding opaque payloads.

```fsharp
let unique = [1; 2; 2; 3] |> List.distinct
```

## `List.fold : ('state -> 'a -> 'state) -> 'state -> 'a list -> 'state`

Folds left over a list.

```fsharp
let sum = [1; 2; 3] |> List.fold (fun acc n -> acc + n) 0
```

## `List.filter : ('a -> bool) -> 'a list -> 'a list`

Keeps only matching items.

```fsharp
let odds = [1; 2; 3; 4] |> List.filter (fun n -> n % 2 = 1)
```

## `List.length : 'a list -> int`

Returns item count.

```fsharp
let n = ["a"; "b"] |> List.length
```

## `List.tryFind : ('a -> bool) -> 'a list -> 'a option`

Returns first matching element.

```fsharp
let maybeBig = [2; 4; 9] |> List.tryFind (fun n -> n > 5)
```

## `List.tryGet : ('a -> bool) -> 'a list -> int option`

Returns index of first matching element.

```fsharp
let maybeIndex = ["a"; "bb"; "ccc"] |> List.tryGet (fun s -> String.indexOf "c" s |> Option.isSome)
```

## `List.tryItem : int -> 'a list -> 'a option`

Returns the item at a zero-based index.
Returns `None` for negative or out-of-range indices.

```fsharp
let maybeSecond = List.tryItem 1 [10; 20; 30]
```

## `List.tryHead : 'a list -> 'a option`

Returns first item if present.

```fsharp
let maybeFirst = [10; 20] |> List.tryHead
```

## `List.tail : 'a list -> 'a list`

Returns all elements except the head. An empty input is an evaluation error; use a `head :: tail` / `[]` match when emptiness is expected.

```fsharp
let rest = [1; 2; 3] |> List.tail
```

## `List.append : 'a list -> 'a list -> 'a list`

Concatenates two lists.

```fsharp
let all = List.append [1; 2] [3; 4]
```

## Pure support additions

Count/size arguments must fit `0..2147483647`; chunk sizes must also be positive. Invalid arguments raise script errors. Collection callbacks run in the documented order and may themselves have effects; these helpers are pure when their callbacks are pure.

### `List.isEmpty`

```fsharp
List.isEmpty : 'a list -> bool
```

Tests whether the list is empty.

```fsharp
List.isEmpty (List.init 0 (fun i -> i))
// true
```

### `List.forall`

```fsharp
List.forall : ('a -> bool) -> 'a list -> bool
```

Tests every item, stopping at the first false predicate result. Empty input returns true.

```fsharp
List.forall (fun x -> x > 0) [1;2]
// true
```

### `List.mapi`

```fsharp
List.mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
```

Maps items with their zero-based index, visiting each item once in input order.

```fsharp
List.mapi (fun i x -> i + x) [10;20]
// [10;21]
```

### `List.indexed`

```fsharp
List.indexed : 'a list -> ((int * 'a)) list
```

Pairs each item with its zero-based index.

```fsharp
List.indexed [4;5]
// [(0, 4);(1, 5)]
```

### `List.init`

```fsharp
List.init : int -> (int -> 'a) -> 'a list
```

Constructs a list by calling the initializer with indices `0` through `count - 1`. Zero makes no calls.

```fsharp
List.init 3 (fun i -> i * 2)
// [0;2;4]
```

### `List.take`

```fsharp
List.take : int -> 'a list -> 'a list
```

Returns the first count items. Negative counts and counts exceeding the list length raise an error.

```fsharp
List.take 2 [1;2;3]
// [1;2]
```

### `List.skip`

```fsharp
List.skip : int -> 'a list -> 'a list
```

Drops the first count items. Negative counts and counts exceeding the list length raise an error.

```fsharp
List.skip 2 [1;2;3]
// [3]
```

### `List.takeWhile`

```fsharp
List.takeWhile : ('a -> bool) -> 'a list -> 'a list
```

Returns the longest prefix satisfying the predicate. Evaluation stops at the first false result.

```fsharp
List.takeWhile (fun x -> x < 3) [1;2;3;1]
// [1;2]
```

### `List.skipWhile`

```fsharp
List.skipWhile : ('a -> bool) -> 'a list -> 'a list
```

Drops the longest prefix satisfying the predicate. Evaluation stops at the first false result.

```fsharp
List.skipWhile (fun x -> x < 3) [1;2;3;1]
// [3;1]
```

### `List.partition`

```fsharp
List.partition : ('a -> bool) -> 'a list -> ('a list * 'a list)
```

Returns `(matching, other)` lists. Calls the predicate once per item and preserves input order in both outputs.

```fsharp
List.partition (fun x -> x % 2 = 0) [1;2;3;4]
// ([2;4], [1;3])
```

### `List.zip`

```fsharp
List.zip : 'a list -> 'b list -> (('a * 'b)) list
```

Pairs corresponding items. Different list lengths raise an error.

```fsharp
List.zip [1;2] ["a";"b"]
// [(1, "a");(2, "b")]
```

### `List.unzip`

```fsharp
List.unzip : (('a * 'b)) list -> ('a list * 'b list)
```

Splits pairs into two lists, preserving order.

```fsharp
List.unzip [(1,"a");(2,"b")]
// ([1;2], ["a";"b"])
```

### `List.chunkBySize`

```fsharp
List.chunkBySize : int -> 'a list -> 'a list list
```

Splits into consecutive chunks of at most size items. Size must be positive. Empty input produces an empty list.

```fsharp
List.chunkBySize 2 [1;2;3]
// [[1;2];[3]]
```

### `List.scan`

```fsharp
List.scan : ('d -> 'a -> 'd) -> 'd -> 'a list -> 'd list
```

Returns the initial state followed by every accumulated state. Visits items from left to right.

```fsharp
List.scan (fun s x -> s+x) 0 [1;2;3]
// [0;1;3;6]
```

### `List.sortWith`

```fsharp
List.sortWith : ('a -> 'a -> int) -> 'a list -> 'a list
```

Performs a stable ascending sort using a comparer returning a negative, zero, or positive integer. Use this for composite keys. The comparer must define a consistent order.

```fsharp
List.sortWith (fun a b -> a-b) [3;1;2;1]
// [1;1;2;3]
```

### `List.sortBy`

```fsharp
List.sortBy : ('a -> 'c) -> 'a list -> 'a list
```

Performs a stable ascending sort after computing each projection once in input order. Supported keys are int, float, string, bool, and unit. String ordering is ordinal; float NaN sorts before other floats. Composite keys require `List.sortWith`.

```fsharp
List.sortBy String.length ["bb";"a";"cc"]
// ["a";"bb";"cc"]
```

### `List.distinctBy`

```fsharp
List.distinctBy : ('a -> 'c) -> 'a list -> 'a list
```

Retains the first item for each structurally equal projected key. Calls the projection once per item in input order.

```fsharp
List.distinctBy String.length ["a";"b";"cc"]
// ["a";"cc"]
```

### `List.groupBy`

```fsharp
List.groupBy : ('a -> 'c) -> 'a list -> (('c * 'a list)) list
```

Groups items by structural equality of projected keys. Groups follow first key occurrence, and items retain input order. Calls the projection once per item.

```fsharp
List.groupBy String.length ["a";"bb";"c"]
// [(1, ["a";"c"]);(2, ["bb"])]
```

### `List.countBy`

```fsharp
List.countBy : ('a -> 'c) -> 'a list -> (('c * int)) list
```

Counts structurally equal projected keys. Results follow first key occurrence. Calls the projection once per item.

```fsharp
List.countBy String.length ["a";"bb";"c"]
// [(1, 2);(2, 1)]
```
