---
id: stdlib-list
title: List Module
slug: /stdlib/list
---

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
["a"; "b"] |> List.iter print
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
let hasEven = [1; 3; 4] |> List.exists (fun n -> n % 2 == 0)
```

## `List.contains : 'a -> 'a list -> bool`

Checks whether a value appears in a list.

```fsharp
let hasTwo = [1; 2; 3] |> List.contains 2
```

## `List.rev : 'a list -> 'a list`

Reverses a list.

```fsharp
let reversed = [1; 2; 3] |> List.rev
```

## `List.distinct : 'a list -> 'a list`

Removes duplicate values.

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
let odds = [1; 2; 3; 4] |> List.filter (fun n -> n % 2 == 1)
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

## `List.tryHead : 'a list -> 'a option`

Returns first item if present.

```fsharp
let maybeFirst = [10; 20] |> List.tryHead
```

## `List.tail : 'a list -> 'a list`

Returns all elements except the head.

```fsharp
let rest = [1; 2; 3] |> List.tail
```

## `List.append : 'a list -> 'a list -> 'a list`

Concatenates two lists.

```fsharp
let all = List.append [1; 2] [3; 4]
```
