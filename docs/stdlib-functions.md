# FScript Stdlib Functions

## Purpose
This document lists the functions available in the embedded FScript prelude (stdlib).

The stdlib is loaded automatically by `FScript.Language` before user scripts.

## Modules
- `List`
- `Option`
- `Map`

## Notes
- Functions are curried.
- The stdlib is part of the language runtime and is always available.
- Top-level script bindings cannot collide with reserved stdlib names.

## List
- `List.empty : 'a list`
- `List.map : ('a -> 'b) -> 'a list -> 'b list`
- `List.iter : ('a -> unit) -> 'a list -> unit`
- `List.choose : ('a -> 'b option) -> 'a list -> 'b list`
- `List.collect : ('a -> 'b list) -> 'a list -> 'b list`
- `List.exists : ('a -> bool) -> 'a list -> bool`
- `List.contains : 'a -> 'a list -> bool`
- `List.rev : 'a list -> 'a list`
- `List.distinct : 'a list -> 'a list`
- `List.fold : ('state -> 'a -> 'state) -> 'state -> 'a list -> 'state`
- `List.filter : ('a -> bool) -> 'a list -> 'a list`
- `List.length : 'a list -> int`
- `List.tryFind : ('a -> bool) -> 'a list -> 'a option`
- `List.tryGet : ('a -> bool) -> 'a list -> int option`
- `List.tryHead : 'a list -> 'a option`
- `List.tail : 'a list -> 'a list`
- `List.append : 'a list -> 'a list -> 'a list`

## Option
- `Option.defaultValue : 'a -> 'a option -> 'a`
- `Option.defaultWith : (unit -> 'a) -> 'a option -> 'a`
- `Option.isNone : 'a option -> bool`
- `Option.isSome : 'a option -> bool`
- `Option.map : ('a -> 'b) -> 'a option -> 'b option`

## Map
- `Map.empty : 'v map`  
  Alias of `{}`.
- `Map.tryGet : 'k -> map<'k, 'v> -> 'v option`
- `Map.containsKey : 'k -> map<'k, 'v> -> bool`
- `Map.add : 'k -> 'v -> map<'k, 'v> -> map<'k, 'v>`
- `Map.ofList : ('k * 'v) list -> map<'k, 'v>`
- `Map.fold : ('state -> 'k -> 'v -> 'state) -> 'state -> map<'k, 'v> -> 'state`
- `Map.count : map<'k, 'v> -> int`
- `Map.filter : ('k -> 'v -> bool) -> map<'k, 'v> -> map<'k, 'v>`
- `Map.choose : ('k -> 'v -> 'u option) -> map<'k, 'v> -> map<'k, 'u>`
- `Map.map : ('v -> 'u) -> map<'k, 'v> -> map<'k, 'u>`
- `Map.iter : ('k -> 'v -> unit) -> map<'k, 'v> -> unit`
- `Map.remove : 'k -> map<'k, 'v> -> map<'k, 'v>`

Map key support in FScript currently allows `string` and `int` keys.

## Quick examples
```fsharp
let xs = List.map (fun x -> x + 1) [1;2;3]
let found = List.tryFind (fun x -> x > 2) xs

let maybePort = Some 8080
let port = maybePort |> Option.defaultValue 80

let m = { ["a"] = 1; ["b"] = 2 }
let n = { [1] = "one"; [2] = "two" }
let hasA = Map.containsKey "a" m
let one = n[1] |> Option.defaultValue "missing"
```
