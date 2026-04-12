# FScript Stdlib Functions

## Purpose
This document lists the functions available in the embedded FScript prelude (stdlib).

The stdlib is loaded automatically by `FScript.Language` before user scripts.

## Modules
- `List`
- `Option`
- `Map`
- `Int`
- `Float`
- `Bool`
- `String`

## Top-level built-ins
- `Env : Environment`
- `print : string -> unit`
- `ignore : 'a -> unit`

## Built-in types and values
- `type Environment = { ScriptName: string option; Arguments: string list }`
- `type FsKind = File of string | Directory of string | Missing`
- `let Env : Environment`
  - Injected by the CLI and REPL hosts.
  - `Env.ScriptName` is `Some "<path>"` for file execution and `None` for stdin/REPL execution.
  - `Env.Arguments` contains script arguments passed after `--`.

## Notes
- Functions are curried.
- The stdlib is part of the language runtime and is always available.
- Top-level script bindings cannot collide with reserved stdlib names.
- `print` and `ignore` are built-in language functions, not host externs.

## Native access forms

### Lists
- Indexer: `values[index] : 'a option`
- Signature: `'a list -> int -> 'a option`
- Description:
  - zero-based optional index access
  - negative or out-of-range indices return `None`

### Maps
- Indexer: `values[key] : 'v option`
- Signature: `'v map -> string -> 'v option`
- Description:
  - optional lookup by string key
  - missing keys return `None`
  - map keys are string-only

### Options
- Constructors:
  - `Some : 'a -> 'a option`
  - `None : 'a option`

### Records
- Field access: `record.Field`
- Signature: `{ Field: 'a; ... } -> 'a`

### Tuples
- Access model:
  - tuples are consumed via destructuring and pattern matching
  - tuples do not have a native indexer

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
- `List.tryItem : int -> 'a list -> 'a option`
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
- `Map.tryGet : string -> 'v map -> 'v option`
- `Map.containsKey : string -> 'v map -> bool`
- `Map.add : string -> 'v -> 'v map -> 'v map`
- `Map.ofList : (string * 'v) list -> 'v map`
- `Map.fold : ('state -> string -> 'v -> 'state) -> 'state -> 'v map -> 'state`
- `Map.count : 'v map -> int`
- `Map.filter : (string -> 'v -> bool) -> 'v map -> 'v map`
- `Map.choose : (string -> 'v -> 'u option) -> 'v map -> 'u map`
- `Map.map : ('v -> 'u) -> 'v map -> 'u map`
- `Map.iter : (string -> 'v -> unit) -> 'v map -> unit`
- `Map.remove : string -> 'v map -> 'v map`

Map keys in FScript are string-only.

## Int
- `Int.tryParse : string -> int option`
- `Int.toString : int -> string`

## Float
- `Float.tryParse : string -> float option`
- `Float.toString : float -> string`

## Bool
- `Bool.tryParse : string -> bool option`
- `Bool.toString : bool -> string`

## String
- `String.replace : string -> string -> string -> string`
  - argument order: `oldValue -> newValue -> source`
- `String.indexOf : string -> string -> int option`
  - argument order: `value -> source`
- `String.toLower : string -> string`
- `String.toUpper : string -> string`
- `String.substring : int -> int -> string -> string option`
  - argument order: `start -> length -> source`
- `String.concat : string -> string list -> string`
- `String.split : string -> string -> string list`
  - argument order: `separator -> source`
- `String.endsWith : string -> string -> bool`
  - argument order: `suffix -> source`

## Quick examples
```fsharp
let xs = List.map (fun x -> x + 1) [1;2;3]
let found = List.tryFind (fun x -> x > 2) xs
let maybeSecond = List.tryItem 1 xs

let maybePort = Some 8080
let port = maybePort |> Option.defaultValue 80

let m = { ["a"] = 1; ["b"] = 2 }
let hasA = Map.containsKey "a" m
let one = m["a"] |> Option.defaultValue 0
let first = xs[0] |> Option.defaultValue 0

match Env.Arguments with
| scriptPath :: _ -> print scriptPath
| [] -> print "no args"
```
