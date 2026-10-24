# FScript Stdlib Functions

## Purpose
This document lists core standard-library functions and the optional default .NET runtime extern catalog. Core functions are loaded automatically by `FScript.Language`. Runtime functions require host registration through `FScript.Runtime.Registry.all`; the CLI enables them by default. `Env` is a CLI/REPL-injected value, not an always-injected language value.

## Modules

- Core: `List`, `Option`, `Map`, `Int`, `Float`, `Bool`, `String`.
- Default .NET runtime: `Console`, `Task`, `Fs`, `Json`, `Regex`, `Xml`, `Hash`, `Guid`.

## Top-level surface
- `ignore : 'a -> unit` is an always-available core function.
- `Env : Environment` is injected by CLI/REPL hosts.

## Host-provided console externs
- `Console.writeLine : string -> unit`
- `Console.readLine : unit -> string option`

## Built-in types and values
- `type Environment = { ScriptName: string option; Arguments: string list }`
- `type FsKind = File of string | Directory of string | Missing`
- `let Env : Environment`
  - Injected by the CLI and REPL hosts.
  - `Env.ScriptName` is `Some "<path>"` for file execution and `None` for stdin/REPL execution.
  - `Env.Arguments` contains script arguments passed after `--`.

## Notes
- Functions are curried.
- Core functions remain available when default runtime externs are disabled.
- Top-level script bindings cannot collide with reserved stdlib names.
- `ignore` is a built-in language function.
- Console I/O is host-provided through runtime externs.
- Task concurrency helpers are host-provided through the default runtime extern registry:
  - `Task.spawn : (unit -> 'a) -> 'a task`
  - `Task.await : 'a task -> 'a`

## Native access forms

### Lists
- Indexer: `values[index] : 'a option`
- Signature: `'a list -> int -> 'a option`
- Description:
  - zero-based optional index access
  - negative or out-of-range indices return `None`

### Maps
- Indexer: `values[key] : 'v option`
- Signature: `'v map['key] -> 'key -> 'v option`
- Description:
  - optional lookup by a key matching the map's key type
  - missing keys return `None`
  - map keys may be `string`, `int`, `bool`, or a union whose cases all have no payload

### Options
- Constructors:
  - `Some : 'a -> 'a option`
  - `None : 'a option`

### Tasks
- Module functions:
  - `Task.spawn : (unit -> 'a) -> 'a task`
  - `Task.await : 'a task -> 'a`
- Description:
  - `Task.spawn` schedules a thunk for concurrent execution and returns an opaque task handle immediately
  - `Task.await` waits for completion and returns the result value
  - task failures are fatal runtime errors
  - ending a program with unawaited tasks is a runtime error
  - hosts may disable `Task.*` by omitting those externs

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

## Task
- `Task.spawn : (unit -> 'a) -> 'a task`
- `Task.await : 'a task -> 'a`

## Map
- `Map.empty : 'v map['key]` (alias of `{}`).
- `Map.tryGet : 'key -> 'v map['key] -> 'v option`
- `Map.containsKey : 'key -> 'v map['key] -> bool`
- `Map.add : 'key -> 'v -> 'v map['key] -> 'v map['key]`
- `Map.ofList : ('key * 'v) list -> 'v map['key]`
- `Map.fold : ('state -> 'key -> 'v -> 'state) -> 'state -> 'v map['key] -> 'state`
- `Map.count : 'v map['key] -> int`
- `Map.filter : ('key -> 'v -> bool) -> 'v map['key] -> 'v map['key]`
- `Map.choose : ('key -> 'v -> 'u option) -> 'v map['key] -> 'u map['key]`
- `Map.map : ('v -> 'u) -> 'v map['key] -> 'u map['key]`
- `Map.iter : ('key -> 'v -> unit) -> 'v map['key] -> unit`
- `Map.remove : 'key -> 'v map['key] -> 'v map['key]`

Map keys may be `string`, `int`, `bool`, or a union whose cases all have no payload.

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

## Core edge behavior

- `List.tail []` raises an evaluation error.
- `List.contains` compares scalar values only; for composite data use `List.exists (fun item -> item = expected)`.
- `List.distinct` preserves first occurrence order and compares composite data recursively, including visible host-managed fields.
- `Map.ofList` keeps the final value for a duplicate key; map traversal follows key order.
- `String.split` preserves empty entries; text searching is ordinal and case-sensitive.

## Default runtime catalog

The following functions require runtime registration. See [external functions](./external-functions.md) for argument order, filesystem policy, serialization constraints, and provider behavior.

- `Console.readLine : unit -> string option`
- `Console.writeLine : string -> unit`
- `Fs.combinePath : string -> string -> string`
- `Fs.createDirectory : string -> bool`
- `Fs.enumerateFiles : string -> string -> string list option`
- `Fs.exists : string -> bool`
- `Fs.extension : string -> string option`
- `Fs.fileNameWithoutExtension : string -> string`
- `Fs.glob : string -> string list option`
- `Fs.kind : string -> FsKind`
- `Fs.parentDirectory : string -> string option`
- `Fs.readText : string -> string option`
- `Fs.writeText : string -> string -> bool`
- `Guid.new : 'a -> string option`
- `Hash.md5 : string -> string option`
- `Json.deserialize : type -> string -> 'a option`
- `Json.serialize : 'a -> string option`
- `Regex.matchGroups : string -> string -> string list option`
- `Task.await : 'a task -> 'a`
- `Task.spawn : (unit -> 'a) -> 'a task`
- `Xml.queryValues : string -> string -> string list option`

`Json.deserialize` selects a decoder from a runtime token; its generic scheme does not statically associate the token and output. `Guid.new` ignores its argument; scripts conventionally pass `()`. Filesystem and serialization options represent supported absence/conversion failure, not a universal exception boundary.

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
| scriptPath :: _ -> Console.writeLine scriptPath
| [] -> Console.writeLine "no args"
```
