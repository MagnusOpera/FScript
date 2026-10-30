# FScript Stdlib Functions

## Purpose
This document lists core standard-library functions and the optional default .NET runtime extern catalog. Core functions are loaded automatically by `FScript.Language`. Runtime functions require host registration through `FScript.Runtime.Registry.all`; the CLI enables them by default. `Env` is a CLI/REPL-injected value, not an always-injected language value.

## Modules

- Core: `Env.setExitCode`, `List`, `Option`, `Map`, `Int`, `Float`, `Bool`, `String`.
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
  - execution joins all tasks before returning and collects foreground and background failures
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
  - Replaces all non-overlapping occurrences using ordinal, case-sensitive matching. An empty `oldValue` returns `source` unchanged; an empty `newValue` removes matches. This operation always returns a string.
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

## Env.setExitCode

`Env.setExitCode : int -> unit` is an always-available core operation, including in embedding and JavaScript. It coexists with host-provided `Env.ScriptName` and `Env.Arguments` without adding a field to the `Environment` record. The qualified operation is reserved even when an `Env` value is in scope.

On .NET and compiled targets, the code must be in the portable shell range `0..255`; other values fail with an evaluation error. It selects the eventual shell status and returns unit normally. It never calls process exit, cancels tasks, or changes a host process's exit status during embedded evaluation. The default is `0`, repeated calls use the last completed write, and unhandled execution failures still produce failure rather than a selected success code.

This operation is effectful and is not `Pure`. Its writes survive pruning and common-subexpression elimination. Writes from tasks share the execution status and are synchronized, but their order is scheduling-dependent. For deterministic status, await tasks and set it afterward in the main flow. Existing task lifetime and failure rules are preserved.

```fsharp
Env.setExitCode 7
42 // still evaluated; shell status is 7
```

Result printing is independent of this status. `--no-result` suppresses automatic display in the CLI and can be baked into compiled applications.


Fable/JavaScript deliberately treats this operation as a no-op (including shell-range validation). Arguments still evaluate normally, returned values remain unchanged, and the website displays the returned value as before.


## Pure support expansion

The following functions extend the standard library. All core additions are reserved names, available without host externs, and implemented by the evaluator, artifact interpreter, both native backends, and JavaScript engine. Encoding/SHA-256 remain separately registered .NET runtime externs; native outputs include equivalent BCL-only implementations. Existing IO capabilities are unchanged. The sandbox grants no additional filesystem, network, process, environment, clock, or random capability.

The website module references are the detailed behavioral contracts, including examples and failure behavior:

- [scalars](../../website/docs/stdlib/scalars.md)
- [string](../../website/docs/stdlib/string.md)
- [list](../../website/docs/stdlib/list.md)
- [map](../../website/docs/stdlib/map.md)
- [option](../../website/docs/stdlib/option.md)
- [hash](../../website/docs/stdlib/hash.md)
- [path](../../website/docs/stdlib/path.md)
- [glob](../../website/docs/stdlib/glob.md)
- [semver](../../website/docs/stdlib/semver.md)
- [encoding](../../website/docs/stdlib/encoding.md)

### Added signatures

- `Int.abs : int -> int`
- `Int.sign : int -> int`
- `Int.min : int -> int -> int`
- `Int.max : int -> int -> int`
- `Int.clamp : int -> int -> int -> int`
- `Float.abs : float -> float`
- `Float.sign : float -> int`
- `Float.min : float -> float -> float`
- `Float.max : float -> float -> float`
- `Float.clamp : float -> float -> float -> float`
- `Float.floor : float -> float`
- `Float.ceil : float -> float`
- `Float.round : float -> float`
- `Float.truncate : float -> float`
- `Float.sqrt : float -> float`
- `Float.log : float -> float`
- `Float.log10 : float -> float`
- `Float.exp : float -> float`
- `Float.sin : float -> float`
- `Float.cos : float -> float`
- `Float.tan : float -> float`
- `Float.asin : float -> float`
- `Float.acos : float -> float`
- `Float.atan : float -> float`
- `Float.pow : float -> float -> float`
- `Float.atan2 : float -> float -> float`
- `Float.isNaN : float -> bool`
- `Float.isInfinity : float -> bool`
- `Float.isFinite : float -> bool`
- `Float.pi : float`
- `Int.toFloat : int -> float`
- `Float.tryToInt : float -> int option`
- `String.length : string -> int`
- `String.isEmpty : string -> bool`
- `String.isWhiteSpace : string -> bool`
- `String.trim : string -> string`
- `String.trimStart : string -> string`
- `String.trimEnd : string -> string`
- `String.splitLines : string -> string list`
- `String.startsWith : string -> string -> bool`
- `String.contains : string -> string -> bool`
- `String.padLeft : int -> string -> string`
- `String.padRight : int -> string -> string`
- `String.repeat : int -> string -> string`
- `List.isEmpty : 'a list -> bool`
- `List.forall : ('a -> bool) -> 'a list -> bool`
- `List.mapi : (int -> 'a -> 'b) -> 'a list -> 'b list`
- `List.indexed : 'a list -> ((int * 'a)) list`
- `List.init : int -> (int -> 'a) -> 'a list`
- `List.take : int -> 'a list -> 'a list`
- `List.skip : int -> 'a list -> 'a list`
- `List.takeWhile : ('a -> bool) -> 'a list -> 'a list`
- `List.skipWhile : ('a -> bool) -> 'a list -> 'a list`
- `List.partition : ('a -> bool) -> 'a list -> ('a list * 'a list)`
- `List.zip : 'a list -> 'b list -> (('a * 'b)) list`
- `List.unzip : (('a * 'b)) list -> ('a list * 'b list)`
- `List.chunkBySize : int -> 'a list -> 'a list list`
- `List.scan : ('d -> 'a -> 'd) -> 'd -> 'a list -> 'd list`
- `List.sortWith : ('a -> 'a -> int) -> 'a list -> 'a list`
- `List.sortBy : ('a -> 'c) -> 'a list -> 'a list`
- `List.distinctBy : ('a -> 'c) -> 'a list -> 'a list`
- `List.groupBy : ('a -> 'c) -> 'a list -> (('c * 'a list)) list`
- `List.countBy : ('a -> 'c) -> 'a list -> (('c * int)) list`
- `Map.keys : 'a map['c] -> 'c list`
- `Map.values : 'a map['c] -> 'a list`
- `Map.toList : 'a map['c] -> (('c * 'a)) list`
- `Map.exists : ('c -> 'a -> bool) -> 'a map['c] -> bool`
- `Map.forall : ('c -> 'a -> bool) -> 'a map['c] -> bool`
- `Map.update : 'c -> ('a option -> 'a option) -> 'a map['c] -> 'a map['c]`
- `Map.mergeWith : ('c -> 'a -> 'a -> 'a) -> 'a map['c] -> 'a map['c] -> 'a map['c]`
- `Option.bind : ('a -> 'b option) -> 'a option -> 'b option`
- `Option.flatten : 'a option option -> 'a option`
- `Option.filter : ('a -> bool) -> 'a option -> 'a option`
- `Option.orElse : 'a option -> 'a option -> 'a option`
- `Option.orElseWith : (unit -> 'a option) -> 'a option -> 'a option`
- `Option.map2 : ('a -> 'b -> 'd) -> 'a option -> 'b option -> 'd option`
- `Option.toList : 'a option -> 'a list`
- `Path.normalize : string -> string`
- `Path.isAbsolute : string -> bool`
- `Path.combine : string -> string -> string`
- `Path.fileName : string -> string`
- `Path.extension : string -> string option`
- `Path.fileNameWithoutExtension : string -> string`
- `Path.parent : string -> string option`
- `Path.relative : string -> string -> string option`
- `Glob.isMatch : string -> string -> bool`
- `Glob.filter : string -> string list -> string list`
- `SemVer.tryParse : string -> ({ Build: string; Major: string; Minor: string; Patch: string; PreRelease: string }) option`
- `SemVer.format : { Build: string; Major: string; Minor: string; Patch: string; PreRelease: string } -> string`
- `SemVer.compare : string -> string -> int option`
- `Encoding.utf8Encode : string -> int list option`
- `Encoding.utf8Decode : int list -> string option`
- `Encoding.base64Encode : int list -> string option`
- `Encoding.base64Decode : string -> int list option`
- `Encoding.hexEncode : int list -> string option`
- `Encoding.hexDecode : string -> int list option`
- `Encoding.uriEncode : string -> string option`
- `Encoding.uriDecode : string -> string option`
- `Hash.sha256 : string -> string option`
- `Hash.sha256Bytes : int list -> string option`

Scalar string operations use UTF-16 indices and ordinal comparisons. Numeric conversions truncate explicitly and reject invalid ranges. Integer abs overflow, reversed clamp bounds, invalid sizes, unequal zip lengths, invalid glob grammar, and invalid SemVer formatting raise errors; parse/decode operations return options.

Higher-order helpers invoke callbacks through the evaluator trampoline. Sorts are stable bottom-up merge sorts with identical comparison order across .NET execution engines; sortBy computes projections once and compares scalar keys only. Group/distinct/count operations use structural equality and first occurrence order. Map operations preserve existing map key ordering. Callbacks retain their effects and failures; compiler purity remains conservative for partial or higher-order additions.

Paths always use portable slash-only lexical rules. Glob patterns never enumerate files. SemVer follows 2.0.0 precedence with unbounded decimal string components. UTF-8 is strict, Base64 uses the standard alphabet, hex outputs lowercase, URI decoding preserves plus signs, and SHA-256 hashes supplied bytes or strict UTF-8 text.

`String.compare : string -> string -> int` returns normalized ordinal ordering. Native ordered maps also use ordinal string ordering, matching evaluator maps independently of the current culture. Whitespace helpers use an explicit Unicode White_Space set shared by all engines rather than runtime-specific trim defaults.


## Task composition

The full optional runtime Task API is specified in the [Task reference](../../website/docs/stdlib/task.md). Additions:

- `Task.delay : int -> unit task`
- `Task.fromResult : 'a -> 'a task`
- `Task.continueWith : ('a -> 'b) -> 'a task -> 'b task`
- `Task.whenAll : ('a list -> 'b) -> 'a task list -> 'b task`
- `Task.whenAny : ('a -> 'b) -> 'a task list -> 'b task`
- `Task.awaitAll : 'a task list -> 'a list`

Delays use timer waits, accepting only 0..2147483647 milliseconds. Completed-value tasks are completed immediately and preserve distinct task handles. Continuation callbacks are scheduled asynchronously, run once on successful prerequisites, receive ordinary values, and preserve nested task results without automatic flattening. All operations snapshot input list order. WhenAll/awaitAll preserve input order; empty all invokes the callback with [] and empty awaitAll returns []. Empty whenAny raises an error. Unit-valued candidates are ordinary homogeneous task lists. Different result types must first be mapped into an explicit user union; the type checker rejects heterogeneous task lists.

WhenAny registers the race at invocation, consumes only its selected task, and does not cancel losers. Already-completed inputs select the first completed list entry; other ties are scheduling-dependent. At top-level evaluator execution, evaluator exported-function calls, and generated Run boundaries, the runtime repeatedly joins all tracked tasks, including tasks created by other tasks and unobserved nested results, until every worker completes. Explicit observation is optional. Background failures do not interrupt independent siblings; failed prerequisites still prevent dependent callbacks. All fatal foreground/background errors are collected after joining. Propagated instances of the same error are reported once, while independent errors with identical messages remain distinct. The evaluator reports multiple failures in one EvalException; generated .NET code uses AggregateException. Single failures preserve their ordinary exception form. Separately invoked native exports have host-owned CLR task lifetimes. A worker or host extern that never completes prevents the boundary from returning; resource limits remain host-owned.

The canonical evaluator/SDK task functions remain optional runtime registrations. Fable task execution and unadapted standalone LIR task calls fail explicitly; compiler catalogs and artifacts preserve the signatures and arities. Tasks are observable scheduling capabilities and receive no automatic compiler purity guarantee.

Task continuation argument order is callback first and task/task list last. All three combinators support pipeline application and reusable partial application, e.g. `let increment = Task.continueWith (fun value -> value + 1)` followed by `Task.fromResult 41 |> increment`. Parameter metadata, evaluator/native/SDK dispatch, and static task-sharing analysis use this same order.
