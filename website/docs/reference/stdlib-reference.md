---
id: stdlib-reference
title: Standard Library Signature Index
slug: /reference/stdlib
---

This is the complete catalog of core functions/values and default .NET runtime externs. Each name links to argument order, examples, and failure behavior. Type variables are local to each signature; repeated variables in a row must agree.

**Core** functions are always available. **Runtime** functions require host registration, are enabled by default in the CLI, and are disabled by `--no-default-externs` or `runtime.default-externs = false`. JavaScript hosts provide their own externs. Registered custom providers add functions beyond this standard catalog.

## Top-level surface

| Name | Type | Availability |
| --- | --- | --- |
| [`ignore`](../stdlib/builtins) | `'a -> unit` | Core function; evaluates and discards its argument |
| [`Env`](../stdlib/builtins) | `Environment` | Value injected by CLI/REPL, optional in embedding hosts |
| [`Environment`](../stdlib/builtins) | `{ ScriptName: string option; Arguments: string list }` | Built-in record type |
| [`FsKind`](../stdlib/builtins) | `File of string` / `Directory of string` / `Missing` | Built-in union type |

## Core library

### Bool

| Name | Signature |
| --- | --- |
| [`Bool.toString`](../stdlib/scalars) | `bool -> string` |
| [`Bool.tryParse`](../stdlib/scalars) | `string -> bool option` |

### Float

| Name | Signature |
| --- | --- |
| [`Float.toString`](../stdlib/scalars) | `float -> string` |
| [`Float.tryParse`](../stdlib/scalars) | `string -> float option` |

### Int

| Name | Signature |
| --- | --- |
| [`Int.toString`](../stdlib/scalars) | `int -> string` |
| [`Int.tryParse`](../stdlib/scalars) | `string -> int option` |

### List

| Name | Signature |
| --- | --- |
| [`List.append`](../stdlib/list) | `'a list -> 'a list -> 'a list` |
| [`List.choose`](../stdlib/list) | `('a -> 'b option) -> 'a list -> 'b list` |
| [`List.collect`](../stdlib/list) | `('a -> 'b list) -> 'a list -> 'b list` |
| [`List.contains`](../stdlib/list) | `'a -> 'a list -> bool` |
| [`List.distinct`](../stdlib/list) | `'a list -> 'a list` |
| [`List.empty`](../stdlib/list) | `'a list` |
| [`List.exists`](../stdlib/list) | `('a -> bool) -> 'a list -> bool` |
| [`List.filter`](../stdlib/list) | `('a -> bool) -> 'a list -> 'a list` |
| [`List.fold`](../stdlib/list) | `('b -> 'a -> 'b) -> 'b -> 'a list -> 'b` |
| [`List.iter`](../stdlib/list) | `('a -> unit) -> 'a list -> unit` |
| [`List.length`](../stdlib/list) | `'a list -> int` |
| [`List.map`](../stdlib/list) | `('a -> 'b) -> 'a list -> 'b list` |
| [`List.rev`](../stdlib/list) | `'a list -> 'a list` |
| [`List.tail`](../stdlib/list) | `'a list -> 'a list` |
| [`List.tryFind`](../stdlib/list) | `('a -> bool) -> 'a list -> 'a option` |
| [`List.tryGet`](../stdlib/list) | `('a -> bool) -> 'a list -> int option` |
| [`List.tryHead`](../stdlib/list) | `'a list -> 'a option` |
| [`List.tryItem`](../stdlib/list) | `int -> 'a list -> 'a option` |

### Map

| Name | Signature |
| --- | --- |
| [`Map.add`](../stdlib/map) | `'b -> 'a -> 'a map['b] -> 'a map['b]` |
| [`Map.choose`](../stdlib/map) | `('b -> 'a -> 'c option) -> 'a map['b] -> 'c map['b]` |
| [`Map.containsKey`](../stdlib/map) | `'b -> 'a map['b] -> bool` |
| [`Map.count`](../stdlib/map) | `'a map['b] -> int` |
| [`Map.empty`](../stdlib/map) | `'a map['b]` |
| [`Map.filter`](../stdlib/map) | `('b -> 'a -> bool) -> 'a map['b] -> 'a map['b]` |
| [`Map.fold`](../stdlib/map) | `('c -> 'b -> 'a -> 'c) -> 'c -> 'a map['b] -> 'c` |
| [`Map.iter`](../stdlib/map) | `('b -> 'a -> unit) -> 'a map['b] -> unit` |
| [`Map.map`](../stdlib/map) | `('a -> 'c) -> 'a map['b] -> 'c map['b]` |
| [`Map.ofList`](../stdlib/map) | `('b * 'a) list -> 'a map['b]` |
| [`Map.remove`](../stdlib/map) | `'b -> 'a map['b] -> 'a map['b]` |
| [`Map.tryGet`](../stdlib/map) | `'b -> 'a map['b] -> 'a option` |

### Option

| Name | Signature |
| --- | --- |
| [`Option.defaultValue`](../stdlib/option) | `'a -> 'a option -> 'a` |
| [`Option.defaultWith`](../stdlib/option) | `(unit -> 'a) -> 'a option -> 'a` |
| [`Option.isNone`](../stdlib/option) | `'a option -> bool` |
| [`Option.isSome`](../stdlib/option) | `'a option -> bool` |
| [`Option.map`](../stdlib/option) | `('a -> 'b) -> 'a option -> 'b option` |

### String

| Name | Signature |
| --- | --- |
| [`String.concat`](../stdlib/string) | `string -> string list -> string` |
| [`String.endsWith`](../stdlib/string) | `string -> string -> bool` |
| [`String.indexOf`](../stdlib/string) | `string -> string -> int option` |
| [`String.replace`](../stdlib/string) | `string -> string -> string -> string` |
| [`String.split`](../stdlib/string) | `string -> string -> string list` |
| [`String.substring`](../stdlib/string) | `int -> int -> string -> string option` |
| [`String.toLower`](../stdlib/string) | `string -> string` |
| [`String.toUpper`](../stdlib/string) | `string -> string` |

## Default .NET runtime

### Console

| Name | Signature |
| --- | --- |
| [`Console.readLine`](../stdlib/console) | `unit -> string option` |
| [`Console.writeLine`](../stdlib/console) | `string -> unit` |

### Fs

| Name | Signature |
| --- | --- |
| [`Fs.combinePath`](../stdlib/fs) | `string -> string -> string` |
| [`Fs.createDirectory`](../stdlib/fs) | `string -> bool` |
| [`Fs.enumerateFiles`](../stdlib/fs) | `string -> string -> string list option` |
| [`Fs.exists`](../stdlib/fs) | `string -> bool` |
| [`Fs.extension`](../stdlib/fs) | `string -> string option` |
| [`Fs.fileNameWithoutExtension`](../stdlib/fs) | `string -> string` |
| [`Fs.glob`](../stdlib/fs) | `string -> string list option` |
| [`Fs.kind`](../stdlib/fs) | `string -> FsKind` |
| [`Fs.parentDirectory`](../stdlib/fs) | `string -> string option` |
| [`Fs.readText`](../stdlib/fs) | `string -> string option` |
| [`Fs.writeText`](../stdlib/fs) | `string -> string -> bool` |

### Guid

| Name | Signature |
| --- | --- |
| [`Guid.new`](../stdlib/guid) | `'a -> string option` |

### Hash

| Name | Signature |
| --- | --- |
| [`Hash.md5`](../stdlib/hash) | `string -> string option` |

### Json

| Name | Signature |
| --- | --- |
| [`Json.deserialize`](../stdlib/json) | `type -> string -> 'a option` |
| [`Json.serialize`](../stdlib/json) | `'a -> string option` |

### Regex

| Name | Signature |
| --- | --- |
| [`Regex.matchGroups`](../stdlib/regex) | `string -> string -> string list option` |

### Task

| Name | Signature |
| --- | --- |
| [`Task.await`](../stdlib/task) | `'a task -> 'a` |
| [`Task.spawn`](../stdlib/task) | `(unit -> 'a) -> 'a task` |

### Xml

| Name | Signature |
| --- | --- |
| [`Xml.queryValues`](../stdlib/xml) | `string -> string -> string list option` |

## Syntax and data forms

Constructors and syntax are separate from function registration: `Some`/`None`, union cases, literals, ranges, field access, list/map indexers, `typeof`, and `nameof` are covered in [Native Types](./native-types-reference.md) and [Syntax and Operators](./syntax-reference.md). Host-provided type descriptors are covered in [Host-Provided Types](../embedding/host-managed-types).

`Json.deserialize` uses a runtime type token; its generic result scheme does not statically encode the relationship between that token and the result. `Guid.new` accepts an ignored argument; use `Guid.new ()`. `List.contains` compares scalars only; use `List.exists` with `=` for composite data.

`Env.setExitCode : int -> unit` selects the eventual shell status without stopping evaluation; see [core builtins](../stdlib/builtins#envsetexitcode). It is effectful and accepts codes `0..255`.
