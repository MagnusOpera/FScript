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

## Expanded pure support library

Path, Glob, SemVer, scalar, and collection additions are core builtins. Encoding and SHA-256 are pure .NET runtime externs and need host registration. See each module for ordering, errors, and platform rules.

### Int additions

| Name | Signature |
| --- | --- |
| [`Int.abs`](../stdlib/scalars#intabs) | `int -> int` |
| [`Int.sign`](../stdlib/scalars#intsign) | `int -> int` |
| [`Int.min`](../stdlib/scalars#intmin) | `int -> int -> int` |
| [`Int.max`](../stdlib/scalars#intmax) | `int -> int -> int` |
| [`Int.clamp`](../stdlib/scalars#intclamp) | `int -> int -> int -> int` |
| [`Int.toFloat`](../stdlib/scalars#inttofloat) | `int -> float` |

### Float additions

| Name | Signature |
| --- | --- |
| [`Float.abs`](../stdlib/scalars#floatabs) | `float -> float` |
| [`Float.sign`](../stdlib/scalars#floatsign) | `float -> int` |
| [`Float.min`](../stdlib/scalars#floatmin) | `float -> float -> float` |
| [`Float.max`](../stdlib/scalars#floatmax) | `float -> float -> float` |
| [`Float.clamp`](../stdlib/scalars#floatclamp) | `float -> float -> float -> float` |
| [`Float.floor`](../stdlib/scalars#floatfloor) | `float -> float` |
| [`Float.ceil`](../stdlib/scalars#floatceil) | `float -> float` |
| [`Float.round`](../stdlib/scalars#floatround) | `float -> float` |
| [`Float.truncate`](../stdlib/scalars#floattruncate) | `float -> float` |
| [`Float.sqrt`](../stdlib/scalars#floatsqrt) | `float -> float` |
| [`Float.log`](../stdlib/scalars#floatlog) | `float -> float` |
| [`Float.log10`](../stdlib/scalars#floatlog10) | `float -> float` |
| [`Float.exp`](../stdlib/scalars#floatexp) | `float -> float` |
| [`Float.sin`](../stdlib/scalars#floatsin) | `float -> float` |
| [`Float.cos`](../stdlib/scalars#floatcos) | `float -> float` |
| [`Float.tan`](../stdlib/scalars#floattan) | `float -> float` |
| [`Float.asin`](../stdlib/scalars#floatasin) | `float -> float` |
| [`Float.acos`](../stdlib/scalars#floatacos) | `float -> float` |
| [`Float.atan`](../stdlib/scalars#floatatan) | `float -> float` |
| [`Float.pow`](../stdlib/scalars#floatpow) | `float -> float -> float` |
| [`Float.atan2`](../stdlib/scalars#floatatan2) | `float -> float -> float` |
| [`Float.isNaN`](../stdlib/scalars#floatisnan) | `float -> bool` |
| [`Float.isInfinity`](../stdlib/scalars#floatisinfinity) | `float -> bool` |
| [`Float.isFinite`](../stdlib/scalars#floatisfinite) | `float -> bool` |
| [`Float.pi`](../stdlib/scalars#floatpi) | `float` |
| [`Float.tryToInt`](../stdlib/scalars#floattrytoint) | `float -> int option` |

### String additions

| Name | Signature |
| --- | --- |
| [`String.compare`](../stdlib/string#stringcompare) | `string -> string -> int` |
| [`String.length`](../stdlib/string#stringlength) | `string -> int` |
| [`String.isEmpty`](../stdlib/string#stringisempty) | `string -> bool` |
| [`String.isWhiteSpace`](../stdlib/string#stringiswhitespace) | `string -> bool` |
| [`String.trim`](../stdlib/string#stringtrim) | `string -> string` |
| [`String.trimStart`](../stdlib/string#stringtrimstart) | `string -> string` |
| [`String.trimEnd`](../stdlib/string#stringtrimend) | `string -> string` |
| [`String.splitLines`](../stdlib/string#stringsplitlines) | `string -> string list` |
| [`String.startsWith`](../stdlib/string#stringstartswith) | `string -> string -> bool` |
| [`String.contains`](../stdlib/string#stringcontains) | `string -> string -> bool` |
| [`String.padLeft`](../stdlib/string#stringpadleft) | `int -> string -> string` |
| [`String.padRight`](../stdlib/string#stringpadright) | `int -> string -> string` |
| [`String.repeat`](../stdlib/string#stringrepeat) | `int -> string -> string` |

### List additions

| Name | Signature |
| --- | --- |
| [`List.isEmpty`](../stdlib/list#listisempty) | `'a list -> bool` |
| [`List.forall`](../stdlib/list#listforall) | `('a -> bool) -> 'a list -> bool` |
| [`List.mapi`](../stdlib/list#listmapi) | `(int -> 'a -> 'b) -> 'a list -> 'b list` |
| [`List.indexed`](../stdlib/list#listindexed) | `'a list -> ((int * 'a)) list` |
| [`List.init`](../stdlib/list#listinit) | `int -> (int -> 'a) -> 'a list` |
| [`List.take`](../stdlib/list#listtake) | `int -> 'a list -> 'a list` |
| [`List.skip`](../stdlib/list#listskip) | `int -> 'a list -> 'a list` |
| [`List.takeWhile`](../stdlib/list#listtakewhile) | `('a -> bool) -> 'a list -> 'a list` |
| [`List.skipWhile`](../stdlib/list#listskipwhile) | `('a -> bool) -> 'a list -> 'a list` |
| [`List.partition`](../stdlib/list#listpartition) | `('a -> bool) -> 'a list -> ('a list * 'a list)` |
| [`List.zip`](../stdlib/list#listzip) | `'a list -> 'b list -> (('a * 'b)) list` |
| [`List.unzip`](../stdlib/list#listunzip) | `(('a * 'b)) list -> ('a list * 'b list)` |
| [`List.chunkBySize`](../stdlib/list#listchunkbysize) | `int -> 'a list -> 'a list list` |
| [`List.scan`](../stdlib/list#listscan) | `('d -> 'a -> 'd) -> 'd -> 'a list -> 'd list` |
| [`List.sortWith`](../stdlib/list#listsortwith) | `('a -> 'a -> int) -> 'a list -> 'a list` |
| [`List.sortBy`](../stdlib/list#listsortby) | `('a -> 'c) -> 'a list -> 'a list` |
| [`List.distinctBy`](../stdlib/list#listdistinctby) | `('a -> 'c) -> 'a list -> 'a list` |
| [`List.groupBy`](../stdlib/list#listgroupby) | `('a -> 'c) -> 'a list -> (('c * 'a list)) list` |
| [`List.countBy`](../stdlib/list#listcountby) | `('a -> 'c) -> 'a list -> (('c * int)) list` |

### Map additions

| Name | Signature |
| --- | --- |
| [`Map.keys`](../stdlib/map#mapkeys) | `'a map['c] -> 'c list` |
| [`Map.values`](../stdlib/map#mapvalues) | `'a map['c] -> 'a list` |
| [`Map.toList`](../stdlib/map#maptolist) | `'a map['c] -> (('c * 'a)) list` |
| [`Map.exists`](../stdlib/map#mapexists) | `('c -> 'a -> bool) -> 'a map['c] -> bool` |
| [`Map.forall`](../stdlib/map#mapforall) | `('c -> 'a -> bool) -> 'a map['c] -> bool` |
| [`Map.update`](../stdlib/map#mapupdate) | `'c -> ('a option -> 'a option) -> 'a map['c] -> 'a map['c]` |
| [`Map.mergeWith`](../stdlib/map#mapmergewith) | `('c -> 'a -> 'a -> 'a) -> 'a map['c] -> 'a map['c] -> 'a map['c]` |

### Option additions

| Name | Signature |
| --- | --- |
| [`Option.bind`](../stdlib/option#optionbind) | `('a -> 'b option) -> 'a option -> 'b option` |
| [`Option.flatten`](../stdlib/option#optionflatten) | `'a option option -> 'a option` |
| [`Option.filter`](../stdlib/option#optionfilter) | `('a -> bool) -> 'a option -> 'a option` |
| [`Option.orElse`](../stdlib/option#optionorelse) | `'a option -> 'a option -> 'a option` |
| [`Option.orElseWith`](../stdlib/option#optionorelsewith) | `(unit -> 'a option) -> 'a option -> 'a option` |
| [`Option.map2`](../stdlib/option#optionmap2) | `('a -> 'b -> 'd) -> 'a option -> 'b option -> 'd option` |
| [`Option.toList`](../stdlib/option#optiontolist) | `'a option -> 'a list` |

### Path additions

| Name | Signature |
| --- | --- |
| [`Path.normalize`](../stdlib/path#pathnormalize) | `string -> string` |
| [`Path.isAbsolute`](../stdlib/path#pathisabsolute) | `string -> bool` |
| [`Path.combine`](../stdlib/path#pathcombine) | `string -> string -> string` |
| [`Path.fileName`](../stdlib/path#pathfilename) | `string -> string` |
| [`Path.extension`](../stdlib/path#pathextension) | `string -> string option` |
| [`Path.fileNameWithoutExtension`](../stdlib/path#pathfilenamewithoutextension) | `string -> string` |
| [`Path.parent`](../stdlib/path#pathparent) | `string -> string option` |
| [`Path.relative`](../stdlib/path#pathrelative) | `string -> string -> string option` |

### Glob additions

| Name | Signature |
| --- | --- |
| [`Glob.isMatch`](../stdlib/glob#globismatch) | `string -> string -> bool` |
| [`Glob.filter`](../stdlib/glob#globfilter) | `string -> string list -> string list` |

### SemVer additions

| Name | Signature |
| --- | --- |
| [`SemVer.tryParse`](../stdlib/semver#semvertryparse) | `string -> ({ Build: string; Major: string; Minor: string; Patch: string; PreRelease: string }) option` |
| [`SemVer.format`](../stdlib/semver#semverformat) | `{ Build: string; Major: string; Minor: string; Patch: string; PreRelease: string } -> string` |
| [`SemVer.compare`](../stdlib/semver#semvercompare) | `string -> string -> int option` |

### Encoding additions

| Name | Signature |
| --- | --- |
| [`Encoding.utf8Encode`](../stdlib/encoding#encodingutf8encode) | `string -> int list option` |
| [`Encoding.utf8Decode`](../stdlib/encoding#encodingutf8decode) | `int list -> string option` |
| [`Encoding.base64Encode`](../stdlib/encoding#encodingbase64encode) | `int list -> string option` |
| [`Encoding.base64Decode`](../stdlib/encoding#encodingbase64decode) | `string -> int list option` |
| [`Encoding.hexEncode`](../stdlib/encoding#encodinghexencode) | `int list -> string option` |
| [`Encoding.hexDecode`](../stdlib/encoding#encodinghexdecode) | `string -> int list option` |
| [`Encoding.uriEncode`](../stdlib/encoding#encodinguriencode) | `string -> string option` |
| [`Encoding.uriDecode`](../stdlib/encoding#encodinguridecode) | `string -> string option` |

### Hash additions

| Name | Signature |
| --- | --- |
| [`Hash.sha256`](../stdlib/hash#hashsha256) | `string -> string option` |
| [`Hash.sha256Bytes`](../stdlib/hash#hashsha256bytes) | `int list -> string option` |
