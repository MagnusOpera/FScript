---
id: stdlib-reference
title: Standard Library Signature Index
slug: /reference/stdlib
---

This is the complete catalog of core functions/values and default .NET runtime externs. Each name links to argument order, examples, and failure behavior. Type variables are local to each signature; repeated variables in a row must agree.

**Core** functions are always available. **Runtime** functions require host registration, are enabled by default in the CLI, and are disabled by `--no-default-externs` or `runtime.default-externs = false`. JavaScript hosts provide their own externs. Registered custom providers add functions beyond this standard catalog. Click a permission tag to read its definition. **None** means that the function requires no capabilities. The **Host permissions** column applies only to hosted interpretation: hosts grant none by default. The CLI and compilation are unrestricted by this policy. Provider FFI requires `Native`; callbacks retain their own requirements. See [Host permissions](../embedding/host-permissions).

## Top-level surface

| Name | Type | Availability | Host permissions |
| --- | --- | --- | --- |
| [`ignore`](../stdlib/builtins) | `'a -> unit` | Core function; evaluates and discards its argument | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Env`](../stdlib/builtins) | `Environment` | Value injected by CLI/REPL, optional in embedding hosts | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Environment`](../stdlib/builtins) | `{ ScriptName: string option; Arguments: string list }` | Built-in record type | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`FsKind`](../stdlib/builtins) | `File of string` / `Directory of string` / `Missing` | Built-in union type | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

## Core library

### Bool

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Bool.toString`](../stdlib/scalars) | `bool -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Bool.tryParse`](../stdlib/scalars) | `string -> bool option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Float

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Float.toString`](../stdlib/scalars) | `float -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.tryParse`](../stdlib/scalars) | `string -> float option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Int

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Int.toString`](../stdlib/scalars) | `int -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Int.tryParse`](../stdlib/scalars) | `string -> int option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### List

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`List.append`](../stdlib/list) | `'a list -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.choose`](../stdlib/list) | `('a -> 'b option) -> 'a list -> 'b list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.collect`](../stdlib/list) | `('a -> 'b list) -> 'a list -> 'b list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.contains`](../stdlib/list) | `'a -> 'a list -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.distinct`](../stdlib/list) | `'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.empty`](../stdlib/list) | `'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.exists`](../stdlib/list) | `('a -> bool) -> 'a list -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.filter`](../stdlib/list) | `('a -> bool) -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.fold`](../stdlib/list) | `('b -> 'a -> 'b) -> 'b -> 'a list -> 'b` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.iter`](../stdlib/list) | `('a -> unit) -> 'a list -> unit` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.length`](../stdlib/list) | `'a list -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.map`](../stdlib/list) | `('a -> 'b) -> 'a list -> 'b list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.rev`](../stdlib/list) | `'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.tail`](../stdlib/list) | `'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.tryFind`](../stdlib/list) | `('a -> bool) -> 'a list -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.tryGet`](../stdlib/list) | `('a -> bool) -> 'a list -> int option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.tryHead`](../stdlib/list) | `'a list -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.tryItem`](../stdlib/list) | `int -> 'a list -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Map

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Map.add`](../stdlib/map) | `'b -> 'a -> 'a map['b] -> 'a map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.choose`](../stdlib/map) | `('b -> 'a -> 'c option) -> 'a map['b] -> 'c map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.containsKey`](../stdlib/map) | `'b -> 'a map['b] -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.count`](../stdlib/map) | `'a map['b] -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.empty`](../stdlib/map) | `'a map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.filter`](../stdlib/map) | `('b -> 'a -> bool) -> 'a map['b] -> 'a map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.fold`](../stdlib/map) | `('c -> 'b -> 'a -> 'c) -> 'c -> 'a map['b] -> 'c` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.iter`](../stdlib/map) | `('b -> 'a -> unit) -> 'a map['b] -> unit` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.map`](../stdlib/map) | `('a -> 'c) -> 'a map['b] -> 'c map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.ofList`](../stdlib/map) | `('b * 'a) list -> 'a map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.remove`](../stdlib/map) | `'b -> 'a map['b] -> 'a map['b]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.tryGet`](../stdlib/map) | `'b -> 'a map['b] -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Option

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Option.defaultValue`](../stdlib/option) | `'a -> 'a option -> 'a` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.defaultWith`](../stdlib/option) | `(unit -> 'a) -> 'a option -> 'a` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.isNone`](../stdlib/option) | `'a option -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.isSome`](../stdlib/option) | `'a option -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.map`](../stdlib/option) | `('a -> 'b) -> 'a option -> 'b option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### String

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`String.concat`](../stdlib/string) | `string -> string list -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.endsWith`](../stdlib/string) | `string -> string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.indexOf`](../stdlib/string) | `string -> string -> int option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.replace`](../stdlib/string) | `string -> string -> string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.split`](../stdlib/string) | `string -> string -> string list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.substring`](../stdlib/string) | `int -> int -> string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.toLower`](../stdlib/string) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.toUpper`](../stdlib/string) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

## Default .NET runtime

### Console

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Console.readLine`](../stdlib/console) | `unit -> string option` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> <a className="permission-tag permission-tag--interactive" href="../embedding/host-permissions#interactive" title="Interactive permission definition">Interactive</a> |
| [`Console.writeLine`](../stdlib/console) | `string -> unit` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |

### Fs

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Fs.combinePath`](../stdlib/fs) | `string -> string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Fs.createDirectory`](../stdlib/fs) | `string -> bool` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| [`Fs.enumerateFiles`](../stdlib/fs) | `string -> string -> string list option` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| [`Fs.exists`](../stdlib/fs) | `string -> bool` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| [`Fs.extension`](../stdlib/fs) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Fs.fileNameWithoutExtension`](../stdlib/fs) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Fs.glob`](../stdlib/fs) | `string -> string list option` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| [`Fs.kind`](../stdlib/fs) | `string -> FsKind` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| [`Fs.parentDirectory`](../stdlib/fs) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Fs.readText`](../stdlib/fs) | `string -> string option` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| [`Fs.writeText`](../stdlib/fs) | `string -> string -> bool` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |

### Guid

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Guid.new`](../stdlib/guid) | `'a -> string option` | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |

### Hash

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Hash.md5`](../stdlib/hash) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Json

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Json.deserialize`](../stdlib/json) | `type -> string -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Json.serialize`](../stdlib/json) | `'a -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Regex

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Regex.matchGroups`](../stdlib/regex) | `string -> string -> string list option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Task

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Task.spawn`](../stdlib/task#taskspawn) | `(unit -> 'a) -> 'a task` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.delay`](../stdlib/task#taskdelay) | `int -> unit task` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.fromResult`](../stdlib/task#taskfromresult) | `'a -> 'a task` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.continueWith`](../stdlib/task#taskcontinuewith) | `('a -> 'b) -> 'a task -> 'b task` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.whenAll`](../stdlib/task#taskwhenall) | `('a list -> 'b) -> 'a task list -> 'b task` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.whenAny`](../stdlib/task#taskwhenany) | `('a -> 'b) -> 'a task list -> 'b task` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.await`](../stdlib/task#taskawait) | `'a task -> 'a` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |
| [`Task.awaitAll`](../stdlib/task#taskawaitall) | `'a task list -> 'a list` | <a className="permission-tag permission-tag--threading" href="../embedding/host-permissions#threading" title="Threading permission definition">Threading</a> |

### Xml

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Xml.queryValues`](../stdlib/xml) | `string -> string -> string list option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

## Syntax and data forms

Constructors and syntax are separate from function registration: `Some`/`None`, union cases, literals, ranges, field access, list/map indexers, `typeof`, and `nameof` are covered in [Native Types](./native-types-reference.md) and [Syntax and Operators](./syntax-reference.md). Host-provided type descriptors are covered in [Host-Provided Types](../embedding/host-managed-types).

`Json.deserialize` uses a runtime type token; its generic result scheme does not statically encode the relationship between that token and the result. `Guid.new` accepts an ignored argument; use `Guid.new ()`. `List.contains` compares scalars only; use `List.exists` with `=` for composite data.

`Env.setExitCode : int -> unit` selects the eventual shell status without stopping evaluation; see [core builtins](../stdlib/builtins#envsetexitcode). It is effectful and accepts codes `0..255`.

## Expanded pure support library

Path, Glob, SemVer, scalar, and collection additions are core builtins. Encoding and SHA-256 are pure .NET runtime externs and need host registration. See each module for ordering, errors, and platform rules.

### Int additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Int.abs`](../stdlib/scalars#intabs) | `int -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Int.sign`](../stdlib/scalars#intsign) | `int -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Int.min`](../stdlib/scalars#intmin) | `int -> int -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Int.max`](../stdlib/scalars#intmax) | `int -> int -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Int.clamp`](../stdlib/scalars#intclamp) | `int -> int -> int -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Int.toFloat`](../stdlib/scalars#inttofloat) | `int -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Float additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Float.abs`](../stdlib/scalars#floatabs) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.sign`](../stdlib/scalars#floatsign) | `float -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.min`](../stdlib/scalars#floatmin) | `float -> float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.max`](../stdlib/scalars#floatmax) | `float -> float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.clamp`](../stdlib/scalars#floatclamp) | `float -> float -> float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.floor`](../stdlib/scalars#floatfloor) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.ceil`](../stdlib/scalars#floatceil) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.round`](../stdlib/scalars#floatround) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.truncate`](../stdlib/scalars#floattruncate) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.sqrt`](../stdlib/scalars#floatsqrt) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.log`](../stdlib/scalars#floatlog) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.log10`](../stdlib/scalars#floatlog10) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.exp`](../stdlib/scalars#floatexp) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.sin`](../stdlib/scalars#floatsin) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.cos`](../stdlib/scalars#floatcos) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.tan`](../stdlib/scalars#floattan) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.asin`](../stdlib/scalars#floatasin) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.acos`](../stdlib/scalars#floatacos) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.atan`](../stdlib/scalars#floatatan) | `float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.pow`](../stdlib/scalars#floatpow) | `float -> float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.atan2`](../stdlib/scalars#floatatan2) | `float -> float -> float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.isNaN`](../stdlib/scalars#floatisnan) | `float -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.isInfinity`](../stdlib/scalars#floatisinfinity) | `float -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.isFinite`](../stdlib/scalars#floatisfinite) | `float -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.pi`](../stdlib/scalars#floatpi) | `float` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Float.tryToInt`](../stdlib/scalars#floattrytoint) | `float -> int option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### String additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`String.compare`](../stdlib/string#stringcompare) | `string -> string -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.length`](../stdlib/string#stringlength) | `string -> int` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.isEmpty`](../stdlib/string#stringisempty) | `string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.isWhiteSpace`](../stdlib/string#stringiswhitespace) | `string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.trim`](../stdlib/string#stringtrim) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.trimStart`](../stdlib/string#stringtrimstart) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.trimEnd`](../stdlib/string#stringtrimend) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.splitLines`](../stdlib/string#stringsplitlines) | `string -> string list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.startsWith`](../stdlib/string#stringstartswith) | `string -> string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.contains`](../stdlib/string#stringcontains) | `string -> string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.padLeft`](../stdlib/string#stringpadleft) | `int -> string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.padRight`](../stdlib/string#stringpadright) | `int -> string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`String.repeat`](../stdlib/string#stringrepeat) | `int -> string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### List additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`List.isEmpty`](../stdlib/list#listisempty) | `'a list -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.forall`](../stdlib/list#listforall) | `('a -> bool) -> 'a list -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.mapi`](../stdlib/list#listmapi) | `(int -> 'a -> 'b) -> 'a list -> 'b list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.indexed`](../stdlib/list#listindexed) | `'a list -> ((int * 'a)) list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.init`](../stdlib/list#listinit) | `int -> (int -> 'a) -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.take`](../stdlib/list#listtake) | `int -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.skip`](../stdlib/list#listskip) | `int -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.takeWhile`](../stdlib/list#listtakewhile) | `('a -> bool) -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.skipWhile`](../stdlib/list#listskipwhile) | `('a -> bool) -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.partition`](../stdlib/list#listpartition) | `('a -> bool) -> 'a list -> ('a list * 'a list)` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.zip`](../stdlib/list#listzip) | `'a list -> 'b list -> (('a * 'b)) list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.unzip`](../stdlib/list#listunzip) | `(('a * 'b)) list -> ('a list * 'b list)` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.chunkBySize`](../stdlib/list#listchunkbysize) | `int -> 'a list -> 'a list list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.scan`](../stdlib/list#listscan) | `('d -> 'a -> 'd) -> 'd -> 'a list -> 'd list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.sortWith`](../stdlib/list#listsortwith) | `('a -> 'a -> int) -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.sortBy`](../stdlib/list#listsortby) | `('a -> 'c) -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.distinctBy`](../stdlib/list#listdistinctby) | `('a -> 'c) -> 'a list -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.groupBy`](../stdlib/list#listgroupby) | `('a -> 'c) -> 'a list -> (('c * 'a list)) list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`List.countBy`](../stdlib/list#listcountby) | `('a -> 'c) -> 'a list -> (('c * int)) list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Map additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Map.keys`](../stdlib/map#mapkeys) | `'a map['c] -> 'c list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.values`](../stdlib/map#mapvalues) | `'a map['c] -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.toList`](../stdlib/map#maptolist) | `'a map['c] -> (('c * 'a)) list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.exists`](../stdlib/map#mapexists) | `('c -> 'a -> bool) -> 'a map['c] -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.forall`](../stdlib/map#mapforall) | `('c -> 'a -> bool) -> 'a map['c] -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.update`](../stdlib/map#mapupdate) | `'c -> ('a option -> 'a option) -> 'a map['c] -> 'a map['c]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Map.mergeWith`](../stdlib/map#mapmergewith) | `('c -> 'a -> 'a -> 'a) -> 'a map['c] -> 'a map['c] -> 'a map['c]` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Option additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Option.bind`](../stdlib/option#optionbind) | `('a -> 'b option) -> 'a option -> 'b option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.flatten`](../stdlib/option#optionflatten) | `'a option option -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.filter`](../stdlib/option#optionfilter) | `('a -> bool) -> 'a option -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.orElse`](../stdlib/option#optionorelse) | `'a option -> 'a option -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.orElseWith`](../stdlib/option#optionorelsewith) | `(unit -> 'a option) -> 'a option -> 'a option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.map2`](../stdlib/option#optionmap2) | `('a -> 'b -> 'd) -> 'a option -> 'b option -> 'd option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Option.toList`](../stdlib/option#optiontolist) | `'a option -> 'a list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Path additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Path.normalize`](../stdlib/path#pathnormalize) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.isAbsolute`](../stdlib/path#pathisabsolute) | `string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.combine`](../stdlib/path#pathcombine) | `string -> string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.fileName`](../stdlib/path#pathfilename) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.extension`](../stdlib/path#pathextension) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.fileNameWithoutExtension`](../stdlib/path#pathfilenamewithoutextension) | `string -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.parent`](../stdlib/path#pathparent) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Path.relative`](../stdlib/path#pathrelative) | `string -> string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Glob additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Glob.isMatch`](../stdlib/glob#globismatch) | `string -> string -> bool` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Glob.filter`](../stdlib/glob#globfilter) | `string -> string list -> string list` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### SemVer additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`SemVer.tryParse`](../stdlib/semver#semvertryparse) | `string -> ({ Build: string; Major: string; Minor: string; Patch: string; PreRelease: string }) option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`SemVer.format`](../stdlib/semver#semverformat) | `{ Build: string; Major: string; Minor: string; Patch: string; PreRelease: string } -> string` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`SemVer.compare`](../stdlib/semver#semvercompare) | `string -> string -> int option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Encoding additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Encoding.utf8Encode`](../stdlib/encoding#encodingutf8encode) | `string -> int list option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.utf8Decode`](../stdlib/encoding#encodingutf8decode) | `int list -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.base64Encode`](../stdlib/encoding#encodingbase64encode) | `int list -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.base64Decode`](../stdlib/encoding#encodingbase64decode) | `string -> int list option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.hexEncode`](../stdlib/encoding#encodinghexencode) | `int list -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.hexDecode`](../stdlib/encoding#encodinghexdecode) | `string -> int list option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.uriEncode`](../stdlib/encoding#encodinguriencode) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Encoding.uriDecode`](../stdlib/encoding#encodinguridecode) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

### Hash additions

| Name | Signature | Host permissions |
| --- | --- | --- |
| [`Hash.sha256`](../stdlib/hash#hashsha256) | `string -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| [`Hash.sha256Bytes`](../stdlib/hash#hashsha256bytes) | `int list -> string option` | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
