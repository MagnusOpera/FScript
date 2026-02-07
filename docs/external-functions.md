# External Functions and Extensibility Specification

## Purpose
This document describes the host external-function model, built-in extern catalog, and extension points.

## External function model

An external function is represented by:
- `Name: string`
- `Scheme: Scheme` (polymorphic type scheme)
- `Arity: int`
- `Impl: Value list -> Value`

Registry entrypoint:
- `FScript.Host.Registry.all : HostContext -> ExternalFunction list`

Interpreter integration:
- Extern schemes are injected into type inference environment.
- Extern values are injected into runtime environment as curried `VExternal`.

## Runtime-handled externs
Some externs declare a scheme in host code but are executed in evaluator runtime logic (for closure/function callbacks).  
These are currently dispatched by `ext.Name` in `Eval.applyFunctionValue`.

Current runtime-handled groups include:
- `List.map`, `List.iter`, `List.choose`, `List.collect`, `List.contains`, `List.distinct`, `List.exists`, `List.fold`, `List.filter`, `List.tryFind`, `List.tryFindIndex`
- `Option.map`, `Option.defaultWith`

## Built-in extern catalog

### Console
- `print : string -> unit`

### Filesystem
- `Fs.readText : string -> string option`
- `Fs.glob : string -> string list option`

### Regex
- `Regex.matchGroups : string -> string -> string list option`

### Hash and GUID
- `Hash.md5 : string -> string option`
- `Guid.new : 'a -> string option` (dummy argument to preserve call shape)

### Map (`'a map`)
- `Map.empty`
- `Map.add`
- `Map.try` (alias of `Map.tryFind`)
- `Map.tryFind`
- `Map.containsKey`
- `Map.remove`

### List
- `List.map`
- `List.choose`
- `List.collect`
- `List.contains`
- `List.distinct`
- `List.exists`
- `List.fold`
- `List.filter`
- `List.iter`
- `List.rev`
- `List.length`
- `List.tryFind`
- `List.tryFindIndex`
- `List.tryHead`
- `List.tail`
- `List.append`

### Option
- `Option.get`
- `Option.defaultValue`
- `Option.defaultWith`
- `Option.isNone`
- `Option.isSome`
- `Option.map`

### Typed decoders
- `Json.deserialize : type -> string -> 'a option`
- `Xml.values : type -> string -> string -> 'a list option`

## Error and option behavior
- Type/arity mismatch in extern calls raises eval errors.
- Many data/IO externs return `None` on operational failures (parse/IO/regex failures), rather than throwing.

## Extending with new externs
Recommended steps:
1. Add extern in a host module (`src/FScript.Host/*Externs.fs`) with `Name/Scheme/Arity/Impl`.
2. Register in `Registry.all`.
3. If extern requires host callback/function argument behavior, add runtime branch in `Eval.applyFunctionValue`.
4. Add tests in:
   - `tests/FScript.Host.Tests` for direct module behavior.
   - `tests/FScript.Core.Tests/HostExternTests.fs` for interpreter integration.

## Compatibility guidance
- External names are part of source-level API; renaming is breaking.
- Maintain explicit aliases only when needed for compatibility (example: `Map.try` / `Map.tryFind`).
