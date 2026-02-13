# External Functions and Extensibility Specification

## Purpose
This document describes the host external-function model, built-in extern catalog, and extension points.

## External function model

An external function is represented by:
- `Name: string`
- `Scheme: Scheme` (polymorphic type scheme)
- `Arity: int`
- `Impl: ExternalCallContext -> Value list -> Value`

Registry entrypoint:
- `FScript.Runtime.Registry.all : HostContext -> ExternalFunction list`

Interpreter integration:
- Extern schemes are injected into type inference environment.
- Extern values are injected into runtime environment as curried `VExternal`.

## Higher-order extern execution
Higher-order externs are implemented in runtime extern modules and receive an `ExternalCallContext`.

`ExternalCallContext.Apply` is a function pointer provided by the evaluator so extern code can apply script closures and curried functions without re-implementing evaluation.

## Built-in extern catalog

### Filesystem
- `Fs.readText : string -> string option`
- `Fs.exists : string -> bool`
- `Fs.isFile : string -> bool`
- `Fs.isDirectory : string -> bool`
- `Fs.createDirectory : string -> bool`
- `Fs.writeText : string -> string -> bool`
- `Fs.combinePath : string -> string -> string`
- `Fs.parentDirectory : string -> string option`
- `Fs.extension : string -> string option`
- `Fs.fileNameWithoutExtension : string -> string`
- `Fs.glob : string -> string list option`
- `Fs.enumerateFiles : string -> string -> string list option`

### Regex
- `Regex.matchGroups : string -> string -> string list option`

### Hash and GUID
- `Hash.md5 : string -> string option`
- `Guid.new : 'a -> string option` (dummy argument to preserve call shape)

### Collections and prelude
- `List.*`, `Option.*`, and `Map.*` helpers are provided by the embedded prelude in `FScript.Language`.
- `print : string -> unit` is a built-in language function (not a host extern).
- Runtime externs focus on host/system capabilities.
- See `docs/stdlib-functions.md` for the full stdlib function reference.

### Typed decoders
- `Json.deserialize : type -> string -> 'a option`
- `Xml.values : type -> string -> string -> 'a list option`

## Error and option behavior
- Type/arity mismatch in extern calls raises eval errors.
- Many data/IO externs return `None` on operational failures (parse/IO/regex failures), rather than throwing.

## Extending with new externs
Recommended steps:
1. Add extern in a host module (`src/FScript.Runtime/*Externs.fs`) with `Name/Scheme/Arity/Impl`.
2. Register in `Registry.all`.
3. For higher-order externs, use `ExternalCallContext.Apply` from `Impl` to invoke script functions.
4. Add tests in:
   - `tests/FScript.Runtime.Tests` for direct module behavior.
   - `tests/FScript.Language.Tests/HostExternTests.fs` for interpreter integration.

## Compatibility guidance
- External names are part of source-level API; renaming is breaking.
