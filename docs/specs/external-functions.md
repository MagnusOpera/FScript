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
  - `HostContext` includes:
    - `RootDirectory : string`
    - `ExcludedPaths : string list` (absolute paths hidden/blocked inside root)
- Optional plugin protocol marker:
  - `[<FScript.Runtime.FScriptExternProvider>]` on `public static` provider methods in user assemblies
  - supported method signatures:
    - `unit -> ExternalFunction list`
    - `HostContext -> ExternalFunction list`

Interpreter integration:
- Extern schemes are injected into type inference environment.
- Extern values are injected into runtime environment as curried `VExternal`.

LSP integration:
- The language server injects runtime extern schemes (`Registry.all`) during document inference.
- Hover/completion/signature help expose callable signatures for injected runtime functions and prelude stdlib functions.
- Injected stdlib functions use named-argument signatures when parameter names are available.
- Go-to-definition for stdlib injected functions resolves to readonly virtual stdlib sources (`fscript-stdlib:///...`).

## Higher-order extern execution
Higher-order externs are implemented in runtime extern modules and receive an `ExternalCallContext`.

`ExternalCallContext.Apply` is a function pointer provided by the evaluator so extern code can apply script closures and curried functions without re-implementing evaluation.

## Built-in extern catalog

### Filesystem
- `Fs.readText : string -> string option`
- `Fs.exists : string -> bool`
- `Fs.kind : string -> FsKind`
- `Fs.createDirectory : string -> bool`
- `Fs.writeText : string -> string -> bool`
- `Fs.combinePath : string -> string -> string`
- `Fs.parentDirectory : string -> string option`
- `Fs.extension : string -> string option`
- `Fs.fileNameWithoutExtension : string -> string`
- `Fs.glob : string -> string list option`
- `Fs.enumerateFiles : string -> string -> string list option`
- Excluded path policy:
  - `Fs.glob` and `Fs.enumerateFiles` hide excluded entries.
  - `Fs.readText`, `Fs.writeText`, and `Fs.createDirectory` throw an eval error on excluded targets.
  - `Fs.exists` returns `false` and `Fs.kind` returns `FsKind.Missing` for excluded targets.

### Regex
- `Regex.matchGroups : string -> string -> string list option`

### Hash and GUID
- `Hash.md5 : string -> string option`
- `Guid.new : 'a -> string option` (dummy argument to preserve call shape)

### Collections and prelude
- `List.*`, `Option.*`, and `Map.*` helpers are provided by the embedded prelude in `FScript.Language`.
- Scalar conversion helpers are built into the language runtime:
  - `Int.tryParse : string -> int option`
  - `Float.tryParse : string -> float option`
  - `Bool.tryParse : string -> bool option`
  - `Int.toString : int -> string`
  - `Float.toString : float -> string`
  - `Bool.toString : bool -> string`
- `print : string -> unit` is a built-in language function (not a host extern).
- Runtime externs focus on host/system capabilities.
- `FsKind` is provided by stdlib as:
  - `FsKind.File of string`
  - `FsKind.Directory of string`
  - `FsKind.Missing`
- See [`stdlib-functions.md`](./stdlib-functions.md) for the full stdlib function reference.

### Typed decoders
- `Json.deserialize : type -> string -> 'a option`
- `Json.serialize : 'a -> string option`
- `Xml.deserialize : type -> string -> string -> 'a list option`
- `Xml.serialize : 'a -> string option`

## Error and option behavior
- Type/arity mismatch in extern calls raises eval errors.
- Many data/IO externs return `None` on operational failures (parse/IO/regex failures), rather than throwing.
- `Xml.deserialize` and `Xml.serialize` do not support XML namespaces.

## Extending with new externs
Recommended steps:
1. Add extern in a host module (`src/FScript.Runtime/*Externs.fs`) with `Name/Scheme/Arity/Impl`.
2. Register in `Registry.all`.
3. For higher-order externs, use `ExternalCallContext.Apply` from `Impl` to invoke script functions.
4. Add tests in:
   - `tests/FScript.Runtime.Tests` for direct module behavior.
   - `tests/FScript.Language.Tests/HostExternTests.fs` for interpreter integration.

## CLI-only extern assembly injection
- The `fscript` CLI can inject externs from user assemblies via:
  - `--extern-assembly <path>` (repeatable)
  - `--no-default-externs` (disables `Registry.all`)
- Provider discovery:
  - CLI loads each unique assembly path once.
  - It discovers methods annotated with `[<FScriptExternProvider>]`.
  - It invokes each discovered provider once per CLI process startup.
- Conflicts:
  - Any duplicate external function name across defaults + user providers is a fatal CLI error.
- Runtime constraints:
  - If runtime assembly loading is unavailable (for example native AOT), `--extern-assembly` is rejected with a clear CLI error.

## Compatibility guidance
- External names are part of source-level API; renaming is breaking.
