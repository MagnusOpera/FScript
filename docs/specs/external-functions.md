# External Functions and Extensibility Specification

## Purpose
This document describes the host external-function model, built-in extern catalog, and extension points.

## External function model

The native interpreter external function is represented by:
- `Name: string`
- `Scheme: Scheme` (polymorphic type scheme)
- `Arity: int`
- `Impl: ExternalCallContext -> Value list -> Value`

Registry entrypoint:
- `FScript.Runtime.Registry.all : HostContext -> ExternalFunction list`
  - `HostContext` includes:
    - `RootDirectory : string`
    - `DeniedPathGlobs : string list` (root-relative glob patterns hidden/blocked inside root)
- Optional plugin protocol marker:
  - `[<FScript.Extensibility.FScriptExternProvider>]` on `public static` provider methods in user assemblies
  - supported method signatures (all contract types below belong to `FScript.Extensibility`):
    - `unit -> ExternalFunction list`
    - `HostContext -> ExternalFunction list`
    - `unit -> ExtensionRegistration`
    - `HostContext -> ExtensionRegistration`

Interpreter integration:
- Extern schemes are injected into type inference environment.
- Extern values are injected into runtime environment as curried `VExternal`.

LSP integration:
- The language server injects runtime extern schemes (`Registry.all`) during document inference.
- Hover/completion/signature help expose callable signatures for injected runtime functions and prelude stdlib functions.
- Injected stdlib functions use named-argument signatures when parameter names are available.
- Go-to-definition for stdlib injected functions resolves to readonly virtual stdlib sources (`fscript-stdlib:///...`) for namespaced stdlib and runtime surfaces such as `Console.*`, `Task.*`, `Map.*`, and `List.*`.

## Typed F# handlers

`FScript.Extensibility.HostFunctions.create registration name handler` returns the same `ExternalFunction` record with a generated closed scheme, curried arity, and conversions shared with `HostTypes`. Concrete synchronous F# handlers do not need to repeat `Scheme`, `Arity`, or `Value` pattern matches. Registered host descriptors remain explicit. Manual records remain available for capabilities outside this adapter. See [Host function adapters](./host-functions.md) for signature validation and invocation semantics.

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
- `Fs.enumerateFiles` searches only the requested directory when its pattern contains no path separator; use a path pattern such as `**/*.fs` for recursive matching.
- Denied-path policy:
  - `Fs.glob` and `Fs.enumerateFiles` hide denied entries.
  - `Fs.glob` and `Fs.enumerateFiles` do not descend into denied or symlink/reparse-point directories.
  - `Fs.readText`, `Fs.writeText`, and `Fs.createDirectory` throw an eval error on denied targets.
  - `Fs.exists` returns `false` and `Fs.kind` returns `FsKind.Missing` for denied targets.
- Root-boundary policy:
  - Paths are normalized before root checks.
  - Symlink/reparse-point paths are treated as unsafe out-of-root paths.

### Regex
- `Regex.matchGroups : string -> string -> string list option`

Generated Go implements `Regex.matchGroups` using Go's regular-expression engine. FScript non-capturing groups `(?:...)` are translated while preserving the returned list of capturing groups; invalid or unsupported expressions return `None`.

### Hash and GUID
- `Hash.md5 : string -> string option`
- `Guid.new : 'a -> string option` (dummy argument to preserve call shape)

### Console
- `Console.writeLine : string -> unit`
- `Console.readLine : unit -> string option`

### Task
- `Task.spawn : (unit -> 'a) -> 'a task`
- `Task.await : 'a task -> 'a`

Generated Go specializes `'a` and represents the task with a private typed completion channel. Channels are not an FScript type and are not exposed by the generated program API.
- These are part of the default runtime extern registry, so hosts may omit them.

### Collections and prelude
- `List.*`, `Option.*`, and `Map.*` helpers are provided by the embedded prelude in `FScript.Language`.
- Scalar conversion helpers are built into the language runtime:
  - `Int.tryParse : string -> int option`
  - `Float.tryParse : string -> float option`
  - `Bool.tryParse : string -> bool option`
  - `Int.toString : int -> string`
  - `Float.toString : float -> string`
  - `Bool.toString : bool -> string`
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
- Runtime externs focus on host/system capabilities.
- `FsKind` is provided by stdlib as:
  - `FsKind.File of string`
  - `FsKind.Directory of string`
  - `FsKind.Missing`
- See [`stdlib-functions.md`](./stdlib-functions.md) for the full stdlib function reference.

### Serialization and XML queries
- `Json.deserialize : type -> string -> 'a option`
- `Json.serialize : 'a -> string option`
  - encoding supports scalars, unit/options, lists, tuples, records, visible host-managed fields, and maps with string/int/bool keys
  - unsupported nested values (including unions, functions, tasks, and union-keyed maps) return `None`
- JSON decoding uses the runtime token, with missing option fields mapped to `None` and extra record properties ignored; unsupported targets, including opaque host-managed types, cannot be manufactured
- The generic decoder scheme does not statically associate its token with the result; receiving functions can declare the intended return type
- `Xml.queryValues : string -> string -> string list option`
  - argument order: `query -> xmldoc`
  - query subset: `.../@attr` and `.../text()` selectors only
  - supports simple element paths with optional leading `//` descendant step
  - returns `Some []` when query is valid but no values match
  - returns `None` for XML parse failure or invalid/unsupported query syntax

## Error and option behavior
- Type/arity mismatch in extern calls raises eval errors.
- Options describe supported absence/conversion failures, not a universal exception boundary. JSON parsing and regex failures return `None`; denied filesystem reads/writes raise errors, and reading an existing unreadable file can propagate I/O errors.
- `Xml.queryValues` does not support XML namespaces.

## Extending with new externs
Recommended steps:
1. Add extern in a host module (`src/FScript.Runtime/*Externs.fs`) with `Name/Scheme/Arity/Impl`.
2. Register in `Registry.all`.
3. For higher-order externs, use `ExternalCallContext.Apply` from `Impl` to invoke script functions.
4. Add tests in:
   - `tests/FScript.Runtime.Tests` for direct module behavior.
   - `tests/FScript.Language.Tests/HostExternTests.fs` for interpreter integration.

## Shared extern assembly loading
- The `fscript` CLI can inject externs from user assemblies via:
  - `--extern-assembly <path>` (repeatable)
  - `--no-default-externs` (disables `Registry.all`)
- Provider discovery:
  - The shared Runtime loader loads each unique assembly path once.
  - It discovers methods annotated with `[<FScriptExternProvider>]`.
  - It invokes each discovered provider once per environment initialization. CLI initializes before execution; VS Code initializes once per folder-server process.
- Conflicts:
  - Any duplicate external function name across defaults + user providers is a fatal CLI error.
- Runtime constraints:
  - If runtime assembly loading is unavailable (for example native AOT), `--extern-assembly` is rejected with a clear CLI error.

## Compatibility guidance
- External names are part of source-level API; renaming is breaking.

## Host-managed type registration

Providers may additionally return `ExtensionRegistration` (`Types`, `Unions`, and `Functions`), with the same zero-argument or `HostContext` invocation conventions. Host type metadata and opaque values use the existing extern execution bridge. SDK function-list providers are also supported. See [host-managed types](./host-managed-types.md) for creation restrictions, structural readers, payload transport, and boundary validation.

## Shared environment discovery

The CLI `--config` flag and VS Code environment selection reuse the runtime provider loader and its authoritative `ExtensionRegistration`. Both SDK function-list provider signatures are supported. See [environment configuration](./environment-configuration.md) for path resolution, merging, discovery, trust and reload behavior.

## Independent extension contract

Assembly providers use `FScript.Extensibility` attributes and return SDK `ExternalFunction list` or `ExtensionRegistration`, with no arguments or one sealed SDK `HostContext`. Existing provider assemblies must rebuild. SDK helpers and values do not depend on interpreter assemblies. Runtime adapts registrations using `ExtensionBridge.toLanguageRegistration`; native embedding APIs retain their Language representations. Authoring helpers live in the SDK, not Runtime.

Provider trust, atomic loading, configuration selection, and reload rules are unchanged. SDK callback/task handles are valid only in their originating active evaluation; handler contexts expire on return. Runtime enforces limits and rejects foreign/expired handles. An SDK `ExtensionException` carries a message and optional inner cause; callback errors preserve script locations.

## NuGet-distributed extensions

`[[runtime.packages]]` declares a required package `name` and optional exact `version`; an omitted version selects latest stable. The shared Runtime loader restores dependencies without the .NET SDK and discovers the existing SDK `[FScriptExternProvider]` methods in explicitly requested package runtime assemblies. Providers return the same SDK registrations used by local DLLs. `EnvironmentLoader.loadWithInfo` additionally exposes resolved package versions and concrete assembly paths; `load` retains its registration-only result. Parsing settings does not restore or execute packages. See [environment configuration](environment-configuration.md) for cache, dependency selection, trust, and failure rules.
