# F# Type Provider for FScript Exports

This specification defines the contract for `MagnusOpera.FScript.TypeProvider`.

## Purpose

- Compile-time parse and type-check `.fss` scripts.
- Expose `[<export>]` functions as strongly-typed static methods in F#.
- Allow runtime script replacement with strict signature compatibility checks.

## Provider entry point

- Namespace: `FScript.TypeProvider`
- Type provider: `FScriptScriptProvider`

Static parameters:
- `ScriptPath: string` (required)
- `RootDirectory: string` (optional, defaults to script directory)
- `ExternProviders: string` (optional, semicolon-separated assembly-qualified provider type names)

## Compile-time behavior

1. Resolve script path and root directory.
2. Resolve externs using runtime defaults plus configured extern-provider types.
3. Parse with includes from file and run type inference.
4. Collect exported functions.
5. Fail compilation on:
   - parse/type errors,
   - unsupported exported signature shapes.

## Exposed members

For each exported function, generate one static method with mapped .NET/F# types.

Provider-generated static members:
- `SetRuntimeResolver : (unit -> RuntimeScriptOverride option) -> unit`
- `ClearRuntimeResolver : unit -> unit`

`RuntimeScriptOverride` fields:
- `RootDirectory: string`
- `EntryFile: string`
- `EntrySource: string`
- `ResolveImportedSource: (string -> string option) option`

## Supported exported signature mapping (v1)

Supported:
- `unit`
- `int` -> `int64`
- `float`
- `bool`
- `string`
- `list<T>`
- `option<T>`
- tuples (arity `2..8`)
- `map<string, T>` -> `Map<string, T>`

Rejected:
- records
- unions
- named/custom types
- function values in argument/return positions
- unresolved type variables
- non-string map keys

## Runtime compatibility policy

- Provider computes a compile-time fingerprint of all exported function signatures.
- Every invocation loads script via compile-time path or active runtime resolver override.
- Runtime exported signature fingerprint must exactly match compile-time fingerprint.
- Mismatch fails invocation with an error before function execution.

## Runtime load source selection

- No resolver set: load compile-time script file.
- Resolver set and returns `Some`: load `EntrySource` with `loadSourceWithIncludes` and optional import resolver.
- Resolver set and returns `None`: fallback to compile-time script file.
