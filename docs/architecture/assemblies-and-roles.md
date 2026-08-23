# FScript Assemblies, Roles, and Usage

## Purpose
This document describes how FScript is split into assemblies and how they are used together in applications.

## Assembly overview

### `FScript`
Role:
- Command-line tool (`fscript`) to execute `.fss` files.

Responsibilities:
- Parse CLI arguments.
- Resolve execution mode (file, stdin, `version`, REPL), sandbox root, and script arguments after `--`.
- Build runtime context and extern registry.
- Inject CLI environment value (`let Env`) into script execution (with stdlib `Environment` type).
- Execute parse -> type inference -> evaluation pipeline.
- Print final result and report parse/type/eval errors.

Use this when:
- You want to run scripts directly from terminal/CI.
- You want a ready-to-use executable without writing host integration code.

### `FScript.Language`
Role:
- Embeddable language engine.

Responsibilities:
- Core language model and AST.
- Lexer and parser.
- Type system and Hindley-Milner inference.
- Optional per-inference local-variable type capture for editor features; ordinary script inference does not allocate this telemetry.
- Evaluator and value model.
- Incremental inference and evaluation state for REPL-like hosts.
- Pretty-printing of evaluation results.

Use this when:
- You want to embed the language in another .NET application.
- You need language processing independently from default runtime extern catalog.

### `FScript.Compiler`
Role:
- Reusable .NET compiler pipeline for typed HIR, executable LIR, and IR artifacts.

Responsibilities:
- Consume the resolved typed result produced by `FScript.Language`.
- Validate and lower HIR to backend-independent LIR.
- Serialize and validate whole-program `.fsir` artifacts.

`FScript.Compiler` depends on `FScript.Language` and is intentionally excluded from the Fable JavaScript package.

### `FScript.Interpreter.Lir`
Role:
- Backend-independent execution engine for validated LIR.

Responsibilities:
- Execute LIR values, closures, decision trees, aggregates, intrinsics, and failures.
- Validate caller-provided LIR and return one structured execution diagnostic on failure.
- Supply script identity and arguments through `InterpreterContext`.
- Reject reachable unsupported target or host linkage with interpreter diagnostics.

`FScript.Interpreter.Lir` depends on `FScript.Compiler`; the core compiler has no dependency on the interpreter.

### `FScript.Backend.Go`
Role:
- Go 1.25 source backend for validated LIR.

Responsibilities:
- Select Go representations for LIR values.
- Generate deterministic Go project files and the embedded FScript Go runtime.
- Report target-specific `FSB4xxx` diagnostics without adding Go concepts to HIR or LIR.

`FScript.Backend.Go` depends on `FScript.Compiler`; the core compiler has no dependency on the Go backend or a Go installation.

Consumer tests follow the same dependency boundary: `FScript.Interpreter.Lir.Tests` owns executable LIR conformance and `FScript.Backend.Go.Tests` owns generated-project integration. `FScript.Compiler.Tests` references neither consumer and covers HIR, lowering, LIR validation, and artifacts only.

NuGet:
- `MagnusOpera.FScript.Language`

Fable:
- The project is Fable-compatible for source package consumers.
- Fable hosts should use the JavaScript facade rather than consuming raw F# discriminated unions directly.

### `FScript.JavaScript`
Role:
- Fable-compiled JavaScript facade for Node and browser-compatible ESM hosts.

Responsibilities:
- Compile `FScript.Language` to JavaScript through Fable.
- Expose stable JS entry points such as `run`, `parse`, `infer`, `evaluate`, `load`, and `invoke`.
- Provide a small stateful session API for browser sandbox and REPL-like JavaScript hosts.
- Convert between FScript runtime values and tagged JavaScript objects.
- Accept JavaScript-provided externs with explicit type schemes.
- Resolve imports through host-provided virtual source maps or resolver callbacks.

Use this when:
- You want to run or embed FScript from JavaScript.
- You need repeated invocation of exported script functions from a JS host.
- You want to provide JS externs without loading the .NET runtime extern catalog.

npm:
- `@magnusopera/fscript`

### `FScript.Runtime`
Role:
- Runtime integration layer and extern catalog.

Responsibilities:
- Host context model (`RootDirectory` and related host constraints).
- Built-in external function modules (filesystem, JSON/XML, regex, hash, GUID).
- Extern registry composition.
- Host-oriented decoding helpers and sandbox-aware path checks.

Use this when:
- You want the built-in extern ecosystem ready to use.
- You want a baseline runtime integration layer to extend with custom externs.

NuGet:
- `MagnusOpera.FScript.Runtime`

### `FScript.CSharpInterop`
Role:
- C#-friendly integration facade over language + runtime services.

Responsibilities:
- Resolve runtime extern catalog from source path/root context.
- Parse with include/import expansion through a stable interop entry point.
- Run inference APIs through a single host-facing surface.
- Expose stdlib virtual source loading for editor integrations.

Use this when:
- You integrate FScript from C# and want to avoid direct F# compiler/runtime internals.
- You build tooling services (for example LSP hosts) with a stable boundary.

### `FScript.LanguageServer`
Role:
- C# host executable for the Language Server process.

Responsibilities:
- Provide the production C# process host for LSP startup/dispatch.
- Execute the full LSP method surface used by the VS Code extension.
- Keep protocol behavior aligned with existing language/runtime analysis services.

Use this when:
- You want C# ownership of the server host process while reusing existing language services.

### `FScript.TypeProvider`
Role:
- F# compile-time type provider for exported FScript functions.

Responsibilities:
- Parse and type-check `.fss` scripts during F# compilation.
- Project `[<export>]` functions as strongly-typed static members.
- Resolve compile-time/runtime extern providers.
- Enforce runtime signature compatibility using compile-time fingerprints.

Use this when:
- You want F# compile-time validation of script contracts.
- You want strongly-typed invocation of exported script functions without hand-written wrappers.

## Typical composition

### CLI execution path
1. `FScript` reads CLI args, splits script arguments after `--`, and picks mode: file path, piped stdin, `version`, or interactive REPL.
2. For execution modes, `FScript` builds `HostContext` root and resolves externs:
   - `HostContext` includes `RootDirectory` and root-relative `DeniedPathGlobs`
   - default runtime externs from `Registry.all` (unless `--no-default-externs`)
   - optional user extern assemblies via `--extern-assembly`
   - duplicate extern names are rejected as fatal CLI errors
3. `FScript` injects `Environment`/`Env` metadata:
   - file mode: `ScriptName = Some <file name>`, `Arguments = [..]`
   - stdin mode: `ScriptName = None`, `Arguments = [..]`
   - REPL mode: `ScriptName = None`, `Arguments = []`
4. `FScript` invokes `FScript.Language` parse/infer/eval pipeline.
5. `FScript` prints result (or version in `version` mode).

### Embedded host path
1. Host app references `FScript.Language` and (optionally) `FScript.Runtime`.
2. Host builds `HostContext` and extern list.
3. Host runs parse/infer/eval programmatically.
4. Host decides output, logging, and error handling behavior.

### JavaScript host path
1. Host app imports `@magnusopera/fscript` as an ESM package.
2. Host provides optional JS externs through `extern({ name, arity, scheme, invoke })`.
3. Host executes `run` for one-shot evaluation or `load` plus `invoke` for repeated calls.
4. Host receives tagged JS values and structured JS errors.
5. Host resolves imports from virtual paths using `sources` or `resolveImport`; no file-backed resolver is available in Fable builds.

## Dependency direction
- `FScript.Language` has no dependency on `FScript.Runtime`.
- `FScript.Compiler` depends only on `FScript.Language`.
- `FScript.Interpreter.Lir` and `FScript.Backend.Go` each depend on `FScript.Compiler`.
- `FScript.JavaScript` depends on `FScript.Language` and Fable packages.
- `FScript.Runtime` depends on `FScript.Language` types.
- `FScript.CSharpInterop` depends on both `FScript.Language` and `FScript.Runtime`.
- `FScript.LanguageServer` depends on `FScript.CSharpInterop`.
- `FScript.TypeProvider` depends on `FScript.Language` and `FScript.Runtime`.
- `FScript` depends on `FScript.Language`, `FScript.Runtime`, `FScript.Compiler`, `FScript.Interpreter.Lir`, and `FScript.Backend.Go`.

This keeps the language engine reusable while runtime capabilities remain host-configurable. The JavaScript facade is intentionally separate from `.NET` runtime externs: v1 exposes core language hosting, virtual imports, tagged values, and JS-provided externs, while filesystem, console, task, and other runtime modules stay in the .NET runtime layer.
