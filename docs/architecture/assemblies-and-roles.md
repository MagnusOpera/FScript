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
- Type system and Hindley-Milner inference, including identity-bearing `TNumeric` variables restricted to `int` or `float`; substitution, generalization, and instantiation preserve their constraints, while renderers display unresolved variables as `numeric`.
- Optional per-inference local-variable type capture for editor features; ordinary script inference does not allocate this telemetry.
- Evaluator and value model.
- Incremental inference and evaluation state for REPL-like hosts.
- Pretty-printing of evaluation results.

Use this when:
- You want to embed the language in another .NET application.
- You need language processing independently from default runtime extern catalog.

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
- `FScript.JavaScript` depends on `FScript.Language` and Fable packages.
- `FScript.Runtime` depends on `FScript.Language` types.
- `FScript.CSharpInterop` depends on both `FScript.Language` and `FScript.Runtime`.
- `FScript.LanguageServer` depends on `FScript.CSharpInterop`.
- `FScript` depends on both `FScript.Language` and `FScript.Runtime`.

This keeps the language engine reusable while runtime capabilities remain host-configurable. The JavaScript facade is intentionally separate from `.NET` runtime externs: v1 exposes core language hosting, virtual imports, tagged values, and JS-provided externs, while filesystem, console, task, and other runtime modules stay in the .NET runtime layer.

## Host-managed value transport

`FScript.Language` owns host type descriptors, `ExtensionRegistration`, `HostInterop`, and `VHostManaged`. The value contains visible field snapshots, type metadata, and an opaque host payload. Inference resolves registered types separately from constructible records. Internal record-shape variables retain the whole argument type and carry a constructibility requirement for record updates; they participate in substitution, generalization, and instantiation. No new script syntax or language trait is added.

`FScript.Runtime.ScriptHost`, interactive sessions, and the CLI pass a registration through inference and evaluation. The existing provider attribute accepts either legacy extern lists or registrations. No process-global host type registry or second invocation bridge is used. Field projection, matching, equality, and display share visible-field behavior; serialization never traverses the opaque payload. See the [host-managed type specification](../specs/host-managed-types.md).

## Shared external-function environments

`FScript.Runtime` owns TOML parsing, effective environment settings and the provider loader extracted from the CLI. Parsing is independent of executable loading. `FScript.Language` continues to depend on neither Runtime nor TOML. CLI and LSP use the same registration metadata and validation.

The VS Code extension owns discovery, persisted selection, trust gating and one language-client/server process per workspace folder, with a separate default client for standalone/untitled documents. Configuration paths and trust enter through initialization options. The server reports effective settings/watch paths through `fscript/environmentInfo`; JavaScript does not parse TOML or duplicate signatures. Process replacement reloads assemblies and discards analysis caches together. Document ownership filters isolate nested workspace roots; imports use the requesting environment. See [environment configuration](../specs/environment-configuration.md).

## CLR projection helpers

`FScript.Runtime.HostTypes` owns opt-in CLR type/property attributes, reflection over visible properties, and a shared field mapping used to derive descriptors and snapshot values. It creates the existing Language descriptors and delegates value creation/validation to `HostInterop`; registrations, inference and external invocation use the existing bridge. Typed retrieval checks expected projection metadata and payload CLR type without evaluating getters. Reflection helpers do not add a global registration cache, CLR method discovery, or a dependency from Language to Runtime.

The reflection attribute's `ScriptConstructible` option selects the existing ordinary-record descriptor encoding (`HostManaged = false`) or opaque encoding. Constructible F# records require all constructor fields to be visible at descriptor initialization. Runtime projection converts ordinary script values back into CLR records with recursive container conversion and checked integer narrowing; Language continues to operate on ordinary records and has no dependency on CLR construction. Opaque mode retains its payload validation and transport rules.


## Export declarations

The lexer keeps `export` as an identifier. At a top-level statement boundary, the parser recognizes `Ident "export"` followed by `Let` and marks the existing `SLet`/`SLetRecGroup` export flag. Nested uses of that declaration prefix are rejected; ordinary identifiers named `export` remain valid. No attribute model or extra AST statement kind is introduced. TextMate and LSP semantic classification highlight only the contextual prefix. Export discovery and invocation continue to consume the existing flags.


## Typed host function adapters

`FScript.Runtime.HostFunctions` inspects the declared F# function type once to prepare its curried invocation steps, scheme, arity, and argument/result codecs. `HostTypes` owns the shared CLR-to-script type projection and snapshot/restoration logic for both object fields and function boundaries. The adapter validates explicit registrations during construction and validates values before handler execution and after result conversion. It returns the existing `ExternalFunction`; no new invocation mechanism, global registry, Language dependency, assembly discovery protocol, or JavaScript authoring API is introduced.
