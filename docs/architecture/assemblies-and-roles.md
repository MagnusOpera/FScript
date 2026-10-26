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

### `FScript.Backend.CSharp` and `FScript.Backend.DotNet`
Role:
- Consume validated LIR to emit an independent C# project or optimized .NET assembly.
- Share concrete HM specialization and simplification in `FScript.Compiler`.
- Bind FFI calls and external types to the CLR; report target-specific diagnostics.
- Optionally publish self-contained or Native AOT executables through the .NET SDK.

Consumer tests follow the same boundary: `FScript.Interpreter.Lir.Tests` owns LIR execution; `FScript.Backend.DotNet.Tests` owns native generation. `FScript.Compiler.Tests` covers HIR, lowering, LIR validation, and artifacts.

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
- Carry complete type/union/function registrations through compilation, evaluation, exports, and sessions.
- Provide JavaScript type/function descriptor helpers with explicit types; preserve opaque payloads through authenticated weak-key handles and bind script callbacks to active runtimes.
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
- `FScript.Compiler` depends only on `FScript.Language`.
- `FScript.Interpreter.Lir` and `FScript.Backend.CSharp` each depend on `FScript.Compiler`.
- `FScript.Backend.CSharp` depends on `FScript.Compiler`; `FScript.Backend.DotNet` uses that emitter and Roslyn to produce PE assemblies.
- `FScript.JavaScript` depends on `FScript.Language` and Fable packages.
- `FScript.Runtime` depends on `FScript.Language` types.
- `FScript.CSharpInterop` depends on both `FScript.Language` and `FScript.Runtime`.
- `FScript.LanguageServer` depends on `FScript.CSharpInterop`.
- `FScript` depends on `FScript.Language`, `FScript.Runtime`, `FScript.Compiler`, `FScript.Interpreter.Lir`, `FScript.Backend.CSharp`, and `FScript.Backend.DotNet`.

This keeps the language engine reusable while runtime capabilities remain host-configurable. The JavaScript facade is intentionally separate from `.NET` runtime externs: v1 exposes core language hosting, virtual imports, tagged values, and JS-provided externs, while filesystem, console, task, and other runtime modules stay in the .NET runtime layer.

## Host-managed value transport

`FScript.Language` owns host type descriptors, `ExtensionRegistration`, `HostInterop`, and `VHostManaged`. The value contains visible field snapshots, type metadata, and an opaque host payload. Inference resolves registered types separately from constructible records. Internal record-shape variables retain the whole argument type and carry a constructibility requirement for record updates; they participate in substitution, generalization, and instantiation. No new script syntax or language trait is added.

`FScript.Runtime.ScriptHost`, interactive sessions, and the CLI pass a registration through inference and evaluation. The SDK provider attribute accepts SDK function lists or registrations; Runtime adapts these to Language. No process-global host type registry or second invocation bridge is used. Field projection, matching, equality, and display share visible-field behavior; serialization never traverses the opaque payload. See the [host-managed type specification](../specs/host-managed-types.md).

## Shared external-function environments

`FScript.Runtime` owns TOML parsing, effective environment settings and the provider loader extracted from the CLI. Parsing is independent of executable loading. `FScript.Language` continues to depend on neither Runtime nor TOML. CLI and LSP use the same registration metadata and validation.

The VS Code extension owns discovery, persisted selection, trust gating and a base language-client/server process per workspace folder, with lazy additional isolated processes for document-specific automatic environments and a separate default client for standalone/untitled documents. Automatic selection chooses the nearest containing manifest within the owning folder; explicit selections/settings stay folder-wide. Configuration paths and trust enter through initialization options. The server reports effective settings/watch paths through `fscript/environmentInfo`; JavaScript does not parse TOML or duplicate signatures. Process replacement reloads assemblies and discards analysis caches together. Document ownership filters isolate nested workspace roots and distinct automatic manifests within a root; imports use the requesting environment. Folder routing changes serialize process replacement before resynchronizing open buffers. See [environment configuration](../specs/environment-configuration.md).

## CLR projection helpers

`FScript.Extensibility.HostTypes` owns opt-in CLR type/property attributes, reflection over visible properties, and a shared field mapping used to derive descriptors and snapshot values. It creates SDK descriptors and snapshots; Runtime translates these into Language representations through `ExtensionBridge`, while inference and evaluation retain native validation. Typed retrieval checks expected projection metadata and payload CLR type without evaluating getters. Reflection helpers do not add a global registration cache, CLR method discovery, or a dependency from Language to Runtime.

The reflection attribute's `ScriptConstructible` option selects the existing ordinary-record descriptor encoding (`HostManaged = false`) or opaque encoding. Constructible F# records require all constructor fields to be visible at descriptor initialization. SDK projection converts SDK record values back into CLR records with recursive container conversion and checked integer narrowing; Language continues to operate on ordinary records and has no dependency on CLR construction. Opaque mode retains its payload validation and transport rules.


## Export declarations

The lexer keeps `export` as an identifier. At a top-level statement boundary, the parser recognizes `Ident "export"` followed by `Let` and marks the existing `SLet`/`SLetRecGroup` export flag. Nested uses of that declaration prefix are rejected; ordinary identifiers named `export` remain valid. No attribute model or extra AST statement kind is introduced. TextMate and LSP semantic classification highlight only the contextual prefix. Export discovery and invocation continue to consume the existing flags.


## Typed host function adapters

`FScript.Extensibility.HostFunctions` inspects the declared F# function type once to prepare its curried invocation steps, scheme, arity, and argument/result codecs. `HostTypes` owns the shared CLR-to-script type projection and snapshot/restoration logic for both object fields and function boundaries. The adapter validates explicit registrations during construction and validates values before handler execution and after result conversion. It returns the existing `ExternalFunction`; no new invocation mechanism, global registry, Language dependency, assembly discovery protocol, or JavaScript authoring API is introduced.

## FFI graphics sample

`samples/ffi` separates script-owned geometry and scene decisions from the native renderer. Its F# provider library references the independent Extensibility SDK and a small C# GLFW/OpenGL backend; the backend has no Language or Runtime dependency. `teapot.fss` tessellates Bézier control points and supplies vertices/normals/indices; `scene.fss` supplies camera, light, material, and animation. `main.fss` starts execution through callback-based `Graphics.run`/`Graphics.runHeadless` externs; the provider invokes setup and frame closures through `ExternalCallContext.Apply` on the window thread. Static compiler companions instead bind typed CLR calls and unary setup/render delegates, allowing the same script to compile to Native AOT without SDK dispatch. The renderer calls its small set of GLFW/OpenGL entry points through unmanaged function pointers. GLFW runtime binaries and their license are embedded in the renderer assembly and extracted by content hash only on graphical startup; no managed assembly path or dynamic delegate generation is required. Geometry uses array-backed native records while preserving script list semantics. No sample executable project is required. Sample tests use a recording backend and require no display; graphical checks remain explicit. These sample-only dependencies do not enter the CLI, Runtime package, or JavaScript build.

The macOS release entitlement permits external native dependency signatures for explicitly loaded providers. The graphics backend resolves its bundled native assets relative to its provider assembly; the CLI installation directory and system search paths are not modified.

## CLI manifest discovery

The CLI supplies the entry script directory (or cwd for stdin/REPL) to Runtime’s `EnvironmentConfiguration.resolveForCli`. This selects only that directory’s `fscript.toml`, then delegates to the existing explicit parser/merger and atomic provider loader. Discovery never changes roots, searches parents, or loads per-import environments. Embedders and the language server retain explicit `resolve` semantics. VS Code’s run task passes its selected `--config`, or `--no-config` when no environment is selected, so CLI discovery cannot override editor selection.

## Foreign union registration

Language owns `HostUnionDescriptor` and the registration’s `Unions` collection. Registered union definitions populate the existing inference constructor map and evaluation environment, including qualified names. Runtime alone reflects attributed F# unions and reconstructs CLR cases; multiple payload fields map to script tuples. Its shared codec also maps F# sets to lists and reconstructs sets with CLR ordering/deduplication. EnvironmentLoader merges union descriptors atomically with records/functions, and LSP completion/hover consumes the same registration.

## Release assembly identity

The tag workflow publishes the VS Code language server with the release `Version`, matching Language and Runtime NuGet assembly identities. A staging regression check verifies every bundled FScript server assembly before VSIX packaging. A default `0.0.0.0` server assembly cannot satisfy providers referencing the versioned NuGet assembly.

## Independent extension SDK

`FScript.Extensibility.dll` belongs to its own sibling repository and NuGet release lifecycle. Extension assemblies reference `MagnusOpera.FScript.Extensibility` without Language or Runtime. Attributes, CLR projection helpers, SDK descriptors, type schemes, values, and opaque callable/task interfaces live there. Public record and union shapes are frozen within 1.x; its unsigned assembly version stays `1.0.0.0`.

Runtime depends on the SDK and translates registrations through `ExtensionBridge.toLanguageRegistration`. Language retains its native inference types and values and has no SDK dependency, including in Fable builds. Providers receive SDK `HostContext` with adapted default functions. Configured loading still merges registrations atomically and performs native validation before analysis or evaluation.

Callbacks and tasks cross through host-owned handles tied to an evaluation session. Contexts expire on handler return; sessions expire after evaluation/invocation. Conversion retains visible snapshots, payloads, and nominal metadata, while callback errors preserve interpreter source locations. The bridge never exports ASTs, closure environments, or worker state.

For development, `/p:UseLocalFScriptExtensibility=true` selects the sibling SDK project. Ordinary builds and releases use the independently published NuGet package. No build configuration is shared in the umbrella directory.

## NuGet environment resolution

Runtime owns `NuGetEnvironment`, backed by the netstandard-compatible NuGet.Commands 6.14.3 client and an embedded portable RID graph from .NET SDK 10.0.401 (MIT; see Runtime's third-party notice). Environment parsing records package IDs/optional versions without I/O. Loading resolves the full graph against the manifest's NuGet.Config hierarchy, chooses framework/RID runtime assets, stages them atomically in the manifest-local `.fscript` cache, and applies existing attributed SDK provider discovery once per compatible assembly, including marked provider dependencies. No SDK installation, MSBuild subprocess, package targets, or scripts are involved. A per-environment filesystem lock protects concurrent restores. Selected package versions and staged assembly paths are returned by `loadWithInfo`; `load` delegates and returns only the registration.

CLI and LSP share this pipeline. LSP invokes it only after workspace trust, exposes resolved packages through environment-info and the configuration tooltip, and watches staged DLLs using existing restart handling. Failed restoration never exposes a partial registration. One non-collectible `ExtensionLoadContext` owns the complete resolved NuGet environment, with SDK and FSharp.Core types unified to the host. All package providers share one copy of their numerical core dependencies, while host implementation dependencies remain separate. Consumers can reference an owner’s script descriptor in their schemes without registering it twice. Explicit and transitive provider references are deduplicated. Local DLL injection retains `Assembly.LoadFrom`. This follows the [shared-contract plugin model](https://learn.microsoft.com/en-us/dotnet/core/tutorials/creating-app-with-plugin-support). Runtime owns all NuGet dependencies; Language/Fable and the independent Extensibility SDK remain unchanged.

## Debugger execution

Language owns the shared evaluator with eager completion and resumable continuations and .NET-only low-level debug
hooks. Runtime owns `DebugSession`: logical tasks, frames, breakpoint policy, safe
inspection, and watch validation. `FScript.DebugAdapter` translates DAP without
implementing language semantics. The CLI hosts adapter and script-process modes;
VS Code selects the environment and launches the installed CLI. Script terminal
I/O and authenticated debugger IPC are separate.

The main task preserves its process main thread. Background script continuations
release scheduler workers on pause/await; synchronous native calls use task-owned
threads, allowing callback suspension without scheduler starvation. Fable uses
the synchronous continuation driver and contains no debugger API. See the
[debugger specification](../specs/debugger.md).
