# Environment configuration

`fscript.toml` describes external-function providers and optional compile defaults. It supports local assemblies and NuGet extension packages, with no script entry point or import declarations.

```toml
[runtime]
root = "."
default-externs = true
extern-assemblies = ["./extensions/Geometry.dll"]

[[runtime.packages]]
name = "FScript.Numerics.Extensions"
version = "0.1.0" # optional; omit for latest stable
```

All properties are optional. The `runtime` and `compile` tables are accepted; the compile properties are specified below. `packages` is an array of tables, each with a required NuGet `name` and optional exact string `version`. Unknown package properties, invalid IDs/versions, and duplicate IDs (case-insensitive) are errors. Version ranges and wildcard strings are not accepted; omit the version for latest stable. Explicit prerelease versions are supported. `root` is a string, `default-externs` is a boolean, and `extern-assemblies` is an array of strings. Invalid TOML, unknown keys/tables, and incorrect property types are errors. Parsing and normalization never restore packages, access feeds, or load executable providers.

## CLI

File execution, `compile`, and `analyze` share the following discovery rules. Without an explicit override, the CLI loads `fscript.toml` beside the entry script. Stdin and REPL modes look only in the current working directory. Discovery never searches parent directories or nested folders, and file execution never falls back to the working directory. Imported files retain the entry script’s environment. If no manifest exists at that location, existing defaults apply. `--config <path>` overrides discovery, with `-c <path>` also accepted for file execution and `analyze`; `--no-config` disables manifest loading. These two options are mutually exclusive. The positional script, stdin and interactive modes, and script arguments after `--` retain their existing meanings.

Settings apply in order: existing defaults, properties present in the manifest, explicit CLI flags. `--root` overrides `runtime.root`; `--no-default-externs` disables defaults even when enabled by configuration. There is no force-enable flag.

Manifest root and assembly paths resolve relative to the manifest directory. Explicit flag paths retain their working-directory resolution. Assemblies from configuration precede explicit `--extern-assembly` arguments; normalized paths are deduplicated in first-occurrence order, using the existing loader's case-insensitive comparison. A manifest directory becomes the sandbox root only when `root = "."` is explicit. Without a root property, CLI defaults remain the script directory (file mode) or current directory (stdin/REPL).

Compilation automatically references its provider assemblies and their managed dependencies. An explicit `--reference` is only needed for additional assemblies outside that dependency closure. Single-file CLI distributions extract their bundled managed assemblies so the compiler can read their metadata; runtime-pack framework assemblies remain runtime dependencies rather than generated application assets. Manifest discovery does not make a provider Native AOT compatible. Providers using SDK callback dispatch require static compiled FFI bindings before `--aot` is supported.

Configuration, missing-file, loading, provider-signature and registration-conflict errors stop execution with a nonzero exit. The same legacy and registration-returning `[<FScriptExternProvider>]` methods work through flags and configuration.

## Compile defaults

The same manifest can set defaults for `fscript compile`:

```toml
[compile]
backend = "dotnet"
aot = true
size-optimized = true
# output = "artifacts"   # optional, relative to this manifest
# runtime = "osx-arm64" # optional, otherwise use the build host for executables
# single-file = false
```

Then run `fscript compile main.fss`. The entry script is still a positional argument; the manifest does not select it. The CLI uses the same beside-script discovery, `--config`, and `--no-config` rules as execution. Compile settings have no effect on interpretation, analysis, or editor evaluation, and reading them never starts a compiler.

| Property | Type | Default |
| --- | --- | --- |
| `compile.backend` | String: `dotnet`, `csharp`, or `ir` | `dotnet` |
| `compile.aot` | Boolean | `false` |
| `compile.size-optimized` | Boolean | `false` |
| `compile.single-file` | Boolean | `false` |
| `compile.runtime` | Nonempty string | Build host RID for executable publishing |
| `compile.output` | Nonempty path string | Conventional directory below the entry script |

Explicit CLI options override manifest values. `--target` overrides `backend`; `--output` and `--runtime` override their corresponding properties. `--aot` / `--no-aot`, `--size-optimized` / `--no-size-optimized`, and `--single-file` / `--no-single-file` override booleans in either direction. Supplying both positive and negative forms of one flag is an error. Explicit options missing their value are errors, even if the manifest defines a fallback.

Selecting C# or IR ignores inherited .NET publishing defaults. Explicit .NET publishing flags on those backends remain errors. `--single-file` replaces inherited AOT and size optimization; `--aot` replaces inherited single-file mode. `--no-aot` also clears inherited size optimization. An explicit `--size-optimized` still requires effective AOT. If both publishing modes are enabled together, AOT takes precedence, matching the existing CLI behavior. Inherited runtime identifiers apply only to executable publishing; explicit `--runtime` without a publishing mode remains an error.

Without `output`, generated files go under the entry script's directory:

| Output | Directory |
| --- | --- |
| .NET assembly | `bin/<script-name>/dotnet/` |
| C# project | `bin/<script-name>/csharp/` |
| IR artifact | `bin/<script-name>/ir/` |
| Native AOT | `bin/<script-name>/dotnet/<rid>/aot/` |
| Managed single-file | `bin/<script-name>/dotnet/<rid>/single-file/` |

`<script-name>` excludes the script extension. Backend, script, runtime, and publishing directories keep conventional builds separate. Configured output paths resolve relative to the manifest directory; CLI output paths resolve relative to the working directory. An explicit output directory always takes precedence over this convention. Existing unrelated files in an output directory are preserved.

For example, `fscript compile main.fss --no-size-optimized` keeps configured AOT but retains normal framework diagnostics and culture support. `fscript compile main.fss --no-aot` emits a managed DLL, and `fscript compile main.fss --target csharp` emits a C# project. `--no-config` ignores both compile and runtime settings, so providers must then be supplied through CLI options when needed.

## NuGet extension packages

The shared Runtime loader restores all configured packages as one dependency graph using the in-process NuGet client (6.14.3, compatible with Runtime's netstandard2.1 target). Execution needs no installed .NET SDK, MSBuild, or `dotnet` subprocess. Package build targets/scripts are not executed. Standard `NuGet.Config` package-source settings are resolved from the manifest directory; packages and runtime assets are cached in `.fscript` beside the selected manifest. Restoration is automatic: there is no install command. `.fscript/packages` stores extracted/downloaded NuGets, `.fscript/restore` contains resolution state, and `.fscript/environments` contains selected runtime assets. Add `.fscript/` to source-control exclusions; deleting it clears the cache (stop loaded editor/host processes first on Windows). Local feeds and explicitly configured source credentials are supported. With default NuGet settings, packages come from nuget.org. Changes to NuGet.Config require reinitializing the environment.

Omitted versions float to the latest stable release and are reevaluated on each environment initialization, including editor-server restart. No background update polling is performed. Exact versions can reuse the package cache offline; floating versions require available source metadata. Pin versions for repeatable execution. A missing/incompatible package, unavailable source, dependency downgrade/range violation, or unusable provider fails initialization atomically.

NuGet selects compatible runtime assets for the running .NET major version and process RID using an embedded portable runtime graph. Selected managed dependencies, satellite resources, and native libraries are staged under the manifest-local `.fscript` cache, with a separate directory for each resolved graph. Native assets retain both their normal library-search placement and RID-native paths. Reference assemblies, build targets, analyzers, and tools are not provider candidates. Cache writes are serialized and files are published atomically.

Each explicitly requested package must contain at least one compatible runtime assembly with a public static `[FScriptExternProvider]` method. This **existing SDK attribute** identifies the registration entry point; no extra package/type attribute or assembly path is required. Each provider returns SDK `ExtensionRegistration` (type/union/function descriptors) or an SDK function list, with zero arguments or an SDK `HostContext`. Dependencies bearing the same provider attribute are registered automatically, once per assembly; ordinary dependencies contribute runtime assets only. This lets a plotting extension depend on the numerical provider that owns `NDArray`, without duplicating its descriptor. Explicitly listing that dependency as well does not register it twice. Multiple requested packages share dependency resolution; local DLL providers load first, then package providers in declaration order. Existing duplicate function/type checks apply across defaults, DLLs, and packages.

All NuGet providers and their dependencies from one manifest use one private, non-collectible `AssemblyLoadContext`, sharing only the host SDK and FSharp.Core contracts. Compatible dependencies are resolved once for the whole environment, so numerical and plotting packages exchange the same CLR `NDArray` type. Host-loaded copies of private package assemblies are not reused. SDK ABI-version mismatches and requirements for a newer FSharp.Core assembly fail clearly. Conflicting package requirements fail during restore rather than loading two incompatible copies of a shared numerical type. A package that consumes another provider’s script type references its descriptor in local validation but returns only its own descriptors/functions. Local DLL injection keeps its existing load context. Change package versions in a fresh host process; CLI runs and editor restarts naturally provide this boundary. SDK-only extensions remain independently versioned from the FScript interpreter. Runtime restores and adapts their registrations; Language and Fable gain no NuGet dependency.

## VS Code

In automatic mode, each script uses the nearest containing manifest within its owning workspace folder. The extension discovers nested `fscript.toml` candidates within that folder, respecting enabled `files.exclude` and `search.exclude` patterns and excluding `.git`, `.out`, `.fscript`, `node_modules`, `bin`, and `obj`. Explicit paths can refer to excluded or external locations. The manifest beside a script wins; otherwise the closest ancestor manifest within the workspace folder wins. Sibling manifests are unrelated. With no containing manifest, use defaults, even if one unrelated candidate exists. Manifests are selected, never merged or inherited. Nested workspace roots own their own documents and candidates.

**FScript: Select Configuration** offers discovered workspace-relative paths, **No configuration**, and **Automatic selection**. With no active editor folder, the command asks which folder to configure. Choices persist in workspace state by folder URI.

Precedence is a nonempty resource-scoped `fscript.config` setting, then a persisted explicit manifest or explicit none, then automatic discovery. Relative setting paths resolve from the workspace folder. Automatic discovery resolves separately for each script by its containing directory, so multiple sibling configurations need no manual selection. Explicit selections apply to all scripts in the owning folder. Explicit missing paths remain selected and report errors. A setting remains authoritative until cleared; the selection command offers to open settings rather than overwrite it.

The configuration status item shows the effective manifest, its selection source, trust block, or initialization failure. The output channel retains detailed server errors.

For a selected environment, omitted `runtime.root` defaults to the workspace folder. This gives provider initialization a stable root before any document is analyzed. Without a selection, historical source-directory runtime defaults remain. Standalone and untitled documents use a separate default server without discovery.

## Language server and reload

The extension maintains a base process per workspace folder and lazily starts an isolated process for each additional automatic environment needed by open scripts. Documents sharing the same manifest in a folder share that process; different manifests have separate registrations and analysis caches, including shared imported source. A separate default client serves documents outside folders. Initialization options are `workspaceFolderUri`, optional `configurationPath`, and `workspaceTrusted`; configured assemblies and NuGet restore/provider execution require an explicit true trust value. Absent environment options preserve existing LSP behavior. Registered schemes and host-type descriptors feed normal and best-effort inference, completion, hover, signature help, hints and inferred-AST requests. Script code and extern implementations are not evaluated for editor analysis; provider registration itself executes host code.

`fscript/environmentInfo` returns the selected configuration path, effective root/default-extern flag, normalized local/staged assembly watch paths, resolved root-package `name`/`version` pairs in `packages`, `blockedByTrust`, `failed`, and an optional error. It inspects the already initialized environment and does not reload providers.

Saved manifest edits, trust grants, and configured/adjacent dependency DLL changes debounce and restart only the affected environment. Manifest creation/deletion and selection/settings changes recompute ownership and rebuild that folder’s processes before resynchronizing all current buffers, including unsaved documents. A fresh process drops assembly state and URI-keyed document caches; language-client synchronization resends current open buffers. Imports are analyzed in their requesting root document's environment, even if imported files are shared across roots. Registrations and caches never cross server processes.

Local DLL assemblies use the existing .NET `Assembly.LoadFrom` strategy; NuGet environments use one private managed load context with shared SDK/FSharp.Core contracts, without in-process hot reload. On Windows, loaded DLLs may be locked: stop the language server (disable `fscript.lsp.enabled`) before rebuilding a locked provider, then re-enable it. Restart after replacing dependencies when a filesystem event is unavailable. Package runtime assets are staged for dependency resolution; no collectible contexts or hot reload are introduced. Managed isolation does not change OS native-library loading constraints.

Environment loading is atomic. A failure reports diagnostics on the selected manifest, retains safe default editor analysis and an explicit failed status, and never retains a previous configured registration. Correcting the manifest or provider triggers recovery. Unrelated workspace folders continue operating.

## Trust and boundaries

Assembly providers execute arbitrary .NET code and may invoke native code. Configuration and filesystem roots are not security boundaries for provider code. Running a script trusts providers from its selected or automatically discovered manifest as well as explicit assembly flags. Use `--no-config` to prevent manifest loading.

In VS Code Restricted Mode, discovery and TOML parsing are available but packages are never restored and providers never load. Default editor analysis remains available. Trust grants restart environments. Workspace-defined server and REPL executable settings are restricted, and REPL execution commands require trust. Selection is retained while blocked. VS Code REPL commands do not automatically forward the editor's selected manifest in this phase.

## Editor startup

Development and extension-test hosts build the local server asynchronously, once per extension activation; multiple folder clients share that build. Packaged extensions use their bundled server. Resource-scoped server settings apply to every environment client owned by that folder. Run Current Script and debugger launches resolve the same document-specific selection; CLI discovery itself remains beside-only. Default editor completion and hover expose the visible fields of the built-in `Env` value from its built-in type definition.

## Run a script with its selected environment

**FScript: Run Current Script** saves the active file and starts a fresh CLI process using that document's effective configuration selection (including explicit none, setting overrides, and missing selected paths). It requires workspace trust and a saved file. The command always uses the installed `fscript` executable from PATH, including inside the FScript repository. Install CLI 0.81.5 or later and make it available to VS Code before running a script. Arguments are passed directly without shell interpolation. Imports resolve through normal CLI file loading; CLI root defaults apply. When no editor configuration is selected, the run command passes `--no-config`, preserving explicit none and default selections. REPL execution commands remain separate and do not forward configuration. Automatic REPL launch passes `--no-config` to preserve its default environment; custom REPL commands execute exactly as configured.

Provider registrations merge record and union descriptors together. Duplicate type names across either collection, built-ins, or script declarations are initialization errors. Union constructors and signatures are available in every configured CLI/editor environment. The language server offers union-type and short/qualified case completion plus descriptor-backed type/case hover, using the loaded registration.

Packaged editor servers must carry the release assembly version used by the matching Language and Runtime NuGet packages. Provider DLLs referencing those packages must load without identity mismatches; missing-constructor diagnostics after an environment failure are secondary to the manifest diagnostic.

## Independent extension contract

Assembly providers use `FScript.Extensibility` attributes and return SDK `ExternalFunction list` or `ExtensionRegistration`, with no arguments or one sealed SDK `HostContext`. Existing provider assemblies must rebuild. SDK helpers and values do not depend on interpreter assemblies. Runtime adapts registrations using `ExtensionBridge.toLanguageRegistration`; native embedding APIs retain their Language representations. Authoring helpers live in the SDK, not Runtime.

Provider trust, atomic loading, configuration selection, and reload rules are unchanged. SDK callback/task handles are valid only in their originating active evaluation; handler contexts expire on return. Runtime enforces limits and rejects foreign/expired handles. An SDK `ExtensionException` carries a message and optional inner cause; callback errors preserve script locations.
