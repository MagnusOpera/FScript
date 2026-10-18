# Environment configuration

`fscript.toml` describes external-function providers. It has no script entry point, package references, imports, or build settings.

```toml
[runtime]
root = "."
default-externs = true
extern-assemblies = ["./extensions/Geometry.dll"]
```

All properties are optional. Only the `runtime` table and the three properties above are accepted. `root` is a string, `default-externs` is a boolean, and `extern-assemblies` is an array of strings. Invalid TOML, unknown keys/tables, and incorrect property types are errors. Parsing and normalization never load executable providers.

## CLI

Pass `--config <path>` or `-c <path>` explicitly. The CLI never searches for configuration automatically. The positional script, stdin and interactive modes, and script arguments after `--` retain their existing meanings.

Settings apply in order: existing defaults, properties present in the manifest, explicit CLI flags. `--root` overrides `runtime.root`; `--no-default-externs` disables defaults even when enabled by configuration. There is no force-enable flag.

Manifest root and assembly paths resolve relative to the manifest directory. Explicit flag paths retain their working-directory resolution. Assemblies from configuration precede explicit `--extern-assembly` arguments; normalized paths are deduplicated in first-occurrence order, using the existing loader's case-insensitive comparison. A manifest directory becomes the sandbox root only when `root = "."` is explicit. Without a root property, CLI defaults remain the script directory (file mode) or current directory (stdin/REPL).

Configuration, missing-file, loading, provider-signature and registration-conflict errors stop execution with a nonzero exit. The same legacy and registration-returning `[<FScriptExternProvider>]` methods work through flags and configuration.

## VS Code

Each workspace folder has one active environment. The extension discovers nested `fscript.toml` candidates within that folder, respecting enabled `files.exclude` and `search.exclude` patterns and excluding `.git`, `node_modules`, `bin`, and `obj`. Explicit paths can refer to excluded or external locations. Discovery does not create inheritance or select the nearest manifest to a source file. Nested workspace roots own their own documents and candidates.

**FScript: Select Configuration** offers discovered workspace-relative paths, **No configuration**, and **Automatic selection**. With no active editor folder, the command asks which folder to configure. Choices persist in workspace state by folder URI.

Precedence is a nonempty resource-scoped `fscript.config` setting, then a persisted explicit manifest or explicit none, then automatic discovery. Relative setting paths resolve from the workspace folder. Automatic discovery selects exactly one candidate; zero candidates uses defaults, and multiple candidates require selection. Explicit missing paths remain selected and report errors. A setting remains authoritative until cleared; the selection command offers to open settings rather than overwrite it.

The configuration status item shows the effective manifest, its selection source, selection-needed state, trust block, or initialization failure. The output channel retains detailed server errors.

For a selected environment, omitted `runtime.root` defaults to the workspace folder. This gives provider initialization a stable root before any document is analyzed. Without a selection, historical source-directory runtime defaults remain. Standalone and untitled documents use a separate default server without discovery.

## Language server and reload

The extension starts one process per workspace folder, plus a default client for documents outside folders. Initialization options are `workspaceFolderUri`, optional `configurationPath`, and `workspaceTrusted`; configured assemblies require an explicit true trust value. Absent environment options preserve existing LSP behavior. Registered schemes and host-type descriptors feed normal and best-effort inference, completion, hover, signature help, hints and inferred-AST requests. Script code and extern implementations are not evaluated for editor analysis; provider registration itself executes host code.

`fscript/environmentInfo` returns the selected configuration path, effective root/default-extern flag, normalized assembly watch paths, `blockedByTrust`, `failed`, and an optional error. It inspects the already initialized environment and does not reload providers.

Saved manifest edits, manifest discovery changes, selection/settings changes, trust grants, and configured/adjacent dependency DLL changes debounce and restart affected environments. A fresh process drops assembly state and URI-keyed document caches; language-client synchronization resends current open buffers. Imports are analyzed in their requesting root document's environment, even if imported files are shared across roots. Registrations and caches never cross server processes.

Assemblies use the existing .NET `Assembly.LoadFrom` strategy, not in-process hot reload. On Windows, loaded DLLs may be locked: stop the language server (disable `fscript.lsp.enabled`) before rebuilding a locked provider, then re-enable it. Restart after replacing dependencies when a filesystem event is unavailable. No binary shadow copying or collectible load contexts are introduced.

Environment loading is atomic. A failure reports diagnostics on the selected manifest, retains safe default editor analysis and an explicit failed status, and never retains a previous configured registration. Correcting the manifest or provider triggers recovery. Unrelated workspace folders continue operating.

## Trust and boundaries

Assembly providers execute arbitrary .NET code and may invoke native code. Configuration and filesystem roots are not security boundaries for provider code. The CLI trusts explicitly supplied assemblies/configuration.

In VS Code Restricted Mode, discovery and TOML parsing are available but providers never load. Default editor analysis remains available. Trust grants restart environments. Workspace-defined server and REPL executable settings are restricted, and REPL execution commands require trust. Selection is retained while blocked. VS Code REPL commands do not automatically forward the editor's selected manifest in this phase.

## Editor startup

Development and extension-test hosts build the local server asynchronously, once per extension activation; multiple folder clients share that build. Packaged extensions use their bundled server. Resource-scoped server settings apply independently to folder clients. Default editor completion and hover expose the visible fields of the built-in `Env` value from its built-in type definition.

## Run a script with its selected environment

**FScript: Run Current Script** saves the active file and starts a fresh CLI process using that document's workspace-folder configuration selection (including explicit none, setting overrides, and missing selected paths). It requires workspace trust and a saved file. The command always uses the installed `fscript` executable from PATH, including inside the FScript repository. Install the CLI and make it available to VS Code before running a script. Arguments are passed directly without shell interpolation. Imports resolve through normal CLI file loading; CLI root defaults apply. REPL execution commands remain separate and do not forward configuration.
