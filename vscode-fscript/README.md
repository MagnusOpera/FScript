# VS Code FScript Extension

Requires VS Code 1.91 or later. Extension development and packaging require Node.js 22 or later.

This extension provides:

- `.fss` language registration
- TextMate syntax highlighting, including `numeric` type annotations
- LSP client wiring to `FScript.LanguageServer`
- Configurable inlay hints and server settings

## Numeric Types

Hover and inlay hints display unresolved arithmetic types as `numeric`:

```fsharp
let add x y = x + y
// add: (x: numeric) -> (y: numeric) -> numeric

let addInt x (y: int) = x + y
// addInt: (x: int) -> (y: int) -> int
```

Each `add` call accepts either two integers or two floats. Mixed numeric types and nonnumeric operands produce diagnostics; no implicit conversion is performed. `numeric` is also supported in explicit annotations and syntax highlighting. Remainder (`%`) accepts integers only.

See the [type-system guide](../website/docs/language/type-system.md) for the type hierarchy.

## Development

From `vscode-fscript/`:

```bash
npm install
```

Then run the extension in VS Code (`F5`) from this repository. The local server builds asynchronously once per activation, so the editor stays responsive while it starts.

Run `npm test` to check extension activation, logging-channel compatibility, startup failures, and shutdown. These JavaScript regression tests live in `tests/FScript.LanguageServer.Tests/` and run alongside the .NET language-server tests in CI.

The extension depends on a .NET runtime for `FScript.LanguageServer`. It requests runtime acquisition through the VS Code `.NET Install Tool` integration (`ms-dotnettools.vscode-dotnet-runtime`) and falls back to `dotnet` from `PATH` when needed.

## Release

The extension is published on tag pushes only, to both:

- VS Code Marketplace
- Open VSX

Follow the repository [release process](../AGENTS.md#release-process-tags-and-github-draft): prepare the release with `make release-prepare version=X.Y.Z`, push its commit and tag together, then review and publish the draft produced by CI. Extension packaging and publishing are controlled by the same versioned tag workflow.

## Settings

- `fscript.lsp.enabled`: enable/disable LSP features.
- `fscript.inlayHints.enabled`: toggle inlay hints from the language server.
- `fscript.hoverHints.enabled`: toggle inferred hover hints.
- `fscript.config`: explicit folder-relative environment manifest path; overrides persisted UI selection.
- `fscript.cli.path`: optional executable or `fscript.dll` for Run Current Script and debugging; empty uses `fscript` on PATH.
- `fscript.server.path`: optional custom path to `FScript.LanguageServer.dll`.
- `fscript.server.logLevel`: requested server log level (`error`, `warn`, `info`, `debug`).
- `fscript.repl.command`: shell command used to start REPL in the extension terminal. Default is `auto` (prefer local workspace CLI via `dotnet run`, then fallback to `fscript`).

## Commands

- `FScript: Open REPL`: opens (or reuses) an `FScript REPL` terminal and starts REPL.
- `FScript: Send Selection to REPL`: sends current selection to the REPL terminal, preserving single top-level `let` bindings for later REPL use.
- `FScript: Execute Current Script in REPL`: sends full active `.fss` document to the REPL terminal as one atomic block.
- `FScript: Show Language Server Output`: opens language server logs.
- `FScript: View AST`: renders AST JSON from the active file.
- `FScript: View Inferred AST`: renders inferred AST JSON from the active file.
- `FScript: Select Configuration`: choose automatic discovery, a manifest, or no configuration for a workspace folder.

See the complete [settings and commands reference](../website/docs/reference/editor-reference.md) for defaults and trust behavior.

## Logs and Status

- Run command: `FScript: Show Language Server Output` to open server logs.
- A status-bar item shows server lifecycle:
  - `starting...` spinner during initialization
  - `FScript` with check mark when ready
  - error indicator when startup fails

The extension starts the language server with one of these strategies:

1. Custom path from `fscript.server.path` (if configured)
2. Packaged server: `server/FScript.LanguageServer.dll`
3. Local development fallback: builds `../src/FScript.LanguageServer/FScript.LanguageServer.csproj` and runs `bin/Debug/net10.0/FScript.LanguageServer.dll`

Map annotations use postfix forms such as `string map[int]` and `string map[Priority]`. String-keyed maps appear as `string map`, including when annotated with `map[string]`. Hover and inlay hints preserve inferred key types.

## External-function environments

Place a `fscript.toml` in a workspace folder:

```toml
[runtime]
extern-assemblies = ["./extensions/Geometry.dll"]
```

Automatic selection uses the `fscript.toml` beside the script, or the nearest containing ancestor manifest within its workspace folder. Sibling configurations do not conflict: Numerics examples and a graphics sample can be open together with independent environments. With no containing manifest, defaults apply. **FScript: Select Configuration** can explicitly select one manifest for the whole folder, or **No configuration**. Each workspace folder has an independent persisted choice. Different automatic environments run in separate processes; documents sharing a manifest share their process. A nonempty folder setting `fscript.config` overrides that choice; clear the setting to use the picker. The configuration status item shows the effective selection.

Configured paths resolve from the manifest directory. With no explicit root, selected editor environments use the workspace folder, while the CLI keeps its normal script/current-directory default. Run the same environment with `fscript --config ./fscript.toml script.fss`. Editor REPL commands do not automatically adopt it.

Configured DLLs execute registration code and require workspace trust. Changes to saved manifests or provider DLLs restart only the affected server and reanalyze open buffers. On Windows, stop the server with `fscript.lsp.enabled` before rebuilding a locked DLL, then re-enable it. Invalid environments report manifest diagnostics and revert to default analysis without stale configured signatures. See the [full specification](../docs/specs/environment-configuration.md).

## Run a script with its selected environment

**FScript: Run Current Script** saves the active file and starts a fresh CLI process using that document's effective configuration selection (including explicit none, setting overrides, and missing selected paths). It requires workspace trust and a saved file. The command uses `fscript.cli.path` when configured, otherwise `fscript` from PATH. Use CLI 0.81.5 or later. Arguments are passed directly without shell interpolation. Imports resolve through normal CLI file loading; CLI root defaults apply. With no selected editor environment, the command passes `--no-config` so CLI discovery cannot override the editor’s default or explicit-none choice. REPL execution commands remain separate and do not forward configuration. Automatic REPL launch passes `--no-config` to preserve its default environment; custom REPL commands execute exactly as configured.

### NuGet extension packages

Manifests can also use `[[runtime.packages]]` with required `name` and optional exact `version` (omitted means latest stable). Packages use the existing SDK `[FScriptExternProvider]` registration method; no assembly path or additional attribute is needed. Trusted folder environments restore transitive runtime dependencies through NuGet, without an installed .NET SDK. Restricted Mode performs no restore or provider execution. Packages restore automatically into `.fscript` beside the manifest; this cache is excluded from configuration discovery. Add `.fscript/` to `.gitignore`. Resolved versions appear in the configuration tooltip. Restart the environment to refresh an unpinned package; pin versions for repeatable execution.

```toml
[[runtime.packages]]
name = "FScript.Numerics.Extensions"
version = "0.1.0"
```

## Debugging

Press F5 in a saved FScript file to debug with the CLI selected by
`fscript.cli.path`, or `fscript` from PATH when the setting is empty. The
CLI must support `--debug-capabilities`; upgrade it if launch reports an older
version. Debugging requires workspace trust and uses the selected workspace
configuration, matching editor analysis. Script input/output uses an integrated
terminal.

You can also run **FScript: Debug Current Script** from the Command Palette,
or choose **Debug Current Script** from the FScript editor context menu.

```json
{
  "type": "fscript",
  "request": "launch",
  "name": "Debug FScript",
  "program": "${file}",
  "stopOnEntry": false
}
```

Optional `args` and `cwd` configure script arguments and working directory.
Breakpoints and stepping operate on expressions, including same-line pipeline
stages. Call Stack exposes independent logical tasks; pausing one does not pause
others. Variables are read-only, and watches accept names, visible fields,
parentheses, and literals without executing functions. Native FScript callbacks
can be debugged, but their native caller waits while paused. Fable debugging and
attach are unavailable. Restart to pick up source or provider changes.

### Testing a local CLI build

Build the CLI with `make build`, then set `fscript.cli.path` in user or
workspace-folder settings to the resulting DLL, for example:

```json
{
  "fscript.cli.path": "/absolute/path/to/FScript/src/FScript/bin/Release/net10.0/fscript.dll"
}
```

The extension invokes managed DLLs through `dotnet` on PATH. An executable path
or command name also works. This setting takes one path, without shell quoting
or extra arguments. Relative file paths resolve from the owning workspace folder,
or the script's directory for standalone files. The next run or debug launch
uses the changed setting; rebuild the DLL after source changes. Workspace-defined
CLI settings require trust.

The language server uses the separate `fscript.server.path` setting. REPL commands
continue to use `fscript.repl.command`.
