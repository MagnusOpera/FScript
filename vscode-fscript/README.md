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

One discovered manifest is selected automatically. With multiple candidates, use **FScript: Select Configuration**; **No configuration** is always available. Each workspace folder has an independent persisted choice. A nonempty folder setting `fscript.config` overrides that choice; clear the setting to use the picker. The configuration status item shows the effective selection.

Configured paths resolve from the manifest directory. With no explicit root, selected editor environments use the workspace folder, while the CLI keeps its normal script/current-directory default. Run the same environment with `fscript --config ./fscript.toml script.fss`. Editor REPL commands do not automatically adopt it.

Configured DLLs execute registration code and require workspace trust. Changes to saved manifests or provider DLLs restart only the affected server and reanalyze open buffers. On Windows, stop the server with `fscript.lsp.enabled` before rebuilding a locked DLL, then re-enable it. Invalid environments report manifest diagnostics and revert to default analysis without stale configured signatures. See the [full specification](../docs/specs/environment-configuration.md).

## Run a script with its selected environment

**FScript: Run Current Script** saves the active file and starts a fresh CLI process using that document's workspace-folder configuration selection (including explicit none, setting overrides, and missing selected paths). It requires workspace trust and a saved file. The command always uses the installed `fscript` executable from PATH, including inside the FScript repository. Install CLI 0.81.5 or later and make it available to VS Code before running a script. Arguments are passed directly without shell interpolation. Imports resolve through normal CLI file loading; CLI root defaults apply. With no selected editor environment, the command passes `--no-config` so CLI discovery cannot override the editor’s default or explicit-none choice. REPL execution commands remain separate and do not forward configuration. Automatic REPL launch passes `--no-config` to preserve its default environment; custom REPL commands execute exactly as configured.
