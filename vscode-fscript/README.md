# VS Code FScript Extension

This extension provides:

- `.fss` language registration
- TextMate syntax highlighting
- LSP client wiring to `FScript.LanguageServer`
- Configurable inlay hints and server settings

## Development

From `vscode-fscript/`:

```bash
npm install
```

Then run the extension in VS Code (`F5`) from this repository.

The extension depends on a .NET runtime for `FScript.LanguageServer`. It requests runtime acquisition through the VS Code `.NET Install Tool` integration (`ms-dotnettools.vscode-dotnet-runtime`) and falls back to `dotnet` from `PATH` when needed.

## Release

The extension is published on tag pushes only, to both:

- VS Code Marketplace
- Open VSX

Release checklist:

1. Update `vscode-fscript/package.json` version.
2. Update root `CHANGELOG.md` (the extension changelog points to this file).
3. Commit and push.
4. Push tag `vX.Y.Z` matching `package.json` version.

The CI workflow validates that tag version and extension version are identical before publishing.

## Settings

- `fscript.lsp.enabled`: enable/disable LSP features.
- `fscript.inlayHints.enabled`: toggle inlay hints from the language server.
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
