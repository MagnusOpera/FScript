---
id: editor-reference
title: VS Code Settings and Commands
slug: /reference/editor
---

For installation and a first session, see [Editor Setup](../learn/editor-setup). For configuration discovery, precedence, trust, and reload rules, see [Environment Configuration](./environment-configuration).

## Settings

Defaults below are shipped by the extension. Resource settings resolve in the owning workspace folder; a nested workspace folder owns its documents independently.

| Setting | Default | Meaning |
| --- | --- | --- |
| `fscript.lsp.enabled` | `true` | Enable the FScript language server features (completion, navigation, diagnostics, inlay hints). |
| `fscript.inlayHints.enabled` | `true` | Enable inlay hints provided by the FScript language server. |
| `fscript.hoverHints.enabled` | `true` | Enable hover hints provided by the FScript language server. |
| `fscript.server.path` | `""` | Optional path to a custom FScript.LanguageServer.dll. Relative paths are resolved from the workspace folder, or extension directory for standalone files. |
| `fscript.server.logLevel` | `"info"` | Requested language server log level sent through initialization options. |
| `fscript.repl.command` | `"auto"` | Shell command used to launch the FScript REPL terminal. Use 'auto' to prefer local workspace CLI via dotnet run, then fallback to 'fscript'. |
| `fscript.config` | `""` | Environment configuration path, relative to this workspace folder. Overrides the saved configuration selection. |

`fscript.server.logLevel` accepts `error`, `warn`, `info`, or `debug`. `fscript.config` is a manifest path, not an executable. A nonempty setting overrides persisted UI selection; clear it to use the selection command again.

Workspace trust is required for REPL execution and configured providers. Workspace-defined executable server/REPL settings are restricted in untrusted workspaces. Default static analysis remains available. The REPL commands do not forward the selected manifest; execute the CLI with `--config` to use that environment.

## Commands

All commands appear in the Command Palette under **FScript**.

| Command | Identifier | Purpose |
| --- | --- | --- |
| Show Language Server Output | `fscript.showLanguageServerOutput` | Open server logs and startup failures. |
| View AST | `fscript.viewAst` | Display the parsed syntax tree for the active document. |
| View Inferred AST | `fscript.viewInferredAst` | Display the inferred syntax tree using the owning document environment. |
| Open REPL | `fscript.openRepl` | Start an FScript terminal session. |
| Send Selection to REPL | `fscript.sendSelectionToRepl` | Send the selected source to the REPL. |
| Execute Current Script in REPL | `fscript.executeCurrentScriptInRepl` | Send the current script to the REPL. |
| Select Configuration | `fscript.selectConfiguration` | Select automatic discovery, a manifest, or no configuration for a workspace folder. |

The configuration status item shows the effective selection and its source or failure. Server health/output access is separate. A saved manifest or provider DLL change restarts only the affected folder server and resynchronizes open buffers. A custom server path must point to a compatible language-server assembly.

## Run a script with its selected environment

**FScript: Run Current Script** saves the active file and starts a fresh CLI process using that document's workspace-folder configuration selection (including explicit none, setting overrides, and missing selected paths). It requires workspace trust and a saved file. The command always uses the installed `fscript` executable from PATH, including inside the FScript repository. Install the CLI and make it available to VS Code before running a script. Arguments are passed directly without shell interpolation. Imports resolve through normal CLI file loading; CLI root defaults apply. REPL execution commands remain separate and do not forward configuration.
