---
id: environment-configuration
title: Environment Configuration
slug: /reference/environment-configuration
---

An optional `fscript.toml` describes the external-function environment available to scripts. The CLI and VS Code use the same assembly-provider registration metadata for execution and type checking.

```toml
[runtime]
root = "."
default-externs = true
extern-assemblies = ["./extensions/Geometry.dll"]
```

All three properties are optional:

| Property | Type | Default |
| --- | --- | --- |
| `runtime.root` | String | Existing execution/editor default, described below |
| `runtime.default-externs` | Boolean | `true` |
| `runtime.extern-assemblies` | Array of strings | Empty |

Unknown sections/properties, incorrect value types, and malformed TOML are errors. There is no entry point, package manager, dependency restore, build configuration, or import declaration in this file.

## CLI usage and precedence

Configuration loading is explicit:

```bash
fscript --config ./fscript.toml scripts/build.fss
fscript -c ./tools/fscript.toml scripts/test.fss -- alice
fscript --config ./fscript.toml
```

The final command starts the REPL when stdin is a terminal; piped stdin continues to run as a script. Without `--config`, the CLI does not search for manifests and retains its existing behavior.

Settings apply in order: existing runtime defaults, properties present in the manifest, then explicit CLI flags. `--root` overrides `runtime.root`. `--no-default-externs` disables defaults regardless of the manifest; there is no force-enable flag. Assembly paths from the manifest precede repeated `--extern-assembly` arguments, with normalized duplicates removed while preserving first occurrence order.

Paths inside the manifest resolve relative to its containing directory, independently of the working directory. Explicit CLI flags and the positional script retain their existing path resolution. If `root` is absent, the CLI uses the script directory in file mode or the current directory in stdin/REPL mode. Setting `root = "."` explicitly selects the manifest directory instead.

Missing manifests/assemblies, invalid providers, and conflicting registrations cause a nonzero CLI exit before execution. See [Register Extern Functions](../embedding/register-externs#assembly-providers-for-cli-and-editor) for provider signatures.

## VS Code selection

The extension discovers nested `fscript.toml` files inside each workspace folder. It respects ordinary workspace exclusions and skips common dependency/build directories. Discovery does not merge manifests or select a different environment for each source file.

- With zero candidates, use the default language-server environment.
- With exactly one candidate, select it automatically unless an explicit choice overrides discovery.
- With multiple candidates, use **FScript: Select Configuration**; until selected, use defaults and show that selection is needed.
- **No configuration** explicitly keeps defaults even when candidates exist. **Automatic selection** restores discovery behavior.

Choices persist independently for each workspace folder. A nonempty folder-resolved setting takes precedence over the saved choice:

```json
{
  "fscript.config": "./tools/fscript.toml"
}
```

Relative setting paths resolve from the workspace folder. Clear the setting to use the picker again. A missing or invalid explicit selection reports an error instead of switching to another discovered file. The status item identifies the effective selection and its source, trust block, or failure.

Each folder has one selected environment for all its `.fss` documents. Nested workspace folders own their own selections. Shared imported source is checked in the requesting document's environment. Standalone files and untitled documents retain the default editor environment without scanning filesystem ancestors.

When a selected manifest omits `root`, the editor uses its workspace folder as the stable provider root. This differs from the CLI's script/current-directory default; specify `root` explicitly when both should use the same root. Without a selected manifest, the editor retains its historical source-directory defaults.

## Reload, errors, and trust

External providers are executable .NET code. Workspace trust is required before configured providers load. Restricted Mode still permits discovery, parsing, and default static analysis. Trusting the workspace reinitializes its environments. Configuration and `root` do not sandbox provider code or native calls.

Saved manifest edits, selection/settings changes, manifest creation/deletion, and configured or adjacent dependency DLL changes restart affected folder servers. Open buffers are resynchronized, so completion, hover, signatures, hints, and diagnostics use the new registration. A failed environment reports manifest diagnostics, uses default analysis, and never retains stale configured externs. Other folders remain independent.

DLLs use the existing .NET load context. On Windows, stop the server by disabling `fscript.lsp.enabled` before rebuilding a locked provider, then re-enable it. Reinitialize manually if filesystem changes were not detected. VS Code REPL commands do not automatically forward the selected configuration; use the explicit CLI command when executing that environment.

## Embedding the same environment

Parsing and effective-setting resolution do not execute assembly code. Hosts can inspect settings before deciding to load providers:

```fsharp
open System.IO
open FScript.Runtime

let require = function
    | Ok value -> value
    | Error message -> failwith message

let workingDirectory = Directory.GetCurrentDirectory()
let settings =
    EnvironmentConfiguration.resolve
        (Some "./fscript.toml") workingDirectory workingDirectory
        None false []
    |> require

// Inspect settings.RootDirectory, DefaultExterns, ExternAssemblies,
// and ConfigurationPath before enabling executable providers.
let registration = EnvironmentLoader.load settings |> require
let loaded = ScriptHost.loadFileWithRegistration registration "scripts/build.fss"
```

The caller supplies its default root and optional overrides. Explicitly registered functions/types remain available through the normal [embedding APIs](../embedding/real-world-embedding); a manifest is optional for embedders too.
