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

[[runtime.packages]]
name = "FScript.Numerics.Extensions"
version = "0.1.0" # optional; omit to use latest stable
```

All runtime properties are optional:

| Property | Type | Default |
| --- | --- | --- |
| `runtime.root` | String | Existing execution/editor default, described below |
| `runtime.default-externs` | Boolean | `true` |
| `runtime.extern-assemblies` | Array of strings | Empty |
| `runtime.packages` | Array of package tables | Empty |

Unknown sections/properties, incorrect value types, and malformed TOML are errors. Each package table requires `name` and optionally accepts an exact string `version`. Unknown properties, invalid IDs/versions, and repeated names are errors. There is no script entry point, build configuration, or import declaration in this file.

## NuGet packages

A package reference needs only its NuGet name. Omit `version` for the latest stable release, or supply an exact version (including a prerelease) for repeatable execution. The latest version is resolved when the CLI starts or the editor environment restarts; there is no background update polling. Version ranges and `"latest"`/wildcard strings are not version values.

FScript restores transitive dependencies using NuGet sources from `NuGet.Config`, resolved from the manifest directory, and caches downloaded packages in `.fscript/packages` beside the manifest. There is no install command: startup restores automatically and reuses downloaded packages. Add `.fscript/` to `.gitignore`; VS Code excludes this cache from manifest discovery. No .NET SDK or build is required when running a packaged extension. Package build scripts and MSBuild targets are not executed. Pinned packages can run from cache offline; latest-version lookup needs source metadata. Reinitialize after changing NuGet.Config.

Providers use the existing SDK `[FScriptExternProvider]` attribute on a public static method returning type/function registrations. No new attribute or DLL filename is needed. Each requested package must contain a compatible attributed provider; dependency packages bearing the provider attribute are also registered once. Ordinary dependencies supply runtime libraries. NuGet selects assets for the running framework and platform, and FScript stages runtime dependencies under the manifest-local `.fscript` cache. All configured packages resolve together; missing packages, version conflicts, incompatible assets, and registration errors stop initialization.

VS Code requires workspace trust **before restoration or loading**. The configuration tooltip shows resolved package versions, and completion, hover, and diagnostics use the package's descriptors. Restart the environment to pick up a newer unpinned package. Use a fresh host process when changing extension versions; CLI execution and editor restarts already do this.

### Sharing numerical values across extensions

All packages in one manifest resolve together and load their dependencies in the same private context. A numerical provider and a plotting provider therefore use one `FScript.Numerics.NDArray` type and can exchange opaque array payloads. Provider dependencies are discovered automatically; the numerical type owner registers its descriptor once, and consumers register only their own functions/types. Explicitly listing the numerical provider as well is safe. Incompatible dependency requirements fail with a restore error instead of silently mixing assembly versions. Embedders that already reference extension CLR libraries directly should register them through their typed APIs when host-side typed access is required.

## CLI usage and precedence

The CLI automatically loads `fscript.toml` beside the entry script, or in the current working directory for stdin/REPL. It checks only that directory: no parent or nested directory search, and no working-directory fallback for file execution. Imported scripts use the entry script’s environment. With no manifest, defaults apply.

You can override or disable discovery:

```bash
fscript scripts/build.fss
fscript --no-config scripts/build.fss
fscript --config ./fscript.toml scripts/build.fss
fscript -c ./tools/fscript.toml scripts/test.fss -- alice
fscript --config ./fscript.toml
```

The final command starts the REPL when stdin is a terminal; piped stdin continues to run as a script. `--config` explicitly selects a manifest; `--no-config` disables discovery. Using both is an error. A discovered malformed manifest or failed provider stops execution rather than falling back to defaults.

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

Local DLLs use the existing .NET load context. NuGet packages from one manifest share an isolated managed load context, with the SDK and FSharp.Core contracts shared with the host. On Windows, stop the server by disabling `fscript.lsp.enabled` before rebuilding a locked provider, then re-enable it. Reinitialize manually if filesystem changes were not detected. VS Code REPL commands do not automatically forward the selected configuration; use the explicit CLI command when executing that environment.

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
