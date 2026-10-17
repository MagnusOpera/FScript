---
id: editor-setup
title: Editor Setup (VS Code)
slug: /learn/editor-setup
---

FScript has an official VS Code extension with Language Server support.

## Install

Choose your marketplace:

- VS Code Marketplace: https://marketplace.visualstudio.com/items?itemName=MagnusOpera.fscript
- Open VSX: https://open-vsx.org/extension/MagnusOpera/fscript

## What you get

The extension provides:

- syntax highlighting,
- diagnostics,
- completion,
- hover,
- document symbols,
- go-to-definition and references,
- rename,
- semantic tokens,
- inlay hints.

## Runtime requirement

The extension uses .NET runtime acquisition via `.NET Install Tool` when needed and can also use `dotnet` from your `PATH`.

## Recommended first workflow

1. Install the extension.
2. Open a folder containing `.fss` files.
3. Create `hello.fss` and type a small function.
4. Confirm diagnostics/completion are active.
5. Run the script from terminal with `fscript hello.fss`.

## Related docs

- Continue with [Language Tour](./language-tour).
- Go deeper in [Language chapters](/manual/language/values-and-bindings).

## Numeric signatures

The extension highlights `numeric` annotations and shows inferred arithmetic signatures such as `(x: numeric) -> (y: numeric) -> numeric`. An `int` or `float` annotation specializes a shared arithmetic type. Mixed numeric types and nonnumeric calls are reported as errors.

See [Type System](../language/type-system) for examples and the type hierarchy. The complete [settings and commands reference](../reference/editor-reference.md) lists extension defaults and execution commands.

## Expose host functions and types

For functions supplied by a .NET assembly, add a workspace environment manifest:

```toml
[runtime]
extern-assemblies = ["./extensions/AppExterns.dll"]
```

The assembly must contain an [extern provider](../embedding/register-externs#assembly-providers-for-cli-and-editor). Its registered schemes and host-type descriptors drive diagnostics, completion, hover, signature help, and inlay hints. Host-managed values expose only their declared visible fields.

One discovered `fscript.toml` is selected automatically. With multiple manifests, use **FScript: Select Configuration**. The picker includes **No configuration** and **Automatic selection**; each workspace folder has an independent persisted choice. A nonempty folder setting `fscript.config` overrides the picker, and the configuration status item shows the effective choice. All files in a folder use its selected environment, without nearest-file switching or manifest inheritance.

Workspace trust is required to execute provider registration. Restricted Mode keeps default analysis available. Saved manifest changes and provider DLL changes restart only the affected folder server and reanalyze open buffers. Failed initialization reports manifest diagnostics and removes stale configured signatures.

Run the same manifest in a terminal with `fscript --config ./fscript.toml hello.fss`. VS Code REPL commands do not automatically adopt the selection. See [Environment Configuration](../reference/environment-configuration) for precedence, root defaults, multi-root workspaces, and DLL replacement on Windows.

## Develop the extension with F5

Open the FScript repository and launch **Run VS Code (FScript Extension Dev)**. The prelaunch task bundles the extension, and its development host asynchronously builds the local server once per activation. Wait for server status to become ready, then open `samples/hello.fss`; `Env.Arguments` provides field hover/completion from the built-in `Environment` type.

After changing extension source or settings metadata, stop the debug session and press F5 again. **FScript: Show Language Server Output** opens FScript-specific logs. A custom `fscript.server.path` overrides the development server; clear it when testing the local build.
