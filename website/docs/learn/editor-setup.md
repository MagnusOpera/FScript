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
