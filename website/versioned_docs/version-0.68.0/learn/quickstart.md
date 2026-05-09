---
id: quickstart
title: Quickstart
slug: /learn/quickstart
---

## Install

### Homebrew

```bash
brew install magnusopera/tap/fscript
```

### From source

```bash
git clone https://github.com/MagnusOpera/FScript.git
cd FScript
make build
```

## Your first script

Create `hello.fss`:

```fsharp
let name = "FScript"
print $"Hello, {name}!"
```

Run it:

```bash
fscript hello.fss
```

## Run from repository source

```bash
dotnet run --project src/FScript -- hello.fss
```

## REPL

Start an interactive session:

```bash
fscript
```

## Script arguments

You can pass arguments after `--`:

```bash
fscript hello.fss -- alice bob
```

Inside scripts, arguments are available via `Env.Arguments`.

## Editor support (recommended)

For day-to-day scripting, install the official VS Code extension from:

- VS Code Marketplace: https://marketplace.visualstudio.com/items?itemName=MagnusOpera.fscript
- Open VSX: https://open-vsx.org/extension/MagnusOpera/fscript

It provides diagnostics, completion, hover, go-to-definition, references, rename, semantic tokens, and inlay hints.

Full setup guide: [Editor Setup (VS Code)](./editor-setup).
