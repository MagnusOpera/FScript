---
id: cli-reference
title: CLI Reference
slug: /reference/cli
---

## Common commands

```bash
# Run script
fscript script.fss

# Pass arguments to script
fscript script.fss -- arg1 arg2

# Start REPL
fscript

# Print version
fscript version
```

## From source

```bash
dotnet run --project src/FScript -- script.fss
```

## Useful options

```bash
# Override root
dotnet run --project src/FScript -- --root /tmp/sandbox script.fss

# Disable default externs
dotnet run --project src/FScript -- --no-default-externs script.fss

# Add extern providers from assembly
dotnet run --project src/FScript -- --extern-assembly ./MyExterns.dll script.fss
```
