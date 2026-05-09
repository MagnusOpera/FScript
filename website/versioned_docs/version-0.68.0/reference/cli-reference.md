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

## Script environment

When the CLI runs a script, it injects:

```fsharp
type Environment =
  { ScriptName: string option
    Arguments: string list }

let Env : Environment
```

- `Env.ScriptName` is the script path for file execution.
- `Env.ScriptName` is `None` for stdin execution and the REPL.
- `Env.Arguments` contains arguments passed after `--`.

```bash
fscript script.fss -- alice
```

```fsharp
match Env.Arguments with
| name :: _ -> print $"hello {name}"
| [] -> print "hello"
```
