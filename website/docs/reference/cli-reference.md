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

## Compile programs

Generate one versioned whole-program IR artifact:

```bash
fscript compile program.fss --target ir --output ./out
```

The IR target writes `out/program.fsir`. Unsupported features use structured compiler/backend diagnostic codes.

Generate a standalone .NET assembly or an independently buildable C# project:

```bash
fscript compile program.fss --target dotnet --output ./out/dotnet
dotnet ./out/dotnet/program.dll
fscript compile program.fss --target csharp --output ./out/csharp
dotnet run --project ./out/csharp/Program.csproj
```

Compiled executables display the final value by default. Compile with `--no-result` to suppress that display; explicit output such as `Console.writeLine` is preserved.

These targets support .NET FFI providers selected by the normal environment configuration. See [Compile to .NET](../compiler/compile-to-dotnet.md) and [Compile to C#](../compiler/compile-to-csharp.md).

For a self-contained executable, add `--single-file` to the .NET target. For a Native AOT executable, use `--aot`. Publishing defaults to the current platform; pass `--runtime <rid>` to select another target. An explicitly supplied `--runtime` requires a value. Runtime identifiers use .NET names such as `osx-arm64`, `linux-x64`, and `win-x64`. Both options require a .NET 10 SDK for publishing and are rejected for other backends.

Continue with [Compilation](../compiler/compilation.md), [Compilation and optimizations](../compiler/compiler-optimizations.md), or [Interpret IR](../compiler/interpret-ir.md).

## From source

```bash
dotnet run --project src/FScript -- script.fss
```

## Options

| Option | Meaning |
| --- | --- |
| `--help` | Show usage and options |
| `--debug-adapter` | Run the DAP adapter used by editor integrations |
| `--debug-capabilities` | Print the debugger protocol capabilities as JSON |
| `--root <path>`, `-r <path>` | Override filesystem/import root |
| `--config <path>`, `-c <path>` | Override automatic selection with an explicit environment manifest |
| `--no-config` | Disable automatic manifest loading |
| `--no-default-externs` | Omit the 21 default runtime externs; retain the core library |
| `--extern-assembly <path>` | Add providers from an assembly; repeat to add more |
| `--` | End CLI parsing; following arguments become `Env.Arguments` |

Explicit flag paths and the positional script resolve from the working directory. The default root is the script's directory in file mode, and the working directory in stdin/REPL mode. `version` is a standalone command, not a script entry point.

## Invocation modes

| Input | Behavior |
| --- | --- |
| `fscript script.fss` | Parse, type-check, and execute the file, resolving imports |
| `cat script.fss \| fscript` | Execute redirected source; imports are unsupported |
| `fscript` with terminal stdin | Start a persistent REPL |
| `fscript version` | Print the application version |

File/stdin execution prints the final value after any console output. Failures return nonzero with parse/type/evaluation diagnostics, including source locations when available.

## REPL

`>` starts a submission; `.` indicates pending multiline input. Bindings and type declarations persist after successful submissions. Expression results are printed; functions display an inferred signature. Complete input may run after each line; two blank lines force a pending submission. Type/evaluation errors clear pending input. EOF exits, attempting pending input first; Ctrl+C exits immediately.

REPL submissions do not resolve file imports. Use file mode for multi-file scripts. Arguments after `--` require file/stdin mode.

## Option examples

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

- `Env.ScriptName` is `Some "script.fss"` (the filename, not the full path) for file execution.
- `Env.ScriptName` is `None` for stdin execution and the REPL.
- `Env.Arguments` contains arguments passed after `--`.

```bash
fscript script.fss -- alice
```

```fsharp
match Env.Arguments with
| name :: _ -> Console.writeLine $"hello {name}"
| [] -> Console.writeLine "hello"
```

## Shared external-function configuration

The CLI loads `fscript.toml` beside the script automatically. Stdin and REPL use the working directory. It does not search parents or nested folders, and imported files retain the entry environment. Use `--config <path>` or `-c <path>` to override selection, or `--no-config` to disable it; combining them is an error:

```bash
fscript --config ./fscript.toml scripts/build.fss -- alice
fscript -c ./tools/fscript.toml --root /tmp/sandbox scripts/test.fss
```

```toml
[runtime]
root = "."
default-externs = true
extern-assemblies = ["./extensions/AppExterns.dll"]
```

The positional script remains the entry point. Without one, terminal input starts the REPL and redirected stdin runs as a script. Automatic discovery uses only the script directory or stdin/REPL working directory.

Manifest paths resolve from its directory. Explicit `--root` overrides the configured root; `--no-default-externs` always disables defaults. Configured assemblies and repeated `--extern-assembly` flags combine in that order with normalized duplicates removed. Invalid manifests or failed provider registration stop execution with a nonzero exit.

VS Code can select the same manifest for completion and type checking. See [Environment Configuration](./environment-configuration) for defaults, selection rules, trust, and embedding APIs.

## Debugging

Use the [VS Code debugger](../learn/debugging.md) to launch the installed CLI in an
integrated terminal. The adapter's protocol uses separate streams from script
input/output. Debugger modes are standalone invocations and cannot be combined
with ordinary file-execution flags.

## Results and shell status

Printing the final value is the default for interpreted file/stdin execution and compiled applications. Use `--no-result` to suppress that display:

```sh
fscript --no-result program.fss
fscript compile program.fss --target dotnet --no-result --output out/program
fscript compile program.fss --target csharp --no-result --output out/source
```

The compilation flag is baked into the generated entry point; it is not consumed from a compiled application's arguments. Explicit `Console.writeLine` output and diagnostics remain visible. The REPL also accepts `--no-result`. IR artifacts retain their program values and do not accept this compilation flag.

The final value never becomes a process exit status. Successful execution returns `0` unless `Env.setExitCode` selects another status. An unhandled failure still returns a nonzero status, even if the program selected `0` beforehand. Suppressing a result does not suppress failures or stop evaluation. Compilers may remove discarded work only when it is proven pure and total.

```fsharp
Env.setExitCode 7
Console.writeLine "Evaluation continues."
42
```

This prints the explicit message and, by default, `42`; the shell receives status `7`. With `--no-result`, only the explicit message is printed and the status remains `7`.
