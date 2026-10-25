# Compiler CLI

The compiler emits a versioned FScript LIR artifact, a C# source project, or a standalone .NET assembly.

```text
fscript compile <program.fss> --target ir --output <directory>
fscript compile <program.fss> --target csharp --output <directory>
fscript compile <program.fss> --target dotnet --output <directory>
```

The IR form writes `<directory>/<program-name>.fsir`.

All targets create missing parent directories and deterministically replace files owned by the selected output on a repeated invocation. An unknown target is rejected before the output directory is created. Filesystem failures are reported as `FSA5101` diagnostics.

The C# target writes an independently buildable .NET 10 project. The .NET target writes `<program-name>.dll`, runtime configuration, and managed dependencies. Both support existing FFI providers selected through `fscript.toml` or `--extern-assembly`; `--reference` adds managed dependencies. Generated programs execute compiled methods without an interpreter. Compiled entry points display the final value by default; `--no-result` suppresses that display.

For a self-contained executable, add `--single-file` to the .NET target. For a Native AOT executable, use `--aot`. Publishing defaults to the current platform; pass `--runtime <rid>` to select another target. An explicitly supplied `--runtime` requires a value. Runtime identifiers use .NET names such as `osx-arm64`, `linux-x64`, and `win-x64`. Both options require a .NET 10 SDK for publishing and are rejected for other backends.

See [Compilation](./compilation.md), [Compile to .NET](./compile-to-dotnet.md), [Compile to C#](./compile-to-csharp.md), [Compilation and optimizations](./compiler-optimizations.md), and [Compile and Interpret FScript IR](./interpret-ir.md). The normative representations are documented in the [typed HIR](../specs/typed-hir.md) and [executable LIR](../specs/executable-lir.md) specifications.

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

## Check potential FFI sharing before compiling

Use `fscript analyze program.fss` as a separate step to report potential cross-task
host-value and host-function sharing without running the script. Compilation does
not run this analysis. Warnings are advisory and include both source locations;
a clean check does not prove thread safety. See
[the analysis specification](../specs/static-task-sharing-analysis.md) for host APIs,
provider contracts, and limitations.
