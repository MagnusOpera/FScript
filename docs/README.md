# Documentation

This folder is organized into three layers:

- Getting started and comparative docs (`docs/guides/`)
- Language behavior specifications: [`docs/specs/`](./specs/README.md)
- Internal structure and component design: [`docs/architecture/`](./architecture/README.md)

## Start here

- Tutorial: [`guides/getting-started-tutorial.md`](./guides/getting-started-tutorial.md)
- REPL guide: [`guides/repl.md`](./guides/repl.md)
- FScript vs F# / OCaml: [`guides/fsharp-ocaml-differences.md`](./guides/fsharp-ocaml-differences.md)
- Language rationale: [`guides/language-choices.md`](./guides/language-choices.md)

## Compilation

- [Build FScript from source](./guides/building.md)
- [Choose a compilation target](./guides/compilation.md)
- [Compile to .NET](./guides/compile-to-dotnet.md): assemblies, single-file executables, Native AOT, and typed FFI
- [Compile to C#](./guides/compile-to-csharp.md): generated projects, source layout, and managed references
- [Compilation and optimizations](./guides/compiler-optimizations.md): shared passes, assembly-only transformations, and pruning limits
- [Compiler CLI reference](./guides/compiler-cli.md)
- [Compile and interpret IR](./guides/interpret-ir.md)

## User references

The [website manual sources](../website/docs/intro.md) separate a guided learning path from lookup material:

- [Standard library overview](../website/docs/stdlib/overview.md) and [complete signature index](../website/docs/reference/stdlib-reference.md)
- [Syntax and operators](../website/docs/reference/syntax-reference.md) and [native types](../website/docs/reference/native-types-reference.md)
- [CLI](../website/docs/reference/cli-reference.md), [environment configuration](../website/docs/reference/environment-configuration.md), and [editor settings/commands](../website/docs/reference/editor-reference.md)
- [.NET embedding](../website/docs/embedding/real-world-embedding.md), [JavaScript embedding](../website/docs/embedding/fable-javascript.md), and [host-provided types](../website/docs/embedding/host-managed-types.md)

## Deep dives

- Specifications index: [`specs/README.md`](./specs/README.md)
- Architecture index: [`architecture/README.md`](./architecture/README.md)

## Debugger development

`make test` includes CLI/DAP integration tests and headless native callbacks.
`make vscode-debug-smoke` launches an isolated real VS Code Extension Development
Host and checks a breakpoint, locals, a safe watch, and resumption. Set
`VSCODE_EXECUTABLE` when VS Code is not at the standard macOS location or on PATH.
Linux needs a graphical session or Xvfb. The smoke test creates a temporary
workspace and user-data directory, with `fscript.cli.path` pointing to the
repository build.

See the [debugger specification](specs/debugger.md) for the runtime API and
execution model.

`make benchmark-evaluation` measures normal evaluation without a debugger. See the
[evaluator benchmark](../benchmarks/FScript.Evaluation.Benchmarks/README.md) for
workloads and comparisons against a published engine.
