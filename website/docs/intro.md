---
id: intro
title: Introduction
slug: /
---

FScript is a typed scripting language with an F#/ML-style syntax, an interpreter for fast iteration, and an optimizing compiler for .NET. Run `.fss` files as you develop, then compile them into .NET assemblies, self-contained executables, or native binaries with Native AOT. Typed .NET FFI connects scripts to your libraries and host-provided types.

You can also embed the interpreter in .NET applications, browsers, and Node.js to load scripts and call their exported functions without rebuilding the host application.

## Iterate, then compile

Use the interpreter for an edit-and-run workflow:

```sh
fscript hello.fss
```

Compile the script to an optimized .NET assembly when execution speed matters:

```sh
fscript compile hello.fss --target dotnet --output out/hello
dotnet out/hello/hello.dll
```

The .NET compiler specializes functions, lowers self tail calls to loops, removes unused code, and fuses eligible pure collection pipelines to avoid intermediate arrays. Generated programs execute without an FScript interpreter. Performance depends on the workload; the [optimization guide](./compiler/compiler-optimizations.md) explains the transformations and their limits, and links to reproducible FScript/F# benchmarks.

You can publish a self-contained executable, use Native AOT to produce a native binary, or generate a buildable C# project. Start with [Compilation](./compiler/compilation.md) for commands and deployment choices. Native AOT requires the .NET SDK and platform build tools, plus AOT-compatible dependencies.

Compilation is an explicit build step. Running a script with the CLI or embedding it through `ScriptHost` uses the interpreter; JavaScript embedding remains interpreted. Standalone compiled programs use OS permissions rather than the interpreter's sandbox or execution budgets.

## Connect scripts to .NET with FFI

Expose typed functions and host-provided types through extern bindings. Scripts can pass records and callbacks and use opaque resources while the host chooses which operations to expose.

Compiled programs support existing .NET FFI providers through generated adapters. Providers can supply typed CLR bindings for direct calls, dependency trimming, and Native AOT when their libraries support it. The [FFI graphics sample](./examples/ffi-graphics.md) uses FScript for scene generation and animation and a .NET provider for OpenGL rendering.

Read [Embedding](./embedding/overview.md) to register host functions and types, or [Compile to .NET](./compiler/compile-to-dotnet.md) to configure compiled FFI and publishing.

## Write typed scripts

FScript combines expression-oriented code, static type inference, immutable data, and pattern matching. Model data with records and unions, work with lists, maps, and options, and compose curried functions. The interpreter and compiler check language types before execution.

Use it for automation, application rules, embedded scripting, and standalone programs that call .NET libraries. Interpreted embedding lets an application load updated scripts and invoke their exports. Compilation provides an optimized execution path when you are ready to build and distribute a program.

## Suggested learning path

1. Start with [Quickstart](./learn/quickstart) to install FScript and run your first script.
2. Take the [Language Tour](./learn/language-tour), then follow the Language chapters beginning with [Values and Bindings](./language/values-and-bindings).
3. Use [Examples](./examples/guided-examples) to practice complete script patterns and [Stdlib](./stdlib/overview) to explore the built-in modules.
4. Follow [Compilation](./compiler/compilation.md) to build a .NET assembly or publish an executable.
5. Study [Embedding](./embedding/overview) for host integration and [compiled FFI](./compiler/compile-to-dotnet.md#existing-ffi-providers) to use providers in compiled programs.

## Manual structure

- **Learn**: installation and first working scripts.
- **Language**: syntax, data modeling, typing, imports/exports, and layout rules.
- **Examples**: practical scripts and reusable patterns, including .NET FFI.
- **Compilation**: .NET assemblies, C# projects, executable publishing, and optimization behavior.
- **Stdlib**: function reference and recipes.
- **Embedding**: .NET and JavaScript hosts, extern registration, host-provided types, and safety boundaries.
- **Reference**: CLI options, environment configuration, syntax/operators, native types, and the complete function signature index.
