---
id: compile-to-csharp
title: Compile to C#
slug: /compiler/csharp
---

The C# backend writes an independently buildable .NET 10 project. Use it when generated source belongs in your build, when you need to inspect a translated program, or when you want to make further changes in C#.

For FScript's additional call, closure, tail-recursion, and pruning optimizations, use the [.NET assembly target](./compile-to-dotnet.md). Building generated C# does not enable those assembly-only passes.

## Generate, build, and run

```sh
fscript compile hello.fss --target csharp --output out/hello-csharp
dotnet build out/hello-csharp/Program.csproj -c Release
dotnet run --project out/hello-csharp/Program.csproj -c Release -- Ada
```

Building the generated project requires the .NET 10 SDK. From the FScript checkout, generation can instead use:

```sh
dotnet run --project src/FScript -- compile samples/hello.fss --target csharp --output out/hello-csharp
```

Generation writes source and a project file; it does not invoke the SDK to build that project. After building, execution uses the generated program and its dependencies, without an interpreter, source script, or `.fsir` artifact.

Generated executables print the final value by default. Add `--no-result` when compiling to suppress that display and permit pure discarded work to be pruned. Explicit `Console.writeLine` calls remain observable. Successful execution returns status `0`, independently of the final value; `Env.setExitCode` selects another status without stopping execution. The generated `Program.ExitCode` property exposes the selected status to callers without changing the host process. See [results and shell status](../reference/cli-reference.md#results-and-shell-status).

## Project layout

```text
out/hello-csharp/
  Program.cs
  Runtime.cs
  Program.csproj
  RuntimeSDK.cs     # when SDK provider adapters are needed
```

`Program.cs` contains generated declarations, concrete data types, helper methods, and the entry point. `Runtime.cs` supplies collection and runtime support. SDK-backed programs also receive `RuntimeSDK.cs` and explicit managed assembly references.

The default generated class is `FScript.Generated.Program`. Its entry point calls `Run`, formats the final FScript value, and writes it to stdout. Console writes in the script appear before that final result. Exports use typed public properties; genuine function values use typed delegates.

Generated names and internal layout may change between compiler versions. Regeneration replaces compiler-owned files, so keep hand-written changes separately or work in another directory.

## Types and functions

Reachable polymorphic uses are specialized to concrete types. Integers become `long`, floats become `double`, Booleans become `bool`, and strings become `string`. Lists use arrays, options use typed structs, maps use ordered entries, and records, tuples, and unions use generated typed declarations. FScript equality and formatting use dedicated helpers.

A curried function may appear as nested `Func<A, B>` delegates. Captures and partial applications can remain visible in the output. The source target shares specialization and simplification with the assembly target, but does not run its additional native-call lowering, self-tail-call loop conversion, or aggressive generated-member pruning. Ordinary C# release compilation and the .NET JIT still perform their own optimizations.

See [Compilation and optimizations](./compiler-optimizations.md) for the full distinction.

## .NET FFI and references

Provider selection uses the same `fscript.toml`, `--config`, `--extern-assembly`, and `--no-default-externs` options as other CLR compilation workflows. Add explicit dependencies with repeated `--reference` arguments:

```sh
fscript compile extension.fss --target csharp --output out/extension-csharp --extern-assembly path/to/Provider.dll --reference path/to/Dependency.dll
```

The generated project records assembly references using paths resolved on the compiling machine. Keep those assemblies available while building. When moving the project, update reference paths or replace them with your own project/package references. The SDK copies private managed dependencies into the application's build output.

Generated adapters support registered host values, callbacks, and tasks. Compiler-only typed mappings can instead link exact CLR methods and native types. See [typed FFI bindings](./compile-to-dotnet.md#prunable-typed-ffi-bindings) for the provider contract and compatibility requirements.

`--single-file`, `--aot`, and `--runtime` are FScript CLI options for `--target dotnet`; they are rejected with `--target csharp`. Use that target for FScript-managed publishing. You can also maintain and publish an owned C# project through your normal .NET build process, subject to its dependencies' trimming and AOT requirements.

## Diagnostics

The C# backend reports `FSC6xxx` diagnostics, including unsupported CLR signatures and representations that cannot be specialized. Shared frontend and IR errors retain their own codes and source spans.

An unconstrained result such as `let identity value = value` followed by `identity` has no concrete native signature. Apply it at a concrete type or give its boundary an explicit concrete annotation. A generalized initializer is also rejected when specialization would duplicate an observable effect.
