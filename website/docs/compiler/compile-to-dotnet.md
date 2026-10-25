---
id: compile-to-dotnet
title: Compile to .NET
slug: /compiler/dotnet
---

The .NET backend compiles a typed FScript program into an optimized .NET assembly. For a buildable source project, use [the C# backend](./compile-to-csharp.md). Generated programs do not require the source script, a `.fsir` file, `ScriptHost`, or an interpreter.

## Build-machine requirements

| Operation | Requirements |
| --- | --- |
| Generate a `.dll` with `--target dotnet` | FScript and a .NET 10 runtime; no SDK required. Roslyn is bundled with FScript and emits the assembly in-process. |
| Publish with `--single-file` | .NET 10 SDK available through `dotnet` on PATH. |
| Publish with `--aot` | .NET 10 SDK plus the target platform's native build tools; build on the target OS. |

Running FScript itself from source with `dotnet run` requires the SDK to build FScript. That is separate from the installed compiler's requirements. A generated ordinary DLL needs a .NET 10 runtime to run; published single-file and AOT executables do not require a .NET installation on the destination machine.

## Runnable .NET assembly

```sh
dotnet run --project src/FScript -- compile samples/fibonacci.fss --target dotnet --output out/fibonacci-dotnet
dotnet out/fibonacci-dotnet/fibonacci.dll
```

Output includes a `.dll`, runtime configuration, and required external managed dependencies. The minimum runtime is .NET 10. Arguments after the assembly name become `Env.Arguments`.

```sh
dotnet run --project src/FScript -- compile samples/basic/basic.fss --target dotnet --output out/basic-dotnet
dotnet out/basic-dotnet/basic.dll samples/basic/helloworld.bas
```

Compiled filesystem operations use the process working directory and OS permissions. Interpreter sandbox and execution-budget settings do not apply to standalone binaries.

Assembly compilation resolves known calls and partial applications, lowers self tail calls to loops, and removes unused generated support code. Functions that escape into collections or FFI still use typed delegates. These optimizations apply to the assembly target. See [Compilation and optimizations](./compiler-optimizations.md) for the pass-by-pass comparison and its limits.

## Single-file and Native AOT executables

Both options produce one executable containing the compiled program and its dependencies. A .NET runtime installation is not required on the destination machine.

```sh
# Self-contained bundle with the .NET runtime
fscript compile samples/hello.fss --target dotnet --single-file --runtime osx-arm64 --output out/hello-single
./out/hello-single/hello Ada

# Ahead-of-time compiled native executable
fscript compile samples/hello.fss --target dotnet --aot --runtime osx-arm64 --output out/hello-native
./out/hello-native/hello Ada
```

These flags are available only for `--target dotnet`, and `--runtime` is required. Use .NET runtime identifiers:

| Platform | x64 | Arm64 |
| --- | --- | --- |
| macOS | `osx-x64` | `osx-arm64` |
| Linux | `linux-x64` | `linux-arm64` |
| Windows | `win-x64` | `win-arm64` |

Invalid or unsupported identifiers produce `FSN7101` before SDK publishing. The diagnostic includes the supplied identifier and supported values; `darwin-arm64`, for example, suggests `osx-arm64`.

When invoking FScript from source, put `--` between `dotnet run` options and FScript arguments:

```sh
dotnet run --project src/FScript -- compile samples/hello.fss --target dotnet --aot --runtime osx-arm64 --output out/hello-native
```

Without that separator, `dotnet run` can consume `--runtime` itself and fail while building FScript, before its CLI starts. Such SDK errors cannot be reported by FScript.

Windows output has an `.exe` extension; macOS and Linux output has no extension. Use a fresh output directory when switching from DLL output: existing files are not deleted. External data such as BASIC source files remains program input and is not bundled.

Publishing requires a .NET 10 SDK on the build machine and may restore runtime/compiler packs from NuGet. Native AOT additionally requires the platform's native build tools (Xcode Command Line Tools on macOS; Clang and zlib headers on Linux; Visual Studio C++ build tools on Windows). Native AOT must be built on the target OS; single-file managed publishing can target another OS. The executables still use normal OS libraries. See Microsoft's [single-file](https://learn.microsoft.com/dotnet/core/deploying/single-file/) and [Native AOT](https://learn.microsoft.com/dotnet/core/deploying/native-aot/) deployment documentation.

Single-file publishing trims dependencies when all reachable FFI calls have static CLR bindings. Reachable legacy provider dispatch disables managed trimming to preserve its registration contract. Bundled native runtime libraries may be extracted by the .NET host at startup. Native AOT trims and compiles dependencies, so FFI libraries must be AOT-compatible. Dynamic loading or reflection-heavy providers may produce SDK diagnostics instead of a runnable native binary. Packaging failures are reported as `FSN710x`, including the SDK's diagnostics.

The compiler packages its optimized assembly; it does not recompile the script through the C# source target. The ordinary DLL target remains available without invoking `dotnet publish`.

## C# source project

`--target csharp` generates source instead of an assembly. Its project layout, build commands, and reference handling are covered in [Compile to C#](./compile-to-csharp.md).

## Existing FFI providers

Compilation uses ordinary `fscript.toml` discovery. `--config`, `--no-config`, `--extern-assembly`, and `--no-default-externs` select the environment. Repeat `--reference <assembly.dll>` to add managed references.

```sh
dotnet build samples/ffi/FfiSample.fsproj -c Release
dotnet run --project src/FScript -- compile samples/ffi/main.fss --target dotnet --output out/graphics-dotnet
dotnet out/graphics-dotnet/main.dll --headless
```

Generated adapters call existing providers through the extension SDK and transport records, collections, unions, opaque payloads, callbacks, and tasks. `ExternalCallContext.Apply` invokes compiled functions. Provider factories run during compilation for metadata and once in the generated process when needed.

## Prunable typed FFI bindings

Existing interpreter providers remain unchanged. To make compiled calls direct and suitable for Native AOT, a provider class can additionally expose these public static parameterless metadata methods:

- `FScriptCompiledFunctions() : IEnumerable<KeyValuePair<string, MethodInfo>>`
- `FScriptCompiledTypes() : IEnumerable<KeyValuePair<string, Type>>`

They run only during compilation. Keys are the registered FScript function/type names. Functions map to exact public, closed CLR methods; instance receivers are explicit first arguments. Types map to their CLR representations. Provide semantically equivalent typed entry points and immutable or snapshot-compatible external values. Unsupported native shapes can use explicit scalar/record adapters.

For example, an existing provider can map its `Ext.add` declaration to a typed method:

```csharp
public static long Add(long left, long right) => left + right;

public static IEnumerable<KeyValuePair<string, MethodInfo>>
    FScriptCompiledFunctions() => new[]
    {
        KeyValuePair.Create("Ext.add", typeof(MyProvider).GetMethod(
            nameof(Add), new[] { typeof(long), typeof(long) })!)
    };
```

The compiler emits a direct CLR call without constructing the provider registration, searching a function list, or transporting SDK values for that binding. Only used bindings and external types become compilation roots. Unused provider assemblies are omitted from emitted dependencies. The JIT can inline direct calls; trimmed single-file publishing and Native AOT remove unreachable provider methods and types. Metadata methods themselves are not runtime roots.

No SDK record shape or interpreter registration API change is needed: the optional metadata uses BCL types. `ClrBindings.bindCompiledProviders` exposes the same linking step for compiler API consumers; the CLI discovers the companions automatically for CLR targets. The resulting CLR links remain serializable in LIR.

Legacy SDK provider factories can still be used in DLL and managed single-file output. Their opaque delegate registrations may retain sibling implementations. Native AOT rejects a reachable legacy SDK dispatch bridge with `FSN7105` and requests typed mappings instead of silently suppressing linker warnings.

## Pure FFI and external values

Providers can optionally declare a trusted `Pure` guarantee. Undeclared functions and external types remain conservative.

- A **pure function** is deterministic, has no observable effects, and terminates successfully for valid inputs. Required runtime initialization is part of this promise. Returning the same value while logging, mutating hidden state, reading a clock, or sometimes failing does not qualify.
- A **pure external type** has deeply immutable visible data and opaque payloads. Registered field reads, structural equality, and formatting must also be deterministic, effect-free, and terminating successfully. A readonly CLR property alone does not establish this guarantee. Constructors and ordinary methods need their own function declarations.

For callback-taking functions, the promise applies when supplied callbacks satisfy the same guarantee. The optimizer separately checks callback effects and remains conservative for unknown callbacks. A pure factory does not automatically make a returned callable pure.

Expose these public static parameterless methods on the provider class:

```csharp
public static IEnumerable<string> FScriptPureFunctions() => new[]
{
    "Ext.add"
};

public static IEnumerable<string> FScriptPureTypes() => new[]
{
    "Point"
};
```

Names must exactly match the provider's registered script functions and types. The example assumes the provider registers both `Ext.add` and `Point`. Empty or absent metadata means no guarantees. Unknown names and attempts to override protected runtime contracts are compilation errors.

The CLI reads this metadata during compilation for IR, C#, and .NET output. It can be used with either existing SDK adapters or direct [typed CLR mappings](#prunable-typed-ffi-bindings). Existing SDK registration records do not need to change. These methods are not runtime roots and do not add FScript declaration syntax.

Compiler API users can supply the same information explicitly:

```fsharp
open FScript.Language
open FScript.Compiler

let purity =
    { PurityContracts.empty with
        PureFunctions = Set.singleton "Ext.add"
        PureTypes = Set.singleton "Point" }

// registration and input are the host's existing compiler inputs.
Compiler.compileWithRegistrationAndPurity registration purity input
```

`Compiler.compileWithPurity` accepts contracts with ordinary `CompileInput.Externs`; `ClrBindings.providerPurity` discovers contracts from provider `MethodInfo` values. Ordinary compilation APIs default to empty contracts.

The compiler can remove an unused pure call and then prune its binding, adapters, and dependencies when no other retained code needs them. Effects or failures in its argument expressions are still preserved. A type guarantee permits removal of unused field reads, equality, and formatting when the complete computation is proven pure. It does not make its factory or methods pure.

Declarations are trusted, not verified by executing arbitrary provider code. Incorrect claims can remove behavior or failures after optimization. Normal AST interpretation and SDK boundary validation are unchanged. See [Compilation and optimizations](./compiler-optimizations.md#trusted-external-purity) for the pruning rules.

## Compiler APIs

Use `Compiler.compileWithRegistration` for host signatures. `ClrBindings.bindMethod` links an exact public CLR method or constructor; instance receivers are explicit first arguments. `ClrBindings.bindType` projects a native CLR type. `ClrBindings.bindProvider` preserves SDK providers. These declarative links survive IR serialization.

`DotNetBackend.generate` accepts `OutputKind = Library` for library assemblies or `ConsoleApplication` for executables. Generated classes expose `Run`, `FormatResult`, and exported bindings. `CSharpBackend.generate` emits source files instead.

Representation and CLR linkage diagnostics use `FSC6xxx`, including failures from the assembly target's shared emitter. Assembly emission and publishing diagnostics use `FSN7xxx`. Frontend and IR errors retain their own codes and source spans. Roslyn errors or warnings prevent assembly output. Reachable open polymorphic results and generalized initializers with effects remain compilation diagnostics under whole-program specialization.
