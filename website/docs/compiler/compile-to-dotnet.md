---
id: compile-to-dotnet
title: Compile to .NET or C#
slug: /compiler/dotnet
---

Both targets compile typed HIR/LIR to ordinary methods and delegates. Generated programs do not require the source script, a `.fsir` file, `ScriptHost`, or an interpreter.

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

Assembly compilation resolves known calls and partial applications, lowers self tail calls to loops, and removes unused generated support code. Functions that escape into collections or FFI still use typed delegates. These optimizations apply to the assembly target; the C# source target retains its existing output policy.

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

Windows output has an `.exe` extension; macOS and Linux output has no extension. Use a fresh output directory when switching from DLL output: existing files are not deleted. External data such as BASIC source files remains program input and is not bundled.

Publishing requires a .NET 10 SDK on the build machine and may restore runtime/compiler packs from NuGet. Native AOT additionally requires the platform's native build tools (Xcode Command Line Tools on macOS; Clang and zlib headers on Linux; Visual Studio C++ build tools on Windows). Native AOT must be built on the target OS; single-file managed publishing can target another OS. The executables still use normal OS libraries. See Microsoft's [single-file](https://learn.microsoft.com/dotnet/core/deploying/single-file/) and [Native AOT](https://learn.microsoft.com/dotnet/core/deploying/native-aot/) deployment documentation.

Single-file publishing trims dependencies when all reachable FFI calls have static CLR bindings. Reachable legacy provider dispatch disables managed trimming to preserve its registration contract. Bundled native runtime libraries may be extracted by the .NET host at startup. Native AOT trims and compiles dependencies, so FFI libraries must be AOT-compatible. Dynamic loading or reflection-heavy providers may produce SDK diagnostics instead of a runnable native binary. Packaging failures are reported as `FSN710x`, including the SDK's diagnostics.

The compiler packages its optimized assembly; it does not recompile the script through the C# source target. The ordinary DLL target remains available without invoking `dotnet publish`.

## C# source project

```sh
dotnet run --project src/FScript -- compile samples/fibonacci.fss --target csharp --output out/fibonacci-csharp
dotnet run --project out/fibonacci-csharp/Program.csproj
```

The project contains `Program.cs` and BCL helpers in `Runtime.cs`. Extension programs also contain `RuntimeSDK.cs` and explicit SDK references. Build and edit this project independently with the .NET SDK.

## Existing FFI providers

Compilation uses ordinary `fscript.toml` discovery. `--config`, `--no-config`, `--extern-assembly`, and `--no-default-externs` select the environment. Repeat `--reference <assembly.dll>` to add managed references.

```sh
dotnet build samples/ffi/FfiSample.fsproj -c Release
dotnet run --project src/FScript -- compile samples/ffi/main.fss --target dotnet --output out/graphics-dotnet
dotnet out/graphics-dotnet/main.dll --headless
```

Generated adapters call existing providers through the extension SDK and transport records, collections, unions, opaque payloads, callbacks, and tasks. `ExternalCallContext.Apply` invokes compiled functions. Provider factories run during compilation for metadata and once in the generated process when needed.

Go cannot call .NET providers: reachable CLR FFI bindings produce `FSB4010`.

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

## Compiler APIs

Use `Compiler.compileWithRegistration` for host signatures. `ClrBindings.bindMethod` links an exact public CLR method or constructor; instance receivers are explicit first arguments. `ClrBindings.bindType` projects a native CLR type. `ClrBindings.bindProvider` preserves SDK providers. These declarative links survive IR serialization.

`DotNetBackend.generate` accepts `OutputKind = Library` for library assemblies or `ConsoleApplication` for executables. Generated classes expose `Run`, `FormatResult`, and exported bindings. `CSharpBackend.generate` emits source files instead.

C# diagnostics use `FSC6xxx`; assembly diagnostics use `FSN7xxx`. Roslyn errors or warnings prevent assembly output. Reachable open polymorphic results and generalized initializers with effects remain compilation diagnostics under whole-program specialization.
