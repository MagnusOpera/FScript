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

Compiled entry points print the final value by default. Add `--no-result` when compiling to suppress it and allow proven-pure discarded computations to be pruned. Explicit output, failures, and exit-code writes remain observable. Without this flag, library callers can retrieve the typed value through `Run` and format it explicitly with `FormatResult`; in discard mode, `Run` returns unit.

Output includes a `.dll`, runtime configuration, and required external managed dependencies. The minimum runtime is .NET 10. Arguments after the assembly name become `Env.Arguments`.

```sh
dotnet run --project src/FScript -- compile samples/basic/basic.fss --target dotnet --output out/basic-dotnet
dotnet out/basic-dotnet/basic.dll samples/basic/helloworld.bas
```

Compiled filesystem operations use the process working directory and OS permissions. Interpreter sandbox and execution-budget settings do not apply to standalone binaries.

Assembly compilation resolves known calls and partial applications, lowers self tail calls to loops, and removes unused generated support code. Functions that escape into collections or FFI still use typed delegates. These optimizations apply to the assembly target. See [Compilation and optimizations](./compiler-optimizations.md) for the pass-by-pass comparison and its limits.

## Project defaults and output

`fscript compile main.fss` defaults to a managed .NET assembly in `bin/main/dotnet/` beside the entry script. `--target` and `--output` are optional. Add a `[compile]` table to the script's `fscript.toml` to choose a different default:

```toml
[compile]
backend = "dotnet"
aot = true
size-optimized = true
```

With this manifest, the same command publishes one native executable to `bin/main/dotnet/<rid>/aot/`, using the build host's runtime identifier. Set `output = "artifacts"` to choose a directory relative to the manifest instead. CLI paths remain relative to the working directory.

Explicit CLI options take precedence. Use `--no-size-optimized` to keep configured AOT with full framework diagnostics, `--no-aot` for a managed DLL, or `--target csharp` for source output. See [Compile defaults](../reference/environment-configuration.md#compile-defaults) for all properties, mode precedence, and output conventions.

## Single-file and Native AOT executables

Both options produce one executable containing the compiled program and its dependencies. A .NET runtime installation is not required on the destination machine.

```sh
# Self-contained bundle with the .NET runtime
fscript compile samples/hello.fss --target dotnet --single-file --output out/hello-single
./out/hello-single/hello Ada

# Ahead-of-time compiled native executable
fscript compile samples/hello.fss --target dotnet --aot --output out/hello-native
./out/hello-native/hello Ada
```

These flags are available only for `--target dotnet`. When `--runtime` is omitted, publishing targets the current .NET host runtime identifier (`RuntimeInformation.RuntimeIdentifier`), so both commands above build for the current platform. Pass `--runtime <rid>` to select another target. An explicitly supplied `--runtime` still requires a value. Use .NET runtime identifiers:

| Platform | x64 | Arm64 |
| --- | --- | --- |
| macOS | `osx-x64` | `osx-arm64` |
| Linux | `linux-x64` | `linux-arm64` |
| Windows | `win-x64` | `win-arm64` |

Invalid or unsupported identifiers produce `FSN7101` before SDK publishing. The diagnostic includes the supplied identifier and supported values; `darwin-arm64`, for example, suggests `osx-arm64`.

When invoking FScript from source, put `--` between `dotnet run` options and FScript arguments:

```sh
dotnet run --project src/FScript -- compile samples/hello.fss --target dotnet --aot --output out/hello-native
```

Without that separator, `dotnet run` can consume `--runtime` itself and fail while building FScript, before its CLI starts. Such SDK errors cannot be reported by FScript.

Windows output has an `.exe` extension; macOS and Linux output has no extension. Use a fresh output directory when switching from DLL output: existing files are not deleted. External data such as BASIC source files remains program input and is not bundled.

Publishing requires a .NET 10 SDK on the build machine and may restore runtime/compiler packs from NuGet. Native AOT additionally requires the platform's native build tools (Xcode Command Line Tools on macOS; Clang and zlib headers on Linux; Visual Studio C++ build tools on Windows). Native AOT must be built on the target OS; single-file managed publishing can target another OS. The executables still use normal OS libraries. See Microsoft's [single-file](https://learn.microsoft.com/dotnet/core/deploying/single-file/) and [Native AOT](https://learn.microsoft.com/dotnet/core/deploying/native-aot/) deployment documentation.

Single-file publishing trims dependencies when all reachable FFI calls have static CLR bindings. Reachable legacy provider dispatch disables managed trimming to preserve its registration contract. Bundled native runtime libraries may be extracted by the .NET host at startup. Native AOT trims and compiles dependencies, so FFI libraries must be AOT-compatible. Dynamic loading or reflection-heavy providers may produce SDK diagnostics instead of a runnable native binary. Packaging failures are reported as `FSN710x`, including the SDK's diagnostics.

The compiler packages its optimized assembly; it does not recompile the script through the C# source target. The ordinary DLL target remains available without invoking `dotnet publish`.

## Native AOT executable size

Since 0.87.1, Native AOT favors smaller native code through .NET's `OptimizationPreference=Size`. Ordinary `--aot` retains culture support, framework exception text, runtime stack traces, and EventSource.

Add `--size-optimized` when the program can give up those facilities:

```sh
fscript compile samples/ffi/main.fss --target dotnet --aot --size-optimized --output out/ffi-small
./out/ffi-small/main --headless
```

The FFI example requires its provider to be built first, as described in [FFI graphics](../examples/ffi-graphics.md). `--size-optimized` requires `--target dotnet --aot`; it cannot be used with ordinary DLL or managed single-file output.

| Runtime capability | Ordinary `--aot` | With `--size-optimized` |
| --- | --- | --- |
| Globalization | Culture support retained | Invariant globalization; named cultures may fail |
| Framework exception messages | Full messages | Resource keys |
| Runtime stack traces | Retained | Disabled |
| EventSource | Retained | Disabled |

Provider and FScript error messages remain intact. Reflection analysis and trimming/AOT warnings remain enabled in both profiles.

### Teapot measurements

The FFI teapot executable on macOS ARM64 with .NET SDK 10.0.403 measured:

| Build | Executable bytes | Reduction |
| --- | ---: | ---: |
| Before 0.87.1 | 4,378,616 | |
| Ordinary `--aot` | 2,181,464 | 50.2% |
| `--aot --size-optimized` | 1,882,776 | 57.0% |

Sizes vary with SDK and target platform. These are executable file lengths, excluding optional debug-symbol files. Both profiles passed standalone headless execution and a real three-frame OpenGL check.

Most of the removable payload was GLFW: the provider embedded eight native libraries totaling 2.22 MB. Executable publishing now retains only the selected target's library. The ordinary provider DLL keeps all supported platforms for interpreted execution. Changes to vertex-buffer allocation and callback storage also remove unnecessary FSharp.Core dependencies. FSharp.Core was already trimmed; its managed DLL size did not represent its contribution to the native executable.

The smaller result is about 1.80 MiB, still 408,216 bytes larger than a 1,474,560-byte floppy. This includes the runtime, renderer, target GLFW library, and compiled script. The [investigation report](https://github.com/MagnusOpera/FScript/blob/main/docs/architecture/native-aot-size.md) records compiler-map analysis, measured alternatives, and rejected techniques. Microsoft's [Native AOT optimization guidance](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/optimizing) and [trimming options](https://learn.microsoft.com/en-us/dotnet/core/deploying/trimming/trimming-options) explain the supported settings and their tradeoffs.

## C# source project

`--target csharp` generates source instead of an assembly. Its project layout, build commands, and reference handling are covered in [Compile to C#](./compile-to-csharp.md).

## Existing FFI providers

`compile` automatically loads `fscript.toml` beside the entry script, even when invoked from another directory. `analyze` uses the same discovery rule. The manifest selects local FFI assemblies and NuGet providers, so neither command needs those providers repeated as assembly flags. Compilation includes their colocated managed dependencies automatically; use `--reference <assembly.dll>` for additional managed assemblies.

Manifest assembly paths resolve from the manifest directory. `--config <path>` selects another manifest relative to the working directory; `--no-config` disables manifest loading. These options are mutually exclusive. Discovery does not search parent directories or fall back to a working-directory manifest. Explicit `--extern-assembly` paths resolve from the working directory and add providers to the selected environment. `--no-default-externs` disables the built-in externs.

For the teapot example, `samples/ffi/fscript.toml` supplies the provider path:

```toml
[runtime]
extern-assemblies = ["bin/Release/net10.0/FScript.Ffi.Sample.dll"]
```

Build the provider first, then analyze or compile without extra assembly flags:

```sh
dotnet build samples/ffi/FfiSample.fsproj -c Release
dotnet run --project src/FScript -- analyze samples/ffi/main.fss
dotnet run --project src/FScript -- compile samples/ffi/main.fss --target dotnet --no-aot --output out/graphics-dotnet
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

## Explicit FFI Inline contracts

A provider can opt individual typed bindings into .NET **IL expansion**, independently of `Pure` or thread safety:

```csharp
public static IEnumerable<string> FScriptInlineFunctions() => new[]
{
    "Ext.add"
};
```

Every name must also appear in `FScriptCompiledFunctions()`. This is compiler-controlled rewriting after Roslyn emission, not `AggressiveInlining` or a request left to the JIT. A profitable eligible callee body replaces its marked call sites; arguments still evaluate once in source order. Two script names may link the same CLR method while only one opts in. Compiler API users can select a binding with `ClrBindings.bindInlineMethod name methodInfo lir`.

`Inline` permits code expansion but promises neither purity nor successful termination. Effectful calls and failures remain observable. Providers must select call-site-independent implementations: method identity, caller-sensitive reflection, stack walking, security-stack behavior, and stack traces cannot be preserved when a frame is removed. Do not opt such implementations in. No unmarked external helper body is copied automatically.

The initial supported bodies are public static nongeneric managed methods using public operands. Parameters/results/locals must not require pointer or by-reference support. Methods with module/type initializers, exception handlers, recursive self calls, unsupported prefixes/call-frame instructions, synchronized/native/runtime implementations, or `NoInlining` are rejected. Calls to public checked helpers can remain inside an expanded body. Instance methods and constructors are not supported yet. A reachable unsupported request reports `FSN7201`; metadata/rewrite failures report `FSN7202`. Unused bindings impose no body-eligibility requirement.

Expansion remaps parameters, locals, branches, and returns, preserves fresh zeroed callee locals, substitutes safe argument temporaries, folds total integer arithmetic with CLR wraparound semantics, and removes temporary markers. Exact forwarders become direct calls without introducing spill locals. Newly unreferenced provider dependencies can disappear. Emission remains deterministic, and tests JIT-check the results and independently check representative rewrites with ILVerify when installed.

The opt-in survives `.fsir` serialization in CLR linkage. Older links default to no expansion. C# output retains ordinary typed calls; interpreters ignore this compiler-only metadata. It does not inline SDK delegate transport. A provider can expose typed `HostManagedValue` entry points to retain its existing SDK host-value representation while avoiding general SDK dispatch; all payload, aliasing, identity, and validation guarantees still belong to the provider.

### Size and growth policy

`Inline` is permission, not a force-inline directive. The initial byte screens follow [.NET 10 RyuJIT](https://github.com/dotnet/runtime/blob/v10.0.0/src/coreclr/jit/inline.h#L1017-L1023): up to **16 original IL bytes** is tiny; **17–100 bytes** is discretionary; over [the normal 100-byte screen](https://github.com/dotnet/runtime/blob/v10.0.0/src/coreclr/jit/compiler.h#L10260-L10266) remains a typed call. This is not a claim of universally optimal thresholds or a reproduction of RyuJIT's profile/native-cost model. F#'s [weighted expression threshold](https://github.com/dotnet/fsharp/blob/main/src/Compiler/Optimize/Optimizer.fs) uses different units and is not copied as an IL byte limit.

The backend tries expansion in a private copy of the caller and measures encoded IL after argument cleanup and constant folding. Tiny sites may add at most **32 bytes**; discretionary sites at most **16 bytes**. Each caller has a cumulative growth allowance of `max(32, min(256, originalCallerBytes))`. Candidates with more than 32 arguments or locals stay calls. Rejected trials leave no spill locals, markers, or changed exception regions. Unprofitable or oversized requests are not compilation errors; eligibility diagnostics apply to bodies selected for attempted expansion.

Exact forwarding adapters collapse without body duplication or positive growth, so they do not consume that budget. The JIT remains free to optimize retained calls. This keeps NDArray's tiny adapters eligible while larger checked kernels remain calls. Thresholds can be revisited against the checked-in comparisons; there is no force override or new CLI tuning flag.

## Compiler APIs

Use `Compiler.compileWithRegistration` for host signatures. `ClrBindings.bindMethod` links an exact public CLR method or constructor; instance receivers are explicit first arguments. `ClrBindings.bindType` projects a native CLR type. `ClrBindings.bindProvider` preserves SDK providers. These declarative links survive IR serialization.

`DotNetBackend.generate` accepts `OutputKind = Library` for library assemblies or `ConsoleApplication` for executables. Generated classes expose `Run`, `FormatResult`, and exported bindings. `CSharpBackend.generate` emits source files instead.

Representation and CLR linkage diagnostics use `FSC6xxx`, including failures from the assembly target's shared emitter. Assembly emission and publishing diagnostics use `FSN7xxx`. Frontend and IR errors retain their own codes and source spans. Roslyn errors or warnings prevent assembly output. Reachable open polymorphic results and generalized initializers with effects remain compilation diagnostics under whole-program specialization.

## Exit status and embedding

`Env.setExitCode : int -> unit` selects the eventual shell status without stopping evaluation or task execution. Valid codes are `0` through `255`; the default is `0`. The last completed write wins. Writes from tasks are synchronized, but their scheduling order can change the final code. For a predictable code, await the tasks and set it in the main flow. Existing task completion and failure rules still apply; the setter does not add cancellation or bypass shutdown.

The generated `Program.ExitCode` property exposes status to embedding callers. Calling `Run` or an exported function does not change the host process's `Environment.ExitCode`. The executable `Main` returns the selected status after normal completion. Unhandled failures take precedence and return a nonzero process status.

The compiler APIs `CSharpBackend.generateWithResultOutput`, `DotNetBackend.generateWithResultOutput`, and `DotNetBackend.generateExecutableWithResultOutput` take a Boolean display policy before their ordinary options. Existing generation APIs default to printing. Passing `false` discards the entry value before specialization and prunes unused pure work; exported library bindings remain roots.
