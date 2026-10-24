# Compile to .NET or C#

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

## Compiler APIs

Use `Compiler.compileWithRegistration` for host signatures. `ClrBindings.bindMethod` links an exact public CLR method or constructor; instance receivers are explicit first arguments. `ClrBindings.bindType` projects a native CLR type. `ClrBindings.bindProvider` preserves SDK providers. These declarative links survive IR serialization.

`DotNetBackend.generate` accepts `OutputKind = Library` for library assemblies or `ConsoleApplication` for executables. Generated classes expose `Run`, `FormatResult`, and exported bindings. `CSharpBackend.generate` emits source files instead. See the [backend specification](../specs/dotnet-backends.md).

C# diagnostics use `FSC6xxx`; assembly diagnostics use `FSN7xxx`. Roslyn errors or warnings prevent assembly output. Reachable open polymorphic results and generalized initializers with effects remain compilation diagnostics under whole-program specialization.
