# Compilation

FScript can run inside a host application or compile into a program that runs on its own. Compilation checks the same language types, resolves imported scripts, and produces ordinary .NET code. Generated programs do not load FScript source or start an interpreter.

## Choose an output

| Output | Command options | Use it when |
| --- | --- | --- |
| .NET assembly | `--target dotnet` | You want compiled execution with FScript's assembly optimizations. Run the DLL with .NET 10. |
| Self-contained executable | `--target dotnet --single-file [--runtime <rid>]` | You want one distributable executable that includes the .NET runtime. |
| Native AOT executable | `--target dotnet --aot [--runtime <rid>]` | You want dependencies trimmed and compiled into a native executable. |
| C# project | `--target csharp` | You want to inspect, edit, or integrate generated source in a .NET build. |

The [optimization guide](./compiler-optimizations.md) explains the shared passes and the additional work performed by the assembly target.

## Compile a first program

Save this as `hello.fss`:

```fsharp
let name =
    match Env.Arguments with
    | first :: _ -> first
    | [] -> "world"

Console.writeLine $"Hello {name}!"
```

Compile and run it:

```sh
fscript compile hello.fss --target dotnet --output out/hello
dotnet out/hello/hello.dll Ada
```

The program writes `Hello Ada!`, followed by `()`, its final FScript result. Arguments after the assembly or executable name become `Env.Arguments`. `Env.ScriptName` contains the running assembly or executable filename.

From a checkout, replace `fscript` with `dotnet run --project src/FScript --` in these commands.

## Language and runtime behavior

Records, unions, tuples, lists, options, maps, closures, patterns, and recursive functions retain their FScript semantics. Equality remains structural, collections remain immutable, Boolean operators short-circuit, and top-level effects execute once in source order. An exported binding uses `export let`.

Script imports are resolved at compile time. An application needs its managed dependencies and any files it reads as input, but does not need the original `.fss` files to execute.

Standalone applications use the process working directory and operating-system permissions. Interpreter sandbox restrictions and execution budgets are not carried into compiled binaries.

## Host integration

Both targets support .NET FFI selected through the normal environment configuration. Existing SDK providers can run through generated adapters. Typed CLR mappings enable direct calls and make provider code suitable for trimming and Native AOT when its dependencies support them.

See [Compile to .NET](./compile-to-dotnet.md) for FFI configuration, typed bindings, publishing, and runtime identifiers, or [Compile to C#](./compile-to-csharp.md) for project layout and reference handling.

## Compiling and embedding

Compilation is an explicit CLI or compiler API operation. Running `fscript program.fss` or using ordinary `ScriptHost` embedding continues to use the AST interpreter. JavaScript embedding also remains interpreted.

The separate [IR workflow](./interpret-ir.md) produces a versioned `.fsir` artifact and executes it with the LIR interpreter. It is useful for compiler tooling and conformance work; it is not required to run compiled .NET programs.
