# Compile and Interpret FScript IR

FScript can compile one complete source program into a versioned `.fsir` artifact and execute its LIR from .NET without returning to parser AST evaluation.

## Produce an artifact

```bash
fscript compile program.fss --target ir --output ./out
```

For `program.fss`, this writes:

```text
out/program.fsir
```

From the repository:

```bash
dotnet run --project src/FScript -- \
  compile program.fss --target ir --output ./out
```

The artifact is canonical indented JSON. It contains the complete validated LIR program, format/version metadata, compiler and language versions, source hashes for file-backed inputs, and ordered foreign dependencies.

Treat `.fsir` as an internal build artifact. Version 1 has no backward-compatibility guarantee; use matching compiler and interpreter builds.

## Reference the interpreter

The compiler and interpreter are separate assemblies. In a repository-local .NET project, add:

```xml
<ItemGroup>
  <ProjectReference Include="path/to/FScript/src/FScript.Compiler/FScript.Compiler.fsproj" />
  <ProjectReference Include="path/to/FScript/src/FScript.Interpreter.Lir/FScript.Interpreter.Lir.fsproj" />
</ItemGroup>
```

`FScript.Compiler` owns LIR and artifact serialization. `FScript.Interpreter.Lir` owns execution. Both APIs currently use the `FScript.Compiler` namespace.

## Load and execute

```fsharp
open System
open System.IO
open FScript.Compiler

let artifact = File.ReadAllBytes "out/program.fsir"

match IrArtifact.deserialize artifact with
| Error diagnostics ->
    for diagnostic in diagnostics do
        eprintfn "%s: %s" diagnostic.Code diagnostic.Message
    1
| Ok program ->
    let context =
        { ScriptName = Some "program.fss"
          Arguments = [ "first"; "second" ] }

    match LirInterpreter.execute context program with
    | Error diagnostic ->
        eprintfn "%s: %s" diagnostic.Code diagnostic.Message
        1
    | Ok execution ->
        for line in execution.Stdout do
            printfn "%s" line

        printfn "Result: %A" execution.Value
        0
```

`ExecutionResult.Stdout` contains `Console.writeLine` output in execution order. `Console.readLine` reads the interpreter process stdin and returns `None` at EOF; an embedding host can provide deterministic input with `Console.SetIn` around execution. `ExecutionResult.Value` is an `LirValue`, with cases for primitives, tuples, lists, records, options, unions, ordered maps, type tokens, and callable/runtime handles.

Applications should pattern-match `LirValue` when they need a stable domain conversion instead of relying on `%A` formatting.

## Compile in memory

Source can also be compiled and executed without writing `.fsir`:

```fsharp
open FScript.Compiler
open FScript.Language

let input =
    { Program = Parser.parseProgram "let add x y = x + y\nadd 1 2"
      Externs = [] }

match Compiler.compile input with
| Error diagnostics -> Error diagnostics
| Ok compiled ->
    LirInterpreter.execute
        { ScriptName = None; Arguments = [] }
        compiled.Lir
    |> Result.mapError List.singleton
```

Use `parseFileWithIncludes` through the normal host/CLI path when compiling file-backed programs with include resolution. `Parser.parseProgram` parses only the supplied source text.

## Validation and failures

Deserialization performs all of these checks before returning a program:

1. JSON and required metadata are valid.
2. `format` is `fscript-lir`.
3. `irVersion` is supported.
4. DTO fields reconstruct a complete LIR program.
5. `LirValidation.validate` accepts the reconstructed program.

`LirInterpreter.execute` validates caller-created LIR again before execution.

Diagnostic families are:

- `FSA5xxx`: artifact format, version, metadata, or reconstruction;
- `FSL2xxx`: malformed or inconsistent LIR;
- `FSI3xxx`: execution failure or unsupported reachable linkage.

An executed `raise`, failed match, integer division by zero, or invalid collection operation returns a structured interpreter diagnostic. The interpreter rejects reachable target imports, Tasks, and arbitrary host externs. Unused unsupported declarations remain inert.

## Runtime context

`InterpreterContext` supplies the compiler runtime-context intrinsic:

```fsharp
type Environment = {
  ScriptName: string option
  Arguments: string list
}

let Env : Environment
```

The interpreter does not read process arguments implicitly. The embedding host decides exactly which script name and arguments the program receives.
