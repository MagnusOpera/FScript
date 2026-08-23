---
id: interpret-ir
title: Interpret IR
slug: /compiler/interpret-ir
---

Compile a complete FScript program to versioned LIR:

```bash
fscript compile program.fss --target ir --output ./out
```

This writes `out/program.fsir`, a canonical JSON artifact containing metadata and validated executable LIR.

## Load from .NET

Reference both repository-local assemblies:

```xml
<ProjectReference Include="path/to/FScript.Compiler/FScript.Compiler.fsproj" />
<ProjectReference Include="path/to/FScript.Interpreter.Lir/FScript.Interpreter.Lir.fsproj" />
```

Then deserialize and execute:

```fsharp
open System.IO
open FScript.Compiler

match File.ReadAllBytes "out/program.fsir" |> IrArtifact.deserialize with
| Error diagnostics ->
    diagnostics
    |> List.iter (fun diagnostic ->
        eprintfn "%s: %s" diagnostic.Code diagnostic.Message)
| Ok program ->
    let context =
        { ScriptName = Some "program.fss"
          Arguments = [ "first"; "second" ] }

    match LirInterpreter.execute context program with
    | Error diagnostic ->
        eprintfn "%s: %s" diagnostic.Code diagnostic.Message
    | Ok execution ->
        execution.Stdout |> List.iter (printfn "%s")
        printfn "Result: %A" execution.Value
```

`ExecutionResult` contains the final typed `LirValue` and ordered `Console.writeLine` output. `Console.readLine` consumes process stdin and returns `None` at EOF; embedding hosts can control it with `Console.SetIn` around execution. The host supplies `Env.ScriptName` and `Env.Arguments` through `InterpreterContext`; execution does not read process arguments implicitly.

## Safety checks

Deserialization checks the artifact format and version, required metadata, DTO structure, and all LIR invariants. Execution validates caller-created LIR again.

- `FSA5xxx`: artifact failures;
- `FSL2xxx`: invalid LIR;
- `FSI3xxx`: execution failures.

Reachable target imports, Tasks, and arbitrary host externs are rejected explicitly. Pure core-language programs, portable builtins, console input/output, and read-only `Fs.readText` execute directly from LIR.

Version-1 `.fsir` is internal and does not promise backward compatibility. Use matching compiler and interpreter builds.

See the [LIR specification](./lir.md) and the repository's [complete interpreter guide](https://github.com/MagnusOpera/FScript/blob/main/docs/guides/interpret-ir.md).
