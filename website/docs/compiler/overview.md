---
id: compiler-overview
title: Compiler Overview
slug: /compiler/overview
---

FScript has a typed compiler pipeline in addition to its established embedded evaluators:

```text
source
  -> resolved typed frontend
  -> validated HIR
  -> executable LIR
  -> .fsir artifact
       |-> LIR interpreter
       `-> Go source backend
```

The frontend remains FScript-specific. Go consumes backend-independent LIR; it does not introduce a second set of language typing rules.

## What you can do

- Compile a complete `.fss` program into one versioned `.fsir` artifact.
- Deserialize and execute LIR from the separate .NET interpreter assembly.
- Generate a complete Go 1.25 project without Go generics.
- Use directly representable Go imports through explicit `[<Import(...)]` declarations.
- Compare behavior across the existing evaluator, LIR interpreter, and generated Go runtime.

Start with [Compile to Go](./compile-to-go.md) or [Interpret IR](./interpret-ir.md). The [HIR](./hir.md) and [LIR](./lir.md) pages describe the compiler contracts.

## Assembly boundaries

| Assembly | Responsibility |
| --- | --- |
| `FScript.Language` | Parsing, inference, and the resolved typed frontend |
| `FScript.Compiler` | HIR, LIR, lowering, validation, and `.fsir` artifacts |
| `FScript.Interpreter.Lir` | Executing validated LIR |
| `FScript.Backend.Go` | Generating Go projects and embedding the Go runtime |

The compiler does not reference either consumer. This keeps LIR reusable for future backends.

## Initial support boundary

The compiler covers the core language, closures, recursion, pattern matching, records and unions, collections, structural equality, the portable pure builtin catalog, console input/output, and `Env`. Generated Go additionally supports the complete `Fs` surface, `Regex.matchGroups`, and typed tasks.

Reachable tasks and arbitrary host externs receive explicit diagnostics in the LIR interpreter. The Go backend executes tasks with goroutines and rejects only host externs outside its supported catalog. Existing .NET and JavaScript execution paths remain available for other host-oriented scripts.

The compiler APIs and version-1 artifact schema are currently internal and are not published as stable standalone packages.
