---
id: compiler-lir
title: Executable LIR Specification
slug: /compiler/lir
---

LIR is FScript's backend-independent executable representation. It is structured, non-SSA, and does not assume Go syntax or Go generics.

```text
validated HIR
  -> LirLowering.lower
  -> LirOptimization.optimize
  -> LirValidation.validate
  -> Lir.Program
```

The optimization pass conservatively removes unreachable declarations, lifted functions, and unused pure locals. It also folds constant Boolean branches and short-circuit expressions. Observable top-level initializers and exports remain ordered roots; closure captures and function IDs are rebuilt deterministically before validation.

## Program and closures

`Lir.Program` contains named type declarations, imported-record linkage, lifted functions, ordered top-level declarations, and an optional entry expression. Every expression retains a semantic type and source span.

Lambdas become unary lifted functions with explicit ordered captures. `MakeClosure` constructs callable values and `CallClosure` applies one argument, preserving currying and partial application. Recursive groups establish every peer closure before use.

## Data and operations

LIR directly represents primitive operations, branches, bindings, failures, tuples, lists, options, records, record conversions, tagged unions, ordered maps, ranges, loops, indexing, interpolation, type tokens, and linkage handles.

LIR retains semantic polymorphism. The Go backend specializes each reachable instantiation into concrete containers and native typed closures, while the LIR interpreter continues to use its own tagged execution values.

Structural equality recursively covers data values and type tokens. Functions, Tasks, constructors, and extern handles are not structurally equal.

## Pattern plans

Source patterns lower to deterministic decision plans. The target evaluates once; projections extract values; checks test shape and cases; bindings receive stable IDs; map clauses retain ordered matching and remainder behavior.

The interpreter and Go runtime execute the same plan. The Go emitter does not recreate source-pattern logic.

## Validation and artifacts

LIR validation checks IDs, named types, spans, loads, captures, calls, operations, aggregate representations, decision plans, imports, intrinsics, and declaration/function result types. Failures use `FSL2xxx` diagnostics.

The `.fsir` serializer writes canonical JSON containing `format = "fscript-lir"`, `irVersion = 1`, compiler/language versions, source hashes, ordered dependencies, and the complete program. Artifact failures use `FSA5xxx`.

Both execution and Go generation validate caller-provided LIR. Version 1 is internal and has no backward-compatibility guarantee.

Read the repository's [complete normative LIR specification](https://github.com/MagnusOpera/FScript/blob/main/docs/specs/executable-lir.md).
