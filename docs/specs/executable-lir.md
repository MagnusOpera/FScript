# Executable Low-Level IR (LIR)

## Status and purpose

This document specifies FScript's executable low-level intermediate representation. LIR is backend-independent, structured, non-SSA, and suitable for both direct interpretation and source generation.

LIR is produced only from validated HIR:

```text
Hir.Program
  -> LirLowering.lower
  -> LirOptimization.optimize
  -> LirValidation.validate
  -> Lir.Program
```

The optimization pass performs conservative effect analysis, constant Boolean control-flow simplification, unused pure-local elimination, and whole-program reachability pruning. Observable top-level initializers and exports remain ordered roots; calls, failures, loops, and potentially failing integer division/remainder are never discarded. Closure captures are recomputed and surviving function IDs are deterministically compacted before validation. LIR and the JSON DTO schema used by `.fsir` are internal versioned contracts, not stable public interchange standards.

## Program model

`Lir.Program` contains:

- concrete named type declarations;
- imported record linkage and field schemas;
- lifted functions;
- ordered top-level declarations;
- an optional entry expression.

`SymbolId` identifies values and bindings. `FunctionId` independently identifies lifted function bodies. Both are deterministic artifact-local integers.

Every expression carries its semantic FScript type and source span. Rich types remain available for validation, interpretation, and backend-specific specialization and representation selection.

## Functions and closures

Every source lambda is lifted into a `Lir.Function` containing:

- one parameter ID and type;
- an ordered, explicit capture list;
- a return type;
- a source span;
- a body expression.

`MakeClosure(functionId, captures)` constructs a closure. `CallClosure` applies one argument. `PipeClosure` preserves value-first pipeline evaluation. The optimizer rewrites calls in function tail position to `TailCallClosure` or `TailPipeClosure`; keeping two nodes preserves their distinct evaluation order. Partial application follows naturally from nested unary closures.

`LetRecursiveGroup` establishes all recursive values before their bodies run. Lifted functions validate their capture list against the ordered free-symbol set of their body.

Tail position propagates through selected branch and decision-tree bodies and through the body of lexical binding nodes, but never through operands, guards, aggregate construction, or a call whose result is subsequently consumed. The LIR interpreter trampolines explicit tail calls across closures, so deep proper-tail recursion does not consume the host stack.

## Operations and control flow

LIR uses explicit operations for integer and floating arithmetic, integer remainder, structural equality, numeric comparisons, and Boolean operations.

Control flow remains structured:

- `Branch` represents `if`;
- `Let` and `LetRecursiveGroup` represent lexical scope;
- `Fail` represents `raise`;
- `ForEach` represents ordered list iteration;
- `DecisionTree` represents pattern matching.

Operands normally evaluate left to right and stop at the first failure. `BooleanAnd` and `BooleanOr` short-circuit. Branches evaluate only the selected body.

## Runtime values and aggregates

LIR directly constructs and projects:

- unit, integer, float, Boolean, and string constants;
- tuples and lists;
- options;
- records and record updates;
- declared tagged unions;
- ordered maps;
- type tokens.

Named records and unions retain declarations in the program type table. `RecordConvert` carries frontend-proven structural conversion evidence between distinct representations.

LIR prescribes semantic values rather than a backend storage strategy. The .NET interpreter may use tagged runtime values; the Go backend must specialize reachable polymorphic uses into closed concrete representations and does not emit Go generics.

Maps are ordered string-key maps according to current FScript semantics. Consumers must not substitute host-language hash-map iteration order.

Structural equality recursively covers primitives, tuples, records, maps, options, lists, unions, and type tokens. Closures, task values, constructors, and extern handles are not structurally equal.

## Pattern decision plans

Pattern lowering produces a `DecisionTree` whose target expression is evaluated exactly once. Each case contains a `PatternPlan`, optional guard, body, and source span.

A plan consists of:

- projection steps such as list head/tail, tuple item, record field, option value, and union payload;
- checks for literals, list shape, tuple arity, record fields, options, union cases, and type shape;
- symbol bindings associated with deterministic projections;
- ordered map matching plans, including remainder handling.

Consumers execute this plan; the Go backend does not reimplement source-pattern semantics.

Cases are attempted in source order. Bindings become visible only after a plan succeeds. Guards run after bindings and before the body. Exhausting every case produces the standard match failure.

## Linkage and reachability

LIR retains the same explicit linkage categories as HIR:

- portable `Intrinsic` values;
- `RuntimeContext` for `Env`;
- `HostExternal` handles;
- `TargetImport` handles.

The LIR interpreter rejects a reachable fully-applied target import, Task operation, or unsupported host external with an `FSI3xxx` diagnostic. The Go backend rejects reachable Task or unsupported host-external linkage with `FSB4xxx`. Unused unsupported declarations remain inert.

## Validation

`LirValidation.validate` returns `Result<Lir.Program, CompilerDiagnostic list>` and checks at least:

- unique symbol and function IDs;
- known named types and valid recursive type references;
- valid nested spans;
- resolved loads, captures, and function IDs;
- exact closure capture lists and call signatures;
- primitive arity and operand/result types;
- aggregate, projection, update, and conversion representations;
- decision-plan checks, bindings, and map projections;
- declaration and function result types;
- known intrinsic names and complete linkage metadata.

LIR/lowering diagnostics use `FSL2xxx` codes.

Both `LirInterpreter.execute` and `GoBackend.generate` validate caller-provided LIR before consuming it.

## Artifact format

`IrArtifact.serialize` writes one complete `Lir.Program` as canonical indented UTF-8 JSON. The artifact includes:

- `format = "fscript-lir"`;
- `irVersion = 1`;
- compiler and language versions;
- normalized source paths and SHA-256 hashes when file-backed sources are available;
- ordered foreign dependencies;
- the complete LIR program.

`IrArtifact.deserialize` rejects an unknown format or version before constructing LIR, validates required metadata, reconstructs the program through explicit DTO parsing, and runs LIR validation before returning it. Artifact diagnostics use `FSA5xxx` codes.

No backward-compatibility guarantee is made for version 1. Producers and consumers should use matching FScript builds.

## Execution APIs

The principal APIs are:

```fsharp
IrArtifact.serialize : Lir.Program -> byte array
IrArtifact.deserialize : byte array -> Result<Lir.Program, CompilerDiagnostic list>

LirInterpreter.execute :
  InterpreterContext -> Lir.Program -> Result<ExecutionResult, CompilerDiagnostic>
```

`InterpreterContext` supplies an optional script name and argument list. `ExecutionResult` contains the final `LirValue` and ordered `Console.writeLine` output. `Console.readLine` consumes the interpreter process stdin and returns `None` at end of input; hosts that need controlled input can temporarily supply a `TextReader` through `Console.SetIn` around execution.

`Lir.format : Lir.Program -> string` is a deterministic debug/snapshot representation and is not the artifact encoding.
