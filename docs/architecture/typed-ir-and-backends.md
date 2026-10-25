# Typed IR and Backend Architecture

## Pipeline

```text
source -> parser/includes -> inference + resolved typed tree -> HIR -> LIR
                                                                      |
                                                                      +-> serialization
                                                                      +-> LIR interpreter
                                                                      +-> C# source backend
                                                                      +-> optimized .NET assembly backend
```

`FScript.Language` owns parsing, inference, and the reference evaluator and remains Fable-compatible. Its compiler inference path emits a recursive resolved typed tree in the same inference pass as the compatibility `TypedProgram`. Expressions carry final instantiated types, source spans, lexical reference kinds, primitive operations, and structural/named-record coercion evidence. Ordinary inference and evaluation APIs retain their direct path without constructing compiler telemetry.

Resolved inference assigns deterministic artifact-local IDs to builtins, externs, types, union cases, declarations, parameters, pattern bindings, and locals in source order. Type variables normalize independently of global inference counters. Non-recursive initializers see preceding bindings; recursive groups see every peer atomically. Repeated included source locations and shadowed names retain distinct lexical identities.

`FScript.Compiler` owns HIR, LIR, validation, lowering, optimization, specialization, and artifact serialization. It depends on Language and references no backend. Exported signatures come from the same inference result, avoiding duplicate inference for embedded hosts.

Validated LIR consumers are separate leaf assemblies: `FScript.Interpreter.Lir` owns LIR values and execution, `FScript.Backend.CSharp` emits independent source projects, and `FScript.Backend.DotNet` emits optimized assemblies. Public modules retain the `FScript.Compiler` namespace while assembly ownership remains explicit.

## IR boundaries

HIR consumes the semantic resolved tree rather than parser AST nodes or compatibility telemetry. It retains complete semantic types, schemes, attributes, declaration shapes, imports, exports, source spans, and explicit record conversions. Source IDs remain unchanged; compiler-synthesized IDs occupy a separate higher range. HIR and LIR contain no target-language storage types.

LIR is structured, executable, and non-SSA. Lambdas become lifted functions with explicit ordered captures; closure creation references function IDs. Pattern matching becomes ordered projection, check, binding, and map-consumption plans. Consumers execute these plans rather than interpreting source patterns. A scrutinee is evaluated once, guards follow source order, and repeated bindings require structural equality.

Named references remain nominal, with record and union declaration shapes carried separately. Local pattern bindings reuse decision trees. Ranges, loops, interpolation, indexing, record updates, and conversions preserve reference evaluation order. Top-level expressions become ordered synthetic declarations; the final entry references the last result without evaluating it twice. Recursive closure groups initialize atomically, including local mutual recursion.

The pipeline is resolved inference, HIR lowering and validation, LIR lowering, `LirOptimization.optimize`, and LIR validation. Shared optimization performs conservative effect/use analysis, Boolean simplification, unused pure-binding elimination, deterministic reachability, and proper-tail-call marking. Ordered roots include observable initializers and exports. Captures and function IDs are rebuilt after pruning. Ordinary and pipeline calls remain distinct, preserving function-first and value-first evaluation respectively.

Validation checks IDs, references, types, schemes, spans, closure captures, call signatures, branches, pattern projections, aggregates, primitives, and import linkage. Artifact loading and every execution/generation entry point validate caller-provided LIR. Canonical debug printers normalize type variables, paths, source locations, and ordering; snapshots protect determinism.

## Native specialization and assembly optimization

`NativeSpecialization`, `LirMonomorphization`, and `LirSimplification` are shared by the C# and .NET backends. They instantiate reachable polymorphic declarations, functions, local bindings, and captures at concrete use types. Keys combine source identity and normalized concrete types, and recursive bodies are memoized before traversal. Unused pure generalized declarations may disappear; effectful generalized initializers cannot be cloned. Bottom expressions do not need an invented storage representation.

Shared specialization diagnostics use `FSL21xx`; native consumers translate them into their target diagnostic families. Open reachable types and unsafe effectful cloning remain explicit failures. Simplification beta-reduces small known closures, propagates constants and direct projections, removes unused pure calls, refreshes captures, and repeats reachability. Unknown callbacks, imports, failures, and I/O remain observable.

`LirCommonSubexpressions` is shared by both compiled targets and runs after their other LIR reductions. It uses `LirEffects` and external-type immutability contracts for dominance-based value numbering. It retains first-evaluation order, separates conditional regions and loop bodies, clears availability at observable operations, and emits ordinary lexical lets. Function invocations have independent local bindings. Key normalization excludes spans but includes type identity and floating-point bits. Temporary cleanup removes administrative bindings that are not shared. No new LIR/artifact representation, runtime cache, or interpreter pass is required.

The .NET backend adds an assembly-only optimization path: static saturated calls, partial-application reduction, closure propagation, self-tail-call loops, typed collection callbacks, immediate-block lowering, and generated-member pruning. It reuses the C# representation emitter through an internal entry point; the ordinary C# source target does not run these extra passes. Roslyn emits valid release IL. See the [native backend specification](../specs/dotnet-backends.md).

The .NET backend can publish its optimized PE as a self-contained single-file or Native AOT executable. A temporary SDK project's post-compile target replaces its placeholder assembly with that PE before publication. No interpreter or hosting layer participates. Publishing requires an explicit .NET runtime identifier and installed SDK/toolchain.

## Interpretation, runtime context, and FFI

The LIR interpreter uses its own tagged values and environments, not source AST nodes or the reference evaluator. It implements the pure scalar, string, list, option, and ordered-map catalog, with curried callback invocation and structural equality. Explicit tail calls run through a stack-safe trampoline. It can initialize a validated program once and invoke exports repeatedly without repeating top-level effects.

Host linkage in artifacts consists of names, schemes, arity, and target descriptors, never executable delegates or opaque payloads. Unused unsupported handles are inert; fully applying unsupported linkage produces a stage-specific diagnostic. Interpreter callback registrations are separate from serialized LIR. Task signatures survive every IR stage, although fully applied tasks without interpreter adapters report `FSI3021`.

`Env` is an intrinsic runtime-context value. LIR receives script name and arguments through `InterpreterContext`; native executables use process arguments. Console output preserves order, and the LIR interpreter exposes captured lines through `ExecutionResult.Stdout`. Native backends implement runtime services according to their documented catalog.

`Compiler.compileWithRegistration` accepts the frontend's `ExtensionRegistration`, retaining registered structural and host-managed types, unions, and constructors. `ClrBindings.bindMethod` and `bindType` serialize exact CLR linkage without embedding reflection objects. Typed provider companions enable direct calls; legacy SDK adapters remain available for managed output. Reachability removes unused external types and provider dependencies; static bindings allow executable publishing to trim provider methods and types.

Backend `Import` attributes remain structured language metadata on bodyless typed function declarations and imported type schemas. They do not create a second type system. Each consumer accepts only linkage it supports. Unused target imports are inert; the LIR interpreter reports `FSI3023` when an unsupported import is fully applied.

## Trusted host purity

A separate `PurityContracts` value supplies registered pure function and type names without changing SDK record shapes. Provider companion methods expose this metadata using BCL `IEnumerable<string>` values; the CLI reads them before resolved inference. Native consumers receive declarative flags and external application arities through HIR/LIR and serialization, with conservative defaults for older artifacts.

`LirEffects` centralizes native totality/effect analysis, propagating known function summaries and callback conditions while treating recursive cycles, potentially failing operations, and unknown external reads conservatively. Unused pure calls can disappear before emission, allowing their SDK adapters and provider dependencies to become unreachable. The AST interpreter does not query these compiler-only companions or change evaluation order. See [FFI purity](../guides/compile-to-dotnet.md#pure-ffi-and-external-values).

## Artifacts and conformance

Versioned `.fsir` artifacts contain the complete LIR program, compiler/language versions, normalized source paths and SHA-256 hashes, and ordered foreign dependencies. Deserialization rejects incompatible format/version, malformed metadata, and programs that fail LIR validation. The format is internal and carries no backward-compatibility guarantee.

The conformance suite compares the reference evaluator with serialized/deserialized LIR execution for values, stdout, and failure categories. Native backend tests independently execute emitted assemblies, validate IL, and cover FFI, external types, collections, recursion, and specialization.

## Reference execution

The `feature/in-memory-execution` experiment is reference material only; its dynamic assembly architecture is neither copied nor referenced. Normal CLI and `ScriptHost` execution retain the reference AST evaluator. JavaScript embedding remains unchanged. The LIR interpreter is a separate execution/reference consumer, and native compilation is explicit.
