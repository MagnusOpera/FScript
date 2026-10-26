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

The .NET backend can publish its optimized PE as a self-contained single-file or Native AOT executable. A temporary SDK project's post-compile target replaces its placeholder assembly with that PE before publication. No interpreter or hosting layer participates. The CLI defaults the .NET runtime identifier to the current host and accepts an explicit override; the backend API receives the resolved identifier. Publishing requires the installed SDK/toolchain.

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

## Result display and exit status

Result display defaults to enabled and is independent of shell status. CLI `--no-result` disables it; native consumers run `LirResultPolicy.discard` before specialization so unused pure result work can be pruned while observable initialization remains rooted. `LirTraversal` supplies common child mapping and complete symbol census for transformations and fresh lexical bindings.

`Env.setExitCode` is a reserved effectful core operation that coexists with the runtime `Env` record. The evaluator owns a locked status cell shared with its tasks; generated programs use an atomic per-program field and return its value from `Main`. Neither embedding surface sets the host process status. LIR exposes status alongside values. Fable/JavaScript ignores the setter and keeps returning/displaying ordinary values. Task ordering remains scheduling-dependent, and existing task failure/lifetime rules are unchanged.

## Optional task-sharing analysis

`FScript.Language.TaskSharingAnalysis` is an independent consumer of resolved
typed programs. CLI analysis and host pre-execution analysis share this pass;
compilation does not run it. It owns its alias, closure, logical
task-clock, and diagnostic state only while requested; evaluators and generated
programs have no hooks or added synchronization. Hosting adapters reuse the
inference result rather than inferring twice. See the
[static analysis contract](../specs/static-task-sharing-analysis.md).

## Opt-in foreign IL rewriting

Provider `Inline` metadata is owned by `FScript.Compiler.ClrBindings` and stored in serialized CLR target linkage, separately from language-level purity. `FScript.Backend.CSharp` emits temporary binding-specific markers only in assembly mode. After Roslyn emission, `FScript.Backend.DotNet.DotNetIlInlining` uses Mono.Cecil to expand explicitly opted-in foreign bodies, clean up locals/total constants, and remove unused dependency scopes. The C# target and interpreted paths perform no IL rewriting. Unsupported bodies produce structured diagnostics. The backend dependency is isolated from Language, Fable, and interpreted hosting.

Foreign IL expansion uses a two-stage profitability policy: original-byte screening followed by measured post-cleanup caller growth. Trial bodies are isolated copies, including locals and exception boundaries; failed trials leave ordinary typed calls. Exact forwarder collapsing does not duplicate code. This caps static expansion without introducing JIT profiling, runtime instrumentation, or interpreter costs.


## Capture cleanup after tail-loop lowering

The assembly backend specializes known captured functions before native lowering. A local partial function whose implementation and captures are known can become direct arithmetic in its consumer, including a recursive loop. Actual captures remain explicit; separate invocations retain separate immutable environments.

After tail calls become loops, the backend removes unused private native parameters and their inert arguments, then removes unreachable local closure cycles and capture snapshots. These passes iterate to a fixed point before member pruning. Delegate method-group references retain their original signatures. Calls and external property reads with possible effects or failures remain ordered. Escaping functions and unknown callbacks can still require delegates. This cleanup does not change interpreter execution or serialized LIR.


## Temporary records and representation conversions

Both compiled targets scalar-replace internal record temporaries used only by field projections, updates, or compatible width conversions. Field expressions keep their original evaluation order and execute once, including unused fields that perform I/O or fail. A projection through a compatible internal record conversion can read the original field directly. A record returned under a different internal record signature can be constructed in the destination representation.

The .NET backend repeats this cleanup after known-call reduction and reachability pruning, so removing an unused closure can expose further temporary records. Records that escape through unknown consumers or captures remain ordinary values. Imported record constructors/getters and nested field adaptations are conservative boundaries; this pass does not treat them as plain field copies. Record scalar replacement alone does not fuse collection pipelines or eliminate their materialized result collections; the separate assembly fusion pass can remove intermediate arrays and expose further record elimination.


## Collection representation propagation

The assembly backend carries a consistent internal record element representation through known `List.map`, `List.filter`, and `List.fold` consumers. Single-use lifted callbacks can receive the producer's representation directly; their original field semantics are preserved by explicit conversions that subsequent scalar replacement can remove. A private collection binding can share that representation between compatible consumers without converting the complete array.

Public returns, escaping/captured collection bindings, imported records, and unknown callbacks retain conversion boundaries. Callback evaluations and stage order are unchanged. This pass does not require callbacks to be pure because it changes representations rather than interleaving their execution. The C# source target and serialized IR retain their existing representations.


## Pure collection pipeline fusion

The assembly backend fuses single-consumer `List.map` and `List.filter` chains ending in `List.fold` when the complete region is proven pure and total. It evaluates callback setup once, constructs a bounded tail-recursive loop in private LIR, and reuses native call reduction, scalar replacement, and tail-loop lowering. Intermediate arrays disappear; nonescaping mapped records can disappear as well. Shared or escaping producer bindings remain materialized.

Range sources can feed the loop directly, including descending ranges. The last endpoint is processed before deciding whether to increment, avoiding overflow at signed integer limits. Other list sources are evaluated once and indexed with a checked, typed option decision. Unknown calls, I/O, tasks, possible failures and incomplete matches prevent fusion; their existing stage order remains intact. The shared effect analysis proves unguarded exhaustive union matches and irrefutable typed tuple/record patterns total, while retaining guards and repeated-binding checks as possible failures. No public IR node or serialization change is needed.


## Native tuple temporaries

The assembly backend replaces local tuple constructors used only for irrefutable destructuring with independent item bindings. Every item still evaluates once in source order. Guards, repeated bindings, nested coercions and escaping uses retain normal pattern decisions and values.

Small tuples of up to four simple scalar/reference fields use generated readonly value types in the .NET target, allowing native fold accumulators and method arguments to avoid heap carriers. Larger or nested/opaque layouts retain reference carriers. The source target keeps its existing tuple representation. Both targets preserve the semantic tuple type and dedicated structural equality/formatting; no CLR record equality or boxing is required for typed native tuple operations. Value-type constructor `newobj` instructions do not themselves imply heap allocation, so allocation measurements remain the check for per-iteration cost.


## Private list-tail views

Native method parameters, local list values and list-pattern bindings can use a readonly view of an owned array plus offset and length. `List.tail` and cons-pattern remainders then advance a view instead of copying every suffix. Native tail-call reassignment preserves that representation; known collection loops use a value-type enumerator or index the view directly. Equality and formatting have typed view consumers. Native cons/append helpers copy spans directly into an exactly sized result buffer; appending a singleton avoids a separate one-element array, and appending an empty list can reuse the immutable view. Readonly collection interfaces retain efficient counted access for fallback BCL consumers.

Exports, ordinary delegates, aggregate fields and FFI keep their array representation. Partial views materialize at those array boundaries; a complete view can reuse its already owned array. Escaping closure captures take an array snapshot when the closure is created, rather than repeatedly materializing on invocation or retaining an unrelated array prefix. Existing array-returning native methods keep their ownership boundary. Some fallback enumerable consumers can still allocate iterator objects.

The view runtime is embedded only for assembly generation and pruned when unused. Source projects retain arrays. Empty `List.tail` raises the documented evaluation error in both compiled targets; a nonempty singleton tail is an empty list. Source reachability follows implicit conversion operators and converted types so all required view/array adapters survive pruning. There is no public view type in the FScript language or IR artifact.


## Exact-size compiled filters

Both compiled targets implement materialized filters with compact 32-bit selection masks and exactly sized output arrays. Known native callbacks remain direct calls, and unknown callbacks use typed runtime helpers. Workers evaluate predicates once in source order, form a mask per word, then copy selected elements through a local readonly span. Small inputs keep the mask in a scalar; empty results reuse the empty array. Predicate failures stop execution immediately.

Private view adapters expose array and offset metadata, with spans constructed locally rather than returned by generated helpers, preserving independent IL verification. The bit operations are backend details: no FScript syntax, public HIR/LIR node, artifact format, or interpreted execution change is required. Pure pipeline fusion can remove materialized filters before emission.
