# Compiler Semantic Baseline

## Purpose

This document records the behavior that the typed IR, LIR interpreter, and compiled backends must preserve. The current type inference and evaluator are the reference until cross-runtime conformance replaces that dependency.

## Frontend and execution

- Includes are resolved into one flattened program before inference. Alias-qualified declarations are rewritten by the include resolver; unresolved `SImport` nodes are rejected by inference.
- Hindley-Milner inference generalizes top-level and local let bindings after applying substitutions. Recursive bindings are inferred monomorphically inside their recursive group and generalized afterward.
- The active typed program stores top-level types but retains parser expressions in bodies. Compiler lowering therefore requires a new resolved, recursively typed result emitted by the inference pass; types cannot safely be reconstructed afterward. The legacy typed-program API projects this result's `TypedProgram`, so it does not maintain a second inference path.
- Resolved inference exposes parser-independent semantic statement, expression, and pattern trees whose nodes carry their final instantiated type, span, lexical reference kind, resolved primitive operation, and named/structural record-coercion evidence. It also exposes deterministic symbols for every lexical binding category and links each captured reference to its target ID. Repeated occurrences of one pattern binding reuse one ID so equality constraints cannot be lost during lowering. Type-variable numbering in resolved schemes and reference types is normalized independently of prior inference calls.
- HIR reuses those resolved IDs unchanged for source symbols even when type and value declarations are interleaved; compiler-synthesized temporaries and constructor payload parameters occupy IDs above the resolved frontend range.
- Top-level statements execute in source order. Every non-final expression must have type `unit`; the final expression is the program result.
- LIR expression children execute strictly from left to right and stop at the first failure; later arguments, aggregate fields, interpolation parts, map entries, and operands are not evaluated after that failure.
- Pipeline application retains its distinct value-then-function evaluation order through resolved inference, HIR, LIR, interpretation, serialization, and Go generation; it is not rewritten into ordinary function-first application.
- Compiled top-level expressions execute exactly once in source order even when interleaved with declarations. Top-level pattern bindings evaluate their scrutinee once, fail before later statements when unmatched, and expose all bound names to subsequent statements.

The first aggregate compiler slice preserves tuple field order, record field names, option presence, list element order, list cons/append behavior, and recursively structural equality through HIR, serialized LIR, interpretation, and Go generation.

Ranges preserve inclusive ascending or descending order. List indexing returns an option and never throws for an out-of-range integer. Record updates allocate a new value, interpolation evaluates embedded expressions from left to right, and generated program output uses FScript value formatting rather than Go's default aggregate formatting.

Declared unions retain their type and case identity plus an optional erased payload. Constructors are deterministic compiler declarations, qualified and unqualified case references share one identity, pattern tests check both type and case tags, and equality recursively compares payloads.

Compiled maps use an ordered entry representation for string and integer keys. Explicit key/value entries replace an earlier value; spread entries retain values already present on their left. Indexing returns an option, and deterministic ordering participates in artifacts, equality, formatting, and generated Go behavior.
- `Map.ofList` and `Map.add` restore canonical key order after insertion or replacement rather than exposing source-list insertion order.
- Functions are curried. Multi-argument declarations are nested lambdas, application is one argument at a time, and LIR closures record only their deterministic free-symbol capture set. Single and mutual recursive function groups are supported.
- Local mutual-recursive function groups initialize atomically: every peer is visible from every closure before the group body executes.

## Types and values

- Primitive runtime values are unit, signed 64-bit integer, IEEE-754 double, Boolean, and string.
- Tuples and lists preserve element order. Options distinguish `None` from `Some value`.
- Maps currently accept string keys through inference and have deterministic ordered-map behavior. Runtime support for integer map keys is legacy representation detail, not permission for newly compiled programs.
- Declared records retain declaration identity for named annotations. Structural record annotations retain field-shape compatibility and may accept additional fields where current inference permits it. Inference records coercion evidence at the accepting application; HIR and LIR retain an explicit conversion node so backends convert representations without changing compatibility rules.
- HIR, LIR, and version-1 artifacts carry ordinary record and union declarations separately from nominal references. Generated Go uses concrete structs for ordinary named records, copies record updates, and performs explicit structural/named conversions; projection, patterns, formatting, and recursive equality remain shape-compatible.
- Generated Go represents each ordinary declared union as its own tagged struct with case-specific payload fields. Runtime union matching, formatting, and recursive equality consume a narrow conversion interface, preserving FScript behavior without erasing declarations to one storage type.
- Discriminated unions retain their declared type name, case name, and optional payload. Recursive types require `type rec`; mutually recursive type-definition graphs are rejected.
- `typeof` produces an FScript type token and `nameof` produces the resolved source name.
- Compiled local pattern bindings evaluate their source once and use the ordinary match-failure path. Compiled `for` evaluates its list once, executes bodies from first to last, discards their values, and returns unit. Compiled `typeof` retains the resolved declared record or union shape in the artifact instead of performing a name lookup at execution time.

## Expressions, patterns, and equality

- Conditionals and match guards require Boolean values and evaluate only the selected branch.
- Match clauses are tried in source order. A guard runs only after its pattern binds successfully. Failure to find a clause is a runtime error.
- HIR patterns lower into LIR projection/check/binding/map-consumption plans. LIR consumers execute that plan vocabulary and never receive source-shaped pattern nodes; generated Go evaluates each serialized plan through one runtime matcher.
- Patterns cover literals, variables, wildcards, lists, tuples, records, ordered maps, options, union cases, and type-reference tests. Let-pattern failure is a runtime error.
- Compiled map patterns consume entries deterministically in map order, merge repeated bindings only when structurally equal, and bind an optional tail to the remaining ordered map. Compiled type-reference patterns use resolved types rather than source text lookup.
- Record updates evaluate the base once and then updates in source order. Ranges, loops, interpolation, indexing, raise, append, and cons preserve the evaluator's existing evaluation order and failures.
- Equality is structural for primitives, tuples, records, maps, options, lists, unions, and type tokens, recursively. Functions, tasks, constructors, and extern handles are not structurally equal. Backend host-language comparability must not leak into this behavior.
- Cross-runtime tests compare the legacy evaluator with artifact-round-tripped LIR execution and generated Go for successful values and failure categories. Named records passed through typed function boundaries keep record field projection semantics; their nominal annotation must not select a union runtime representation.
- Cross-runtime result formatting quotes strings at every nesting level, renders unit as `()`, renders ordered maps as `map { key => value }`, uses compact semicolon separators for lists, and retains comma-space separators for tuples.
- HIR and LIR are accepted only when artifact-local references resolve, source spans are valid, declaration and expression types agree, calls and control-flow nodes have consistent signatures, aggregate representations match their semantic types, and referenced primitives/imports are available and complete.
- Task signatures and host linkage remain representable in compiled artifacts. An unused task function handle is inert; fully applying `Task.spawn` or `Task.await` in LIR reports `FSI3021`, while a Go program whose reachable declaration graph requires a task reports `FSB4004`.
- Compiler extern input is declarative: it carries a name, type scheme, arity, and linkage classification, never an executable `.NET` delegate. Host execution values remain confined to the legacy/runtime API.
- Generated Go rejects a reachable arbitrary host extern with `FSB4006`; an unused host-extern descriptor remains inert and does not block generation.
- `Console.writeLine` evaluates its string argument once, appends one stdout line, and returns unit. LIR reports captured lines in order through `ExecutionResult.Stdout`; generated Go emits them in the same order before its final formatted result.
- Generated Go source is deterministic and already canonical according to `gofmt`; generation itself has no installed-Go dependency. Local integration tests skip only when Go is unavailable, while CI installs Go 1.25 and treats its absence as a failure.
- Tuples and lists remain observably distinct in generated Go: tuples format with parentheses, lists with brackets, tuple patterns require tuple values, and map pair inputs consume tuple values.
- Backend-imported records retain exact field names and declaration linkage. The initial Go adapter accepts exported primitive fields only, performs record/struct conversion solely at target-call boundaries, and rejects imported unions, empty schemas, lowercase fields, tasks, and aggregate/function fields with `FSB4005`.

## Builtins, externs, attributes, and tasks

- Scalar conversions, string helpers, `List`, `Option`, and `Map` functions are protected builtin symbols with explicit polymorphic schemes. They are initial compiler/runtime intrinsics even where a future implementation can compile their FScript source.
- The LIR interpreter and Go runtime execute the complete pure builtin catalog, including higher-order list, option, and ordered-map functions. Callback invocation remains curried and follows source collection order.
- Generated Go implements FScript string indexing and substring ranges in UTF-16 code units, matching the reference evaluator and Fable string model rather than Go byte offsets. Boolean parsing accepts only case-insensitive `true` and `false`, matching the reference runtime.
- `Console.writeLine`, `Console.readLine`, and the CLI `Env` value are candidate portable runtime primitives. Filesystem, regex, hashing, GUID, JSON, and XML functions remain host externs initially.
- Compiled `Env` is a runtime-context intrinsic. The LIR interpreter uses its explicit script name and arguments; generated Go uses the executable base name and `os.Args[1:]`.
- `Task.spawn` and `Task.await` are host externs with tracked-task semantics, background error propagation, and an error for unawaited tasks. HIR may represent them before LIR interpreters or backends support them.
- `[<Export>]` is declaration metadata used by embedding hosts and does not change inference. Backend `Import` metadata is separate and will not weaken the declared FScript type.
- Both `[<export>]` and `[<Export>]` normalize to a structured `Export` attribute in resolved statements and HIR. `Import` attributes retain their two arguments and attribute span on ordinary HIR function/type declarations in addition to dedicated backend linkage.
- Other top-level declaration attributes remain case-sensitive and flow unchanged through resolved statements and HIR; they carry string or identifier arguments but have no evaluator semantics unless a future consumer recognizes them.

The compiler-owned runtime catalog is declarative and contains no executable host delegates:

| Classification | Current bindings |
| --- | --- |
| Compiler context intrinsic | `Env` |
| Portable runtime intrinsic | `Console.writeLine` |
| Deferred host input | `Console.readLine` |
| Host filesystem | `Fs.readText`, `Fs.exists`, `Fs.kind`, `Fs.createDirectory`, `Fs.writeText`, `Fs.glob`, `Fs.enumerateFiles` |
| Pure host candidates | `Fs.combinePath`, `Fs.parentDirectory`, `Fs.extension`, `Fs.fileNameWithoutExtension`, `Regex.matchGroups`, `Hash.md5` |
| Host-only/deferred data services | `Guid.new`, `Json.deserialize`, `Json.serialize`, `Xml.queryValues` |
| Deferred task runtime | `Task.spawn`, `Task.await` |

All entries are type-checkable and retained as linkage in HIR/LIR. Except for the compiler context and portable intrinsic rows, reaching one produces an interpreter or backend unsupported diagnostic; unused linkage remains inert.

## Diagnostics

Parse, type, and evaluation failures currently use source spans and exceptions. Compiler entry points translate expected failures into structured stage diagnostics while leaving existing embedding APIs behavior-compatible.

Version-1 `.fsir` artifacts require compiler and language versions, source metadata, ordered dependencies, and a complete program. Unknown formats and versions are rejected before LIR construction; malformed metadata and invalid reconstructed LIR receive `FSA5xxx` diagnostics.
