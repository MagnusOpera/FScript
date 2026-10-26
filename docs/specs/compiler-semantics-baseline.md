# Compiler Semantic Baseline

## Purpose

This document records the behavior that the typed IR, LIR interpreter, and compiled backends must preserve. The current type inference and evaluator are the reference until cross-runtime conformance replaces that dependency.

## Frontend and execution

- Includes are resolved into one flattened program before inference. Alias-qualified declarations are rewritten by the include resolver; unresolved `SImport` nodes are rejected by inference.
- Hindley-Milner inference generalizes top-level and local let bindings after applying substitutions. Recursive bindings are inferred monomorphically inside their recursive group and generalized afterward.
- The active typed program stores top-level types but retains parser expressions in bodies. Compiler lowering therefore requires a new resolved, recursively typed result emitted by the inference pass; types cannot safely be reconstructed afterward. The legacy typed-program API projects this result's `TypedProgram`, so it does not maintain a second inference path.
- Resolved inference exposes parser-independent semantic statement, expression, and pattern trees whose nodes carry their final instantiated type, span, lexical reference kind, resolved primitive operation, and named/structural record-coercion evidence. It also exposes deterministic symbols for every lexical binding category and links each captured reference to its target ID. Repeated occurrences of one pattern binding reuse one ID so equality constraints cannot be lost during lowering. Type-variable numbering in resolved schemes and reference types is normalized independently of prior inference calls.
- Interpolation placeholder expressions retain their absolute file, line, and column positions. Identical placeholder text in different lexical scopes therefore resolves to the parameter or pattern binding visible at its actual source location.
- Top-level declarations resolve sequentially: a non-recursive shadowing initializer refers to the preceding binding, recursive groups see every peer atomically, and subsequent expressions refer to the newest binding.
- HIR reuses those resolved IDs unchanged for source symbols even when type and value declarations are interleaved; compiler-synthesized temporaries and constructor payload parameters occupy IDs above the resolved frontend range.
- Top-level statements execute in source order. Every non-final expression must have type `unit`; the final expression is the program result.
- LIR expression children execute strictly from left to right and stop at the first failure; later arguments, aggregate fields, interpolation parts, map entries, and ordinary operands are not evaluated after that failure. Boolean operators additionally skip an unnecessary right operand.
- Boolean `&&` and `||` evaluate the left operand first and skip the right operand when the result is already known. The legacy evaluator, LIR interpreter, and compiled backends share this short-circuit behavior.
- Pipeline application retains its distinct value-then-function evaluation order through resolved inference, HIR, LIR, interpretation, serialization, and native generation; it is not rewritten into ordinary function-first application.
- Compiled top-level expressions execute exactly once in source order even when interleaved with declarations. Top-level pattern bindings evaluate their scrutinee once, fail before later statements when unmatched, and expose all bound names to subsequent statements.

The first aggregate compiler slice preserves tuple field order, record field names, option presence, list element order, list cons/append behavior, and recursively structural equality through HIR, serialized LIR, interpretation, and native generation.

Ranges preserve inclusive ascending or descending order. List indexing returns an option and never throws for an out-of-range integer. Record updates allocate a new value, interpolation evaluates embedded expressions from left to right, and generated program output uses FScript value formatting rather than host-language default aggregate formatting.

Declared unions retain their type and case identity plus an optional erased payload. Constructors are deterministic compiler declarations, qualified and unqualified case references share one identity, pattern tests check both type and case tags, and equality recursively compares payloads.

Compiled maps use an ordered entry representation for string and integer keys. Explicit key/value entries replace an earlier value; spread entries retain values already present on their left. Indexing returns an option, and deterministic ordering participates in artifacts, equality, formatting, and compiled-program behavior.
- `Map.ofList` and `Map.add` restore canonical key order after insertion or replacement rather than exposing source-list insertion order.
- Functions are curried. Multi-argument declarations are nested lambdas, application is one argument at a time, and LIR closures record only their deterministic free-symbol capture set. Single and mutual recursive function groups are supported.
- Local mutual-recursive function groups initialize atomically: every peer is visible from every closure before the group body executes.

## Types and values

- Primitive runtime values are unit, signed 64-bit integer, IEEE-754 double, Boolean, and string.
- Tuples and lists preserve element order. Options distinguish `None` from `Some value`.
- Maps currently accept string keys through inference and have deterministic ordered-map behavior. Runtime support for integer map keys is legacy representation detail, not permission for newly compiled programs.
- Declared records retain declaration identity for named annotations. Structural record annotations retain field-shape compatibility and may accept additional fields where current inference permits it. Inference records coercion evidence at the accepting application; HIR and LIR retain an explicit conversion node so backends convert representations without changing compatibility rules.
- HIR, LIR, and version-1 artifacts carry ordinary record and union declarations separately from nominal references. Native backends select concrete representations and preserve structural/named conversions, immutable updates, field projection, formatting, and recursive equality.
- HIR, LIR, and version-1 artifacts also carry the intrinsic `Environment` record and `FsKind` union. Structural record conversions include width conversions from a record with extra fields to a required structural or nominal shape, and recursive named representations are validated without unbounded type expansion.
- Discriminated unions retain their declared type name, case name, and optional payload. Recursive types require `type rec`; mutually recursive type-definition graphs are rejected.
- `typeof` produces an FScript type token and `nameof` produces the resolved source name.
- Compiled local pattern bindings evaluate their source once and use the ordinary match-failure path. Compiled `for` evaluates its list once, executes bodies from first to last, discards their values, and returns unit. Compiled `typeof` retains the resolved declared record or union shape in the artifact instead of performing a name lookup at execution time.

## Expressions, patterns, and equality

- Conditionals and match guards require Boolean values and evaluate only the selected branch.
- Match clauses are tried in source order. A guard runs only after its pattern binds successfully. Failure to find a clause is a runtime error.
- HIR patterns lower into LIR projection/check/binding/map-consumption plans. LIR consumers execute that plan vocabulary and never receive source-shaped pattern nodes.
- Patterns cover literals, variables, wildcards, lists, tuples, records, ordered maps, options, union cases, and type-reference tests. Let-pattern failure is a runtime error.
- Compiled map patterns consume entries deterministically in map order, merge repeated bindings only when structurally equal, and bind an optional tail to the remaining ordered map. Compiled type-reference patterns use resolved types rather than source text lookup.
- Record updates evaluate the base once and then updates in source order. Ranges, loops, interpolation, indexing, raise, append, and cons preserve the evaluator's existing evaluation order and failures.
- Equality is structural for primitives, tuples, records, maps, options, lists, unions, and type tokens, recursively. Functions, tasks, constructors, and extern handles are not structurally equal. Backend host-language comparability must not leak into this behavior.
- Integer and floating-point comparisons share the same resolved numeric operations and are executable in both compiled runtimes. Whole-program specialization gives native operands concrete numeric types. Pure collection functions retain their failure behavior, including an explicit runtime error for `List.tail []`.
- Cross-runtime failure conformance classifies explicit `raise`, exhausted pattern matches, and integer division by zero independently of each host runtime's exception or panic text.
- Cross-runtime tests compare the legacy evaluator with artifact-round-tripped LIR execution and compiled backends for successful values and failure categories. Named records passed through typed function boundaries keep record field projection semantics; their nominal annotation must not select a union runtime representation.
- Cross-runtime result formatting quotes strings at every nesting level, renders unit as `()`, renders ordered maps as `map { key => value }`, uses compact semicolon separators for lists, and retains comma-space separators for tuples.
- HIR and LIR are accepted only when artifact-local references resolve, source spans are valid, declaration and expression types agree, calls and control-flow nodes have consistent signatures, aggregate representations match their semantic types, and referenced primitives/imports are available and complete.
- Task signatures and host linkage remain representable in compiled artifacts. An unused task function handle is inert. Fully applying `Task.spawn` or `Task.await` in the LIR interpreter reports `FSI3021`.
- Compiler extern input is declarative: it carries a name, type scheme, arity, and linkage classification, never an executable `.NET` delegate. Host execution values remain confined to the legacy/runtime API.
- `Console.writeLine` evaluates its string argument once, appends one stdout line, and returns unit. LIR reports captured lines in order through `ExecutionResult.Stdout`; native backends emit them in the same order before the final formatted result.
- `Console.readLine ()` consumes one line from process stdin, strips the line terminator, and returns `Some text`. It returns `None` only when stdin is exhausted before any text is read; a final unterminated line is still returned.
- `Fs.readText path` reads the complete text file and returns `Some text`; missing, inaccessible, or otherwise unreadable paths return `None`. This read-only operation is portable so generated command-line programs can load their inputs.
- Tuples and lists remain observably distinct in compiled backends: tuples format with parentheses, lists with brackets, tuple patterns require tuple values, and map pair inputs consume tuple values.

## Builtins, externs, attributes, and tasks

- Scalar conversions, string helpers, `List`, `Option`, and `Map` functions are protected builtin symbols with explicit polymorphic schemes. They are initial compiler/runtime intrinsics even where a future implementation can compile their FScript source.
- The LIR interpreter and native backends execute the complete pure builtin catalog, including higher-order list, option, and ordered-map functions. Callback invocation remains curried and follows source collection order.
- Compiled `Env` is a runtime-context intrinsic. The LIR interpreter uses its explicit script name and arguments; native backends use the executable name and process arguments.
- `Task.spawn` and `Task.await` are host externs with tracked-task semantics, background error propagation, and an error for unawaited tasks. HIR may represent them before LIR interpreters or backends support them.
- `export let` marks declarations for embedding hosts and does not change inference. Backend `Import` metadata is separate and will not weaken the declared FScript type.
- The `export let` prefix produces structured `Export` metadata in resolved statements and HIR; this is internal metadata, not source attribute syntax. `Import` attributes retain their two arguments and attribute span on ordinary HIR function/type declarations in addition to dedicated backend linkage.
- Other top-level declaration attributes remain case-sensitive and flow unchanged through resolved statements and HIR; they carry string or identifier arguments but have no evaluator semantics unless a future consumer recognizes them.

The compiler-owned runtime catalog is declarative and contains no executable host delegates:

| Classification | Current bindings |
| --- | --- |
| Compiler context intrinsic | `Env` |
| Portable runtime intrinsic | `Console.writeLine`, `Console.readLine`, `Fs.readText` |
| Host filesystem | `Fs.exists`, `Fs.kind`, `Fs.createDirectory`, `Fs.writeText`, `Fs.glob`, `Fs.enumerateFiles`, `Fs.combinePath`, `Fs.parentDirectory`, `Fs.extension`, `Fs.fileNameWithoutExtension` |
| Host regex | `Regex.matchGroups` |
| Pure host candidates | `Hash.md5` |
| Host-only/deferred data services | `Guid.new`, `Json.deserialize`, `Json.serialize`, `Xml.queryValues` |
| Task runtime | `Task.spawn`, `Task.await` |

All entries are type-checkable and retained as linkage in HIR/LIR. The LIR interpreter supplies the compiler context and portable intrinsics; other linkage requires a registered callback or produces an unsupported diagnostic. Native runtime support is specified in [the .NET backend specification](./dotnet-backends.md). Unused linkage remains inert.

## Diagnostics

Parse, type, and evaluation failures currently use source spans and exceptions. Compiler entry points translate expected failures into structured stage diagnostics while leaving existing embedding APIs behavior-compatible.

Version-1 `.fsir` artifacts require compiler and language versions, source metadata, ordered dependencies, and a complete program. Unknown formats and versions are rejected before LIR construction; malformed metadata receives `FSA5xxx`, while reconstructed programs containing unknown named types, unresolved symbols, or invalid nested spans are rejected by the shared `FSL2xxx` validator before execution or generation.

## Closure specialization

When specialization replaces a captured local callback with its implementation, the specialized closure must capture every free symbol introduced by that replacement. Nested inlining must rename those symbols with their enclosing lexical bindings so separate invocations retain separate environments.
