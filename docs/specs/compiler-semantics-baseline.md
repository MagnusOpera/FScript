# Compiler Semantic Baseline

## Purpose

This document records the behavior that the typed IR, LIR interpreter, and compiled backends must preserve. The current type inference and evaluator are the reference until cross-runtime conformance replaces that dependency.

## Frontend and execution

- Includes are resolved into one flattened program before inference. Alias-qualified declarations are rewritten by the include resolver; unresolved `SImport` nodes are rejected by inference.
- Hindley-Milner inference generalizes top-level and local let bindings after applying substitutions. Recursive bindings are inferred monomorphically inside their recursive group and generalized afterward.
- The active typed program stores top-level types but retains parser expressions in bodies. Compiler lowering therefore requires a new resolved, recursively typed result emitted by the inference pass; types cannot safely be reconstructed afterward.
- Top-level statements execute in source order. Every non-final expression must have type `unit`; the final expression is the program result.

The first aggregate compiler slice preserves tuple field order, record field names, option presence, list element order, list cons/append behavior, and recursively structural equality through HIR, serialized LIR, interpretation, and Go generation.
- Functions are curried. Multi-argument declarations are nested lambdas, application is one argument at a time, and closures capture their lexical environment. Single and mutual recursive function groups are supported.

## Types and values

- Primitive runtime values are unit, signed 64-bit integer, IEEE-754 double, Boolean, and string.
- Tuples and lists preserve element order. Options distinguish `None` from `Some value`.
- Maps currently accept string keys through inference and have deterministic ordered-map behavior. Runtime support for integer map keys is legacy representation detail, not permission for newly compiled programs.
- Declared records retain declaration identity for named annotations. Structural record annotations retain field-shape compatibility and may accept additional fields where current inference permits it. A backend must insert representation conversions rather than change compatibility rules.
- Discriminated unions retain their declared type name, case name, and optional payload. Recursive types require `type rec`; mutually recursive type-definition graphs are rejected.
- `typeof` produces an FScript type token and `nameof` produces the resolved source name.

## Expressions, patterns, and equality

- Conditionals and match guards require Boolean values and evaluate only the selected branch.
- Match clauses are tried in source order. A guard runs only after its pattern binds successfully. Failure to find a clause is a runtime error.
- Patterns cover literals, variables, wildcards, lists, tuples, records, ordered maps, options, union cases, and type-reference tests. Let-pattern failure is a runtime error.
- Record updates evaluate the base once and then updates in source order. Ranges, loops, interpolation, indexing, raise, append, and cons preserve the evaluator's existing evaluation order and failures.
- Equality is structural for primitives, tuples, records, maps, options, lists, unions, and type tokens, recursively. Functions, tasks, constructors, and extern handles are not structurally equal. Backend host-language comparability must not leak into this behavior.

## Builtins, externs, attributes, and tasks

- Scalar conversions, string helpers, `List`, `Option`, and `Map` functions are protected builtin symbols with explicit polymorphic schemes. They are initial compiler/runtime intrinsics even where a future implementation can compile their FScript source.
- `Console.writeLine`, `Console.readLine`, and the CLI `Env` value are candidate portable runtime primitives. Filesystem, regex, hashing, GUID, JSON, and XML functions remain host externs initially.
- `Task.spawn` and `Task.await` are host externs with tracked-task semantics, background error propagation, and an error for unawaited tasks. HIR may represent them before LIR interpreters or backends support them.
- `[<Export>]` is declaration metadata used by embedding hosts and does not change inference. Backend `Import` metadata is separate and will not weaken the declared FScript type.

## Diagnostics

Parse, type, and evaluation failures currently use source spans and exceptions. Compiler entry points translate expected failures into structured stage diagnostics while leaving existing embedding APIs behavior-compatible.
