# Typed IR and Backend Architecture

## Pipeline

```text
source -> parser/includes -> inference + resolved typed tree -> HIR -> LIR
                                                               |      |
                                                               |      +-> LIR interpreter
                                                               +--------> serialization
                                                                      +-> Go source backend
```

`FScript.Language` remains the parser, type-system, legacy evaluator, and Fable-compatible package. It emits a resolved typed tree in the same inference pass as its compatibility `TypedProgram`.

Resolved inference assigns deterministic program-local IDs to builtins, externs, types, union cases, declarations, parameters, pattern bindings, and locals in normalized source traversal. Declaration schemes and instantiated reference types normalize type variables from zero, so results do not depend on the process-global fresh-variable counter. The legacy `TypedProgram` remains a projection with unchanged public entry points.

`FScript.Compiler` is a reusable, initially non-packaged .NET assembly. It owns HIR, HIR validation, lowering, LIR, LIR validation, and artifact serialization. It depends on `FScript.Language`; the language assembly never depends on the compiler.

Validated LIR consumers are separate leaf assemblies. `FScript.Interpreter.Lir` owns execution and runtime LIR values. `FScript.Backend.Go` owns representation selection, Go emission, and embedded runtime templates. Both reference `FScript.Compiler`, while the compiler references neither consumer. Public modules retain the `FScript.Compiler` namespace so the API surface remains source-compatible while assembly ownership is explicit.

## IR boundaries

HIR is language-oriented and contains resolved artifact-local symbol identities, complete semantic types and schemes, source locations, declaration attributes, and explicit evidence for named/structural record conversions. It contains no Go types.

LIR is executable but not a Go AST. It uses lifted functions, explicit captures and closure creation, resolved primitive operations, aggregate construction/projection, and structured control flow. Pattern matching becomes deterministic tests and projections before reaching a backend. Semantic types may remain alongside selected runtime representations for validation and diagnostics.

HIR validation rejects duplicate and unresolved symbols, invalid declaration spans, and schemes inconsistent with their values. LIR validation rechecks artifact-local references and spans, then verifies closure calls, branches and decision guards, primitive signatures, aggregate representations, declaration value types, intrinsic catalog membership, and complete target-import linkage. Artifact deserialization runs the same LIR validator, so malformed JSON cannot bypass compiler invariants.

Lexical `let` is retained as a structured LIR node. Its symbol is visible only in the body, or in both the binding and body for recursive functions. The interpreter closes the recursive environment explicitly; Go emission declares the function slot before assigning its closure. `raise` becomes an explicit `Fail` node so consumers preserve evaluation order and attach their own stage-specific failure diagnostic.

Matches lower to a structured decision tree whose target is evaluated once. Cases remain ordered; each pattern test creates artifact-local bindings, then its guard is evaluated, and only then is its body selected. A failed guard continues at the next case. Exhaustion is the explicit FScript match-failure path. Literal, list, tuple, record, and option patterns use the same tree in serialized artifacts, the LIR interpreter, and generated Go.

Ordered-map patterns retain ordered clause matching, repeated-binding consistency, remainder binding, and the existing rule that explicit clauses may leave unmatched entries. Type-reference patterns carry their resolved semantic type; consumers test primitive and aggregate representation shape and retain declared union identity.

Top-level mutual-recursive declarations are initialized as an atomic closure group. The interpreter creates every closure against the preceding environment, extends that environment with the complete group, and updates all captures together. Generated Go declares typed slots for all declarations and assigns them in source order from `init`, allowing closures in one recursive group to refer to every peer.

Local `let rec ... and ...` uses the same atomic rule inside a structured expression node. All local names are allocated before any member is lowered, every binding must evaluate to a closure, captures are updated with the complete group, and the body runs only after group initialization.

Language builtins are explicit `Intrinsic` declarations rather than magic unresolved references. Consumers accumulate their curried arguments according to the catalog signature and dispatch only at full application. The initial portable implementations cover scalar parsing/formatting, strings, simple list and option operations, and map queries; a reachable catalog entry without an implementation receives a stage-specific unsupported diagnostic or runtime failure.

The LIR interpreter implements the pure builtin catalog, including callbacks for list mapping, selection, collection, folding, filtering and search; lazy option fallback and mapping; and ordered-map construction, folds, transforms, iteration, and removal. Callback application goes through the same curried closure path as ordinary source calls, preserving failure propagation and evaluation order.

Generated Go represents every FScript function as an erased `fsruntime.Closure` with a single `any -> any` call slot. Typed values are checked at closure entry and result use. This uniform convention permits ordinary source closures, partial application, recursion, and higher-order runtime intrinsics to interoperate without Go generics or reflection-based invocation.

Host externs are copied into IR as name, signature, and arity only; their executable .NET delegates never cross the compiler boundary. Constructing or retaining an unused extern handle is inert. Fully applying one in the LIR interpreter produces `FSI3021`, preserving the rule that unreachable unsupported linkage does not block execution.

`Task.spawn` and `Task.await` are compiler-known host-linkage descriptors with their polymorphic task signatures. This lets task-typed declarations survive inference, HIR, LIR, and artifact serialization. The LIR interpreter reports `FSI3021` only when either descriptor is fully applied; the Go backend performs deterministic reachability over declaration references and reports `FSB4004` when a reachable declaration requires a task representation.

Local binding patterns reuse the same single-scrutinee decision-tree representation as `match`. Sequential `for ... in ... do ...` is explicit in HIR and LIR so each consumer evaluates the input list once, visits values in order, and discards each body result before returning unit. `typeof` resolves declared record and union definitions during HIR construction and carries the semantic type as a serializable type-token node.

Every top-level source expression becomes a synthetic declaration at its exact position in the declaration stream; the program entry references the final such value. This preserves eager source ordering without evaluating the result expression twice. Top-level pattern bindings similarly store their scrutinee in one synthetic declaration, validate the pattern, and publish deterministic projections for each bound name.

Serialized `.fsir` files contain one closed resolved program plus explicit foreign linkage declarations. JSON format versions are internal and may be rejected rather than upgraded.

Artifact metadata records the actual compiler and language assembly versions, normalized paths and SHA-256 hashes for available file-backed sources, and a sorted unique list of foreign dependencies. Deserialization rejects missing metadata, malformed hashes, non-canonical dependency order, unknown formats or versions, and any LIR that fails post-load validation.

## Representation policy

- Primitive values, declared records, tuple shapes, and declared unions receive concrete representations where possible.
- Polymorphic lists, options, maps, and the universal curried closure calling convention may carry erased payloads.
- Erasure is introduced during HIR-to-LIR lowering, never during frontend inference.
- Structural equality and record conversions are explicit LIR operations/helpers; Go `==` and Go nominal assignability never define FScript semantics.
- LIR remains structured and non-SSA until a concrete optimization requirement justifies a control-flow IR.

## Runtime and foreign calls

Compiler bindings classify names as portable intrinsics, backend runtime primitives, target imports, or unsupported host externs. This metadata is distinct from executable .NET `ExternalFunction` implementations.

`CompileInput.Externs` contains `CompilerExtern` descriptors only: name, normalized scheme, arity, and `CompilerExternKind`. Compilation creates private no-op inference shims because the language inference API still consumes `ExternalFunction`, but no executable delegate enters HIR, LIR, artifacts, the interpreter assembly, or a backend assembly.

`Console.writeLine` is a portable runtime intrinsic. LIR execution captures each line in `ExecutionResult.Stdout` without depending on the process console; generated Go dispatches the same intrinsic to stdout before printing the program result. The cross-runtime harness separates those lines from the final value and compares both channels against the legacy evaluator.

`Env` is a compiler runtime-context primitive, not a serialized host delegate. LIR interpretation materializes its named record from `InterpreterContext`; generated Go materializes the same fields from `os.Args`, including the executable base name and arguments after it.

The LIR interpreter rejects reachable target imports it cannot supply. The Go backend initially rejects unsupported host externs and tasks with backend diagnostics. Imports describe complete ordinary FScript declarations plus target linkage and do not create a second foreign type system.

Initial Go imports use `[<Import("module/path", "Member")>] let name : complete -> signature` with no body. The frontend requires a function type and records linkage separately from its FScript signature. Go emission uses deterministic package aliases and currently accepts only primitive/unit arguments and results; other representations receive `FSB4003`. The LIR interpreter keeps unused imports inert and reports `FSI3023` only when one is fully applied.

## Cross-runtime conformance

The conformance suite evaluates one source program through the legacy evaluator, a serialized-and-deserialized LIR program, and a generated Go executable. It compares successful results and failure categories across nested structural equality, named/structural record conversion, unions, higher-order and polymorphic functions, mutual recursion, and ordered maps. Named semantic types are resolved to their record or union representation while HIR is built, so a named record crossing a function boundary cannot accidentally receive the union representation in a downstream consumer.

## Previous experiment

The `feature/in-memory-execution` branch is useful only as evidence about closures, invocation, and host integration. Its dynamic-method/in-memory assembly backend is not an IR layer and is neither copied nor referenced by the new compiler architecture.
