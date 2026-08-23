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

`FScript.Compiler` is a reusable, initially non-packaged .NET assembly. It owns HIR, HIR validation, lowering, LIR, LIR validation, artifact serialization, LIR interpretation, and target backends. It depends on `FScript.Language`; the language assembly never depends on the compiler.

## IR boundaries

HIR is language-oriented and contains resolved artifact-local symbol identities, complete semantic types and schemes, source locations, declaration attributes, and explicit evidence for named/structural record conversions. It contains no Go types.

LIR is executable but not a Go AST. It uses lifted functions, explicit captures and closure creation, resolved primitive operations, aggregate construction/projection, and structured control flow. Pattern matching becomes deterministic tests and projections before reaching a backend. Semantic types may remain alongside selected runtime representations for validation and diagnostics.

Lexical `let` is retained as a structured LIR node. Its symbol is visible only in the body, or in both the binding and body for recursive functions. The interpreter closes the recursive environment explicitly; Go emission declares the function slot before assigning its closure. `raise` becomes an explicit `Fail` node so consumers preserve evaluation order and attach their own stage-specific failure diagnostic.

Matches lower to a structured decision tree whose target is evaluated once. Cases remain ordered; each pattern test creates artifact-local bindings, then its guard is evaluated, and only then is its body selected. A failed guard continues at the next case. Exhaustion is the explicit FScript match-failure path. Literal, list, tuple, record, and option patterns use the same tree in serialized artifacts, the LIR interpreter, and generated Go.

Top-level mutual-recursive declarations are initialized as an atomic closure group. The interpreter creates every closure against the preceding environment, extends that environment with the complete group, and updates all captures together. Generated Go declares typed slots for all declarations and assigns them in source order from `init`, allowing closures in one recursive group to refer to every peer.

Language builtins are explicit `Intrinsic` declarations rather than magic unresolved references. Consumers accumulate their curried arguments according to the catalog signature and dispatch only at full application. The initial portable implementations cover scalar parsing/formatting, strings, simple list and option operations, and map queries; a reachable catalog entry without an implementation receives a stage-specific unsupported diagnostic or runtime failure.

Serialized `.fsir` files contain one closed resolved program plus explicit foreign linkage declarations. JSON format versions are internal and may be rejected rather than upgraded.

## Representation policy

- Primitive values, declared records, tuple shapes, and declared unions receive concrete representations where possible.
- Polymorphic lists, options, maps, and the universal curried closure calling convention may carry erased payloads.
- Erasure is introduced during HIR-to-LIR lowering, never during frontend inference.
- Structural equality and record conversions are explicit LIR operations/helpers; Go `==` and Go nominal assignability never define FScript semantics.
- LIR remains structured and non-SSA until a concrete optimization requirement justifies a control-flow IR.

## Runtime and foreign calls

Compiler bindings classify names as portable intrinsics, backend runtime primitives, target imports, or unsupported host externs. This metadata is distinct from executable .NET `ExternalFunction` implementations.

The LIR interpreter rejects reachable target imports it cannot supply. The Go backend initially rejects unsupported host externs and tasks with backend diagnostics. Imports describe complete ordinary FScript declarations plus target linkage and do not create a second foreign type system.

## Previous experiment

The `feature/in-memory-execution` branch is useful only as evidence about closures, invocation, and host integration. Its dynamic-method/in-memory assembly backend is not an IR layer and is neither copied nor referenced by the new compiler architecture.
