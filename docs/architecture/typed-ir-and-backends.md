# Typed IR and Backend Architecture

## Pipeline

```text
source -> parser/includes -> inference + resolved typed tree -> HIR -> LIR
                                                               |      |
                                                               |      +-> LIR interpreter
                                                               +--------> serialization
                                                                      +-> Go source backend
```

`FScript.Language` remains the parser, type-system, legacy evaluator, and Fable-compatible package. It emits a recursive semantic resolved tree in the same inference pass as its compatibility `TypedProgram`. Resolved statement, expression, and pattern nodes do not contain parser AST nodes; every resolved expression carries its final instantiated type and span plus lexical-reference kind, primitive operation, and structural/named-record conversion evidence. Compatibility inference and evaluation APIs continue to project their existing results; `inferProgramWithExterns` specifically returns the `TypedProgram` from `inferResolvedProgramWithExterns` rather than running a parallel inference path.

Resolved inference assigns deterministic program-local IDs to builtins, externs, types, union cases, declarations, parameters, pattern bindings, and locals in normalized source traversal. Declaration schemes and instantiated reference types normalize type variables from zero, so results do not depend on the process-global fresh-variable counter. The legacy `TypedProgram` remains a projection with unchanged public entry points.

Top-level reference resolution walks declarations in source order. A non-recursive initializer sees the preceding environment, a recursive declaration or group sees its own complete group, and a later shadowing declaration replaces the name only after its initializer is resolved. Reference occurrences are consumed in traversal order as well as matched by name and span, so repeated included source locations cannot collapse distinct lexical identities.

`FScript.Compiler` is a reusable, initially non-packaged .NET assembly. It owns HIR, HIR validation, lowering, LIR, LIR validation, and artifact serialization. It depends on `FScript.Language`; the language assembly never depends on the compiler.

Validated LIR consumers are separate leaf assemblies. `FScript.Interpreter.Lir` owns execution and runtime LIR values. `FScript.Backend.Go` owns representation selection, Go emission, and embedded runtime templates. Both reference `FScript.Compiler`, while the compiler references neither consumer. Public modules retain the `FScript.Compiler` namespace so the API surface remains source-compatible while assembly ownership is explicit.

## IR boundaries

HIR is language-oriented and contains resolved artifact-local symbol identities, ordinary and imported type declarations, complete semantic types and schemes, source locations, structured export metadata, and explicit nodes for named/structural record conversions. HIR lowering consumes only the semantic resolved statements, expressions, and patterns; it does not traverse `Expr`, `Pattern`, `TypedStmt`, or compatibility expression telemetry. HIR directly reuses the frontend IDs for builtins, externs, types, constructors, declarations, parameters, pattern bindings, and locals; only compiler-synthesized nodes allocate IDs above the frontend range. Named record and union references remain nominal in HIR and LIR; their declaration shapes are carried separately and serialized in artifacts. Export metadata retains the exported source name and span through LIR and artifacts while the legacy Boolean remains available to compatibility consumers. Conversion evidence is produced by inference, not rediscovered from backend representations. HIR contains no Go types.

LIR is executable but not a Go AST. Lambda bodies are lifted into a deterministic program-level function table. Closure creation references a function ID and carries its canonical ordered set of captured symbol IDs, alongside resolved primitive operations, aggregate construction/projection, and structured control flow. Validation recomputes each lifted function's free-symbol set and rejects unknown function IDs or forged, missing, duplicate, or unordered captures. Pattern matching becomes an ordered decision tree whose cases contain lowered projection steps, representation checks, symbol-binding plans, and deterministic map-consumption plans; no HIR/source pattern node survives in LIR. Semantic types remain alongside selected runtime representations for validation and diagnostics.

The Go backend specializes every reachable HM instantiation before selecting representations. Named and structural records, declared unions, tuples, options, lists, maps, and functions therefore receive closed concrete Go types. Explicit LIR conversion nodes copy record fields while evaluating their source once. Imported record adapters remain isolated at target-call boundaries.

Pattern plans are emitted as concrete checks and projections over those representations. Ordered-map patterns copy a typed remainder and use deterministic recursive backtracking, so the scrutinee is evaluated once and no generic binding dictionary or runtime pattern dispatcher is needed.

Reachable builtins are emitted at their specialized signatures. List, option, and ordered-map loops call native typed function values, while structural equality and display formatting are generated recursively for each reachable closed representation.

The pass pipeline is HIR lowering, HIR validation, LIR lowering, `LirOptimization.optimize`, then LIR validation. The optimization pass is intentionally identity-only in version 1; its explicit boundary prevents later transformations from being hidden inside lowering or a backend.

HIR and LIR expose canonical debug printers with stable artifact-local IDs and normalized type rendering. The arithmetic vertical slice is checked in as paired snapshots so changes to symbol allocation, function lifting, captures, or operation selection are reviewed explicitly.

HIR declaration schemes are taken directly from the resolved frontend after deterministic type-variable normalization; lowering does not generalize source declarations again. Synthetic declarations may derive a local scheme because they have no frontend declaration counterpart.

Compiler artifacts are closed over the language's intrinsic `Environment` record and `FsKind` union definitions. Those types and their cases receive deterministic frontend symbol IDs just like source declarations; an intrinsic constructor value is emitted only when referenced. LIR compatibility and pattern validation resolve named types recursively with cycle guards, including concrete payload types selected by a preceding union-case check.

Resolved statements expose normalized structured attributes as name, argument list, and source span. The parser retains unknown attributes instead of assigning semantics to them, and HIR attaches them to ordinary value and type declarations while retaining dedicated export and target-import linkage for consumers that need semantic access without reparsing attribute arguments.

HIR validation rejects duplicate or unresolved symbols (including low-numbered IDs), unknown named types, invalid normalized schemes, invalid nested spans and fields, and schemes inconsistent with their values. LIR validation rechecks every semantic type and nested span in addition to artifact-local references, then verifies closure calls, branches and decision guards, primitive signatures, aggregate representations, declaration value types, intrinsic catalog membership, and complete target-import linkage. Artifact deserialization, LIR execution, and Go generation run the same validator, so malformed or directly constructed LIR cannot bypass compiler invariants.

Lexical `let` is retained as a structured LIR node. Its symbol is visible only in the body, or in both the binding and body for recursive functions. The interpreter closes the recursive environment explicitly; Go emission declares the function slot before assigning its closure. `raise` becomes an explicit `Fail` node so consumers preserve evaluation order and attach their own stage-specific failure diagnostic.

Matches lower to a structured decision tree whose target is evaluated once. Cases remain ordered; each executable plan applies precomputed projections and checks, creates artifact-local bindings, then evaluates its guard before selecting its body. A failed guard continues at the next case. Exhaustion is the explicit FScript match-failure path. The interpreter and Go runtime execute the same LIR plan vocabulary; neither reconstructs or independently expands FScript pattern syntax.

Ordered-map patterns retain ordered clause matching, repeated-binding consistency, remainder binding, and the existing rule that explicit clauses may leave unmatched entries. Type-reference patterns carry their resolved semantic type; consumers test primitive and aggregate representation shape and retain declared union identity.

Top-level mutual-recursive declarations are initialized as an atomic closure group. The interpreter creates every closure against the preceding environment, extends that environment with the complete group, and updates all captures together. Generated Go declares typed slots for all declarations and assigns them in source order from `init`, allowing closures in one recursive group to refer to every peer.

Local `let rec ... and ...` uses the same atomic rule inside a structured expression node. All local names are allocated before any member is lowered, every binding must evaluate to a closure, captures are updated with the complete group, and the body runs only after group initialization.

Language builtins are explicit `Intrinsic` declarations rather than magic unresolved references. Consumers accumulate their curried arguments according to the catalog signature and dispatch only at full application. Both executable consumers cover the complete pure scalar, string, list, option, and ordered-map catalog; a reachable non-portable linkage receives a stage-specific unsupported diagnostic or runtime failure.

The LIR interpreter implements the pure builtin catalog, including callbacks for list mapping, selection, collection, folding, filtering and search; lazy option fallback and mapping; and ordered-map construction, folds, transforms, iteration, and removal. Callback application goes through the same curried closure path as ordinary source calls, preserving failure propagation and evaluation order.

Generated Go represents every reachable FScript function instantiation as a native typed unary function. Currying becomes nested function types; partial application, recursion, captures, and higher-order calls retain concrete signatures without Go generics or universal closure values.

The Go runtime is stored as formatted `.go` resources embedded in `FScript.Backend.Go`. Program emission uses multiline structured constructs for closures, local control flow, decision trees, recursive groups, loops, and imports. Generation never invokes Go tooling; the backend tests run `gofmt -d`, `go test ./...`, and `go run` over generated projects.

Host externs are copied into IR as name, signature, and arity only; their executable .NET delegates never cross the compiler boundary. Constructing or retaining an unused extern handle is inert. Fully applying one in the LIR interpreter produces `FSI3021`, preserving the rule that unreachable unsupported linkage does not block execution.

`Task.spawn` and `Task.await` are compiler-known host-linkage descriptors with their polymorphic task signatures. This lets task-typed declarations survive inference, HIR, LIR, and artifact serialization. The LIR interpreter reports `FSI3021` only when either descriptor is fully applied; the Go backend performs deterministic reachability over declaration references and reports `FSB4004` when a reachable declaration requires a task representation.

Local binding patterns reuse the same single-scrutinee decision-tree representation as `match`. Sequential `for ... in ... do ...` is explicit in HIR and LIR so each consumer evaluates the input list once, visits values in order, and discards each body result before returning unit. `typeof` resolves declared record and union definitions during HIR construction and carries the semantic type as a serializable type-token node.

Every top-level source expression becomes a synthetic declaration at its exact position in the declaration stream; the program entry references the final such value. This preserves eager source ordering without evaluating the result expression twice. Top-level pattern bindings similarly store their scrutinee in one synthetic declaration, validate the pattern, and publish deterministic projections for each bound name.

Serialized `.fsir` files contain one closed resolved program plus explicit foreign linkage declarations. JSON format versions are internal and may be rejected rather than upgraded.

Artifact metadata records the actual compiler and language assembly versions, normalized paths and lowercase SHA-256 hashes for available file-backed sources, and a sorted unique list of foreign dependencies. Deserialization rejects empty versions, duplicate or unsorted sources/dependencies, non-normalized paths, malformed hashes, missing metadata, unknown formats or versions, and any LIR that fails post-load validation.

## Representation policy

- Validated LIR remains semantically typed and backend-neutral. A private Go representation pass specializes reachable declarations, local bindings, functions, captures, intrinsics, and imports by normalized concrete instantiation.
- Generated program and runtime source never use `any`, `interface{}` value carriers, reflection, a universal closure ABI, or Go generics.
- Lists use `[]T`; options, tuples, ordered maps, structural records, named records, and unions use deterministic concrete generated types. Recursive named types use pointer indirection.
- Functions use native typed unary function values. Structural equality, formatting, pattern projection, record conversion, and builtins are generated for their concrete representations.
- A reachable residual open representation produces `FSB4007`. An effectful generalized initializer that cannot be cloned without repeating observable evaluation produces `FSB4008`.
- LIR remains structured and non-SSA until a concrete optimization requirement justifies a control-flow IR.

Unused pure generalized declarations may be omitted. Observable monomorphic top-level initializers retain source order. Bottom expressions such as `raise` need no value representation and remain executable even when their semantic result type is open.

## Runtime and foreign calls

Compiler bindings classify names as portable intrinsics, backend runtime primitives, target imports, or unsupported host externs. This metadata is distinct from executable .NET `ExternalFunction` implementations.

`CompileInput.Externs` contains `CompilerExtern` descriptors only: name, normalized scheme, arity, and `CompilerExternKind`. Compilation creates private no-op inference shims because the language inference API still consumes `ExternalFunction`, but no executable delegate enters HIR, LIR, artifacts, the interpreter assembly, or a backend assembly.

`Console.writeLine` and `Console.readLine` are portable runtime intrinsics. LIR execution captures output in `ExecutionResult.Stdout` and reads input from process stdin; generated Go dispatches them to stdout and stdin. `readLine` returns `None` at EOF and preserves a final unterminated line. `Fs.readText` is the narrow portable read-only filesystem intrinsic used by command-line programs; the rest of the filesystem surface remains host-only. The cross-runtime harness separates written lines from the final value when comparing output.

`Env` is a compiler runtime-context primitive, not a serialized host delegate. LIR interpretation materializes its named record from `InterpreterContext`; generated Go materializes the same fields from `os.Args`, including the executable base name and arguments after it.

The LIR interpreter rejects reachable target imports it cannot supply. The Go backend initially rejects unsupported host externs and tasks with backend diagnostics. Imports describe complete ordinary FScript declarations plus target linkage and do not create a second foreign type system.

Initial Go imports use `[<Import("module/path", "Member")>] let name : complete -> signature` with no body. The frontend requires a function type and records linkage separately from its FScript signature. Go emission uses deterministic package aliases and currently accepts only primitive/unit arguments and results; other representations receive `FSB4003`. The LIR interpreter keeps unused imports inert and reports `FSI3023` only when one is fully applied.

Imported Go record types use `[<Import("module/path", "TypeName")>] type Name = { Field: primitive; ... }`. Their module/type linkage and ordered field schema survive HIR, LIR, and `.fsir` serialization. At imported function boundaries, generated Go converts FScript record maps to the declared Go struct and converts returned structs back to record maps. Fields must begin with an uppercase letter and currently use only unit, integer, floating-point, Boolean, or string representations; imported unions and other field shapes receive `FSB4005`.

## Cross-runtime conformance

The conformance suite evaluates one source program through the legacy evaluator, a serialized-and-deserialized LIR program, and a generated Go executable. It compares successful results across nested structural equality, inequality, non-equal closures, type tokens, named/structural width conversion, unions, higher-order and polymorphic functions, mutual recursion, recursive records, and ordered maps; it also normalizes raised errors, exhausted matches, and division by zero into shared failure categories. Named semantic types are resolved to their record or union representation while HIR is built, so a named record crossing a function boundary cannot accidentally receive the union representation in a downstream consumer.

## Previous experiment

The `feature/in-memory-execution` branch is useful only as evidence about closures, invocation, and host integration. Its dynamic-method/in-memory assembly backend is not an IR layer and is neither copied nor referenced by the new compiler architecture.

The optional migration of ordinary .NET script execution remains deliberately inactive: task execution and supported host-extern adapters have not reached full parity. The CLI and `ScriptHost` therefore retain the legacy evaluator for execution, and the Fable/JavaScript surface is unchanged; compiler CLI targets, LIR interpretation, and Go generation are available without deleting that reference path.
