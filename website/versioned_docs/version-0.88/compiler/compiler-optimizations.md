---
id: compiler-optimizations
title: Compilation and optimizations
slug: /compiler/optimizations
---

Both native targets use the same inferred FScript types. The .NET assembly target adds passes intended to reduce calls, closure work, and unused generated code. The C# target produces a complete buildable source project and does not run those additional assembly passes.

These passes run automatically. There is no FScript CLI optimization-level switch. Native AOT is a publishing choice after assembly optimization, rather than a separate FScript language mode.

## Which target performs which work?

| Transformation | C# project | .NET assembly |
| --- | --- | --- |
| Specialize reachable polymorphic uses | Yes | Yes |
| Simplify constants, branches, aliases, and unused pure bindings in shared IR | Yes | Yes |
| Use declared FFI purity and external-type guarantees | Yes | Yes |
| Eliminate repeated proven-pure expressions | Yes | Yes |
| Scalar-replace nonescaping internal records and forward compatible projections | Yes | Yes |
| Additional known-call and partial-application reduction | No | Yes |
| Native methods for saturated calls | No | Yes |
| Explicit per-binding FFI IL expansion | No | Opt-in |
| Proven self-tail calls converted to loops | No | Yes |
| Typed loops for known collection callbacks | No | Yes |
| Propagate compatible record layouts through known collection consumers | No | Yes |
| Fuse proven-pure map/filter/fold pipelines and range producers | No | Yes |
| Eliminate local tuple temporaries and use small value-type tuple carriers | No | Yes |
| Traverse native list tails through array views | No | Yes |
| Materialize filters with compact masks and exactly sized output arrays | Yes | Yes |
| Aggressive pruning of generated methods, fields, and support types | No | Yes |
| FScript-managed single-file or Native AOT publishing | No | Yes, when requested |

Building C# in Release mode still enables the C# compiler's normal optimizations. Managed assemblies receive JIT optimization at runtime; Native AOT performs native compilation at publish time. FScript does not guarantee that either toolchain will inline a particular CLR method.

## Concrete specialization

A polymorphic declaration can be instantiated at several concrete types:

```fsharp
let identity value = value
(identity 42, identity "answer")
```

Each reachable use receives a concrete representation and function signature. Specialization keys deduplicate matching instantiations and handle recursive functions before traversing their bodies. An unused pure polymorphic declaration need not appear in the output.

A reachable value must have a representable type. A final unconstrained function is a compilation error; a function with concrete arguments and a concrete result can remain a first-class value. Exported library bindings also need concrete native types. Specialization can increase code size when a function is used at many types.

## Calls, currying, and captures

The .NET backend follows known function values through bindings and partial applications. When it can prove the callee and make its captures available, it reduces a nonrecursive call into the caller's body or emits a typed method call with the supplied arguments.

```fsharp
let add amount value = amount + value
let increment = add 1
List.map increment [1..4]
```

This need not allocate a general partial-application object on each iteration. Known applications can become direct work in a typed collection loop. Captured expressions still evaluate once at the correct source position.

Functions that escape, depend on an unknown higher-order parameter, or cross an FFI callback boundary may retain typed delegates and captured environments. Compilation does not remove every closure, delegate invocation, or allocation.

## Capture cleanup after tail-loop lowering

The assembly backend specializes known captured functions before native lowering. A local partial function whose implementation and captures are known can become direct arithmetic in its consumer, including a recursive loop. Actual captures remain explicit; separate invocations retain separate immutable environments.

After tail calls become loops, the backend removes unused private native parameters and their inert arguments, then removes unreachable local closure cycles and capture snapshots. These passes iterate to a fixed point before member pruning. Delegate method-group references retain their original signatures. Calls and external property reads with possible effects or failures remain ordered. Escaping functions and unknown callbacks can still require delegates. This cleanup does not change interpreter execution or serialized LIR.

## Tail recursion

Proven saturated self calls in tail position become loops in the .NET assembly target:

```fsharp
let rec sum remaining total =
    if remaining = 0 then total
    else sum (remaining - 1) (total + remaining)

sum 1000000 0
```

The recursive call is the complete result of that branch. Next arguments are evaluated before parameters are reassigned and execution continues. The compiler recognizes tail calls behind local bindings, conditionals, and pattern decisions. Escaping closures retain captured values from their original iteration.

A call with pending work is not a tail call:

```fsharp
let rec fibonacci n =
    if n < 2 then n
    else fibonacci (n - 1) + fibonacci (n - 2)

fibonacci 20
```

The additions still need the call results, so this remains recursive. Mutual recursion is also not converted into a native loop by the current assembly pass. Do not assume arbitrary recursion is stack-safe in compiled output.

## Collections and Boolean expressions

Known list callbacks can lower to typed loops and direct calls for mapping, iteration, folding, filtering, choosing, collecting, existence tests, and searches. This avoids some delegate machinery; operations that create changed immutable collections still allocate their results.

Constant Boolean terms and branches are simplified when doing so preserves evaluation. A condition such as `condition && true` can become `condition`. Short-circuiting still skips the right operand when the left operand determines the result. An effectful or potentially failing expression cannot be dropped merely because a later term is constant.

## Common-subexpression elimination

Both compiled targets reuse a proven-pure computation when the same typed expression has already been evaluated on every path reaching the later use:

```fsharp
let score n =
    ((n + 1) * (n + 1), (n + 1) * (n + 1))
```

The addition and multiplication can each execute once per call to `score`. The pass also handles repeated intrinsic calls, explicitly pure FFI calls, immutable aggregates, and field observations backed by pure external-type contracts. It follows immutable aliases and preserves concrete types, lexical identities, and floating-point bits, including signed zero.

Reuse starts at the first evaluation; the compiler does not move branch-local work ahead of its condition. A value computed before a branch can serve either arm, but values computed inside an arm do not escape into another arm or the continuation. Short-circuit operands, pattern guards, and loop bodies retain their conditional execution. Observable operations invalidate available computations; unmarked external values and potentially failing or nonterminating computations remain conservative.

Shared values are ordinary local bindings or previously initialized globals. Function invocations and loop iterations have separate local evaluations. No runtime lookup table, persistent cache, or memoization is introduced. Computations that are not shared retain their original expression shape after temporary cleanup.

## Dead code and effects

Generated-member reachability begins at program execution, result formatting, the executable entry point, and exported bindings. Unused generated methods, fields, runtime helpers, and types can be removed. Closed exports remain available to callers even if the script itself does not invoke them.

Observable initialization remains ordered:

```fsharp
let unused value = value + 1
let announced = Console.writeLine "starting"
42
```

The unused function can disappear, but the console write must remain. Console operations, filesystem access, tasks, imports, unknown callbacks, and failures are treated conservatively. Neither an unused result nor immutability makes I/O pure.

A generalized initializer cannot be duplicated if it performs observable work. The compiler reports a diagnostic rather than introducing a value restriction or silently repeating the effect. Move one-time effects outside the generalized function or give its boundary a concrete type.

## Trusted external purity

[FFI providers](./compile-to-dotnet.md#pure-ffi-and-external-values) can declare the same strong `Pure` guarantee used by the optimizer: deterministic, effect-free, and guaranteed to terminate successfully for valid inputs. External type guarantees additionally cover deep immutability and safe, terminating registered field reads, equality, and formatting. No callable member becomes pure just because its receiver type is pure. Implicit imported-record construction, updates, and conversions remain conservative because they may invoke CLR constructors. A pure type does not make those constructors pure; use an explicitly pure factory binding when that guarantee holds.

Both source and assembly specialization can use these contracts. An unused pure call may be removed, including through known FScript wrappers. Afterward, unused bindings and adapters can disappear; the assembly target can also omit provider dependencies that are no longer referenced. Argument expressions are analyzed separately, so `pureFunction (1 / 0)` must still fail and an argument that writes to the console must still execute.

Curried application boundaries are retained in IR. Creating a pure function value and invoking a callable returned from it are separate operations. Callback-taking FFI and known higher-order collection operations require sufficiently strong callback proofs; unknown or effectful callbacks preserve the call.

Failure is observable. Division is not considered pure unless its divisor and any signed-overflow case are proven safe. Explicit failures, potentially exhausted matches, and unproven recursive cycles remain observable even without mutation or I/O. Tail-call lowering does not establish termination. Observable generalized initializers that cannot be represented safely receive a diagnostic rather than silently disappearing.

The contracts survive resolved inference, HIR, LIR, CLR linkage, specialization, and `.fsir` serialization. Older artifacts without them load conservatively. Provider declarations are trusted promises; FScript cannot generally verify their implementations. These contracts support pruning, safe local propagation, and common-subexpression elimination in both compiled targets. They do not enable automatic memoization.

## FFI pruning and executable publishing

There are two levels of pruning:

- **Assembly generation:** remove unused generated code and external type declarations, and omit provider assemblies that the emitted program does not reference. Referenced provider DLLs remain intact.
- **Trimmed publishing:** remove unreachable code inside dependencies as well. Managed single-file output enables this when all reachable FFI bindings are static; Native AOT trims dependencies before native compilation.

SDK provider factories can return opaque delegate registrations. Sibling implementations may remain reachable through registration, and managed single-file output disables trimming when that legacy dispatch path is used. Native AOT reports `FSN7105` for a reachable legacy SDK adapter. [Typed FFI mappings](./compile-to-dotnet.md#prunable-typed-ffi-bindings) provide exact CLR calls so the linker can follow their dependencies and trim unused implementations.

Self-contained single-file output still includes a managed runtime. Native AOT produces native machine code, but neither mode embeds arbitrary input files or removes the need for normal OS libraries. See [the .NET publishing guide](./compile-to-dotnet.md#single-file-and-native-aot-executables) for runtime identifiers and toolchain requirements.

## Interpreted execution

Ordinary CLI execution and `ScriptHost` still use the AST interpreter; JavaScript embedding remains interpreted. Native specialization, call lowering, and assembly pruning are not enabled on those paths. The separate LIR interpreter executes validated IR and has its own tail-call trampoline; it does not run the .NET assembly backend's passes.

For API contracts, representations, and validation details, see the [.NET backend specification](https://github.com/MagnusOpera/FScript/blob/main/docs/specs/dotnet-backends.md).

## Discarding the entry value

Compile with `--no-result` to remove the entry value as a reachability root before specialization. Pure and total computations used only for that value can disappear, including their functions, helpers, and dependencies. This can also permit an unused open pure value such as `None` to be discarded. Observable calls, potential failures, and `Env.setExitCode` writes remain roots; the option never licenses dropping failures or effects. No result-based exit-code inference is performed.

Printing remains the default. Setting an exit code is never pure, including when two writes use the same integer. Tasks may share this status, so synchronized writes remain scheduling-dependent and are not deduplicated or reordered.

## Temporary records and representation conversions

Both compiled targets scalar-replace internal record temporaries used only by field projections, updates, or compatible width conversions. Field expressions keep their original evaluation order and execute once, including unused fields that perform I/O or fail. A projection through a compatible internal record conversion can read the original field directly. A record returned under a different internal record signature can be constructed in the destination representation.

The .NET backend repeats this cleanup after known-call reduction and reachability pruning, so removing an unused closure can expose further temporary records. Records that escape through unknown consumers or captures remain ordinary values. Imported record constructors/getters and nested field adaptations are conservative boundaries; this pass does not treat them as plain field copies. Record scalar replacement alone does not fuse collection pipelines or eliminate their materialized result collections; the assembly-only fusion pass described below can remove intermediate arrays and expose further record elimination.

## Comparing generated IL with F#

A [reproducible compiler comparison](https://github.com/MagnusOpera/FScript/tree/main/benchmarks/FScript.DotNet.Comparison) checks equivalent FScript and F# programs covering tail loops, record pipelines, payload unions, and recursive quicksort. In a source checkout, run `make benchmark-dotnet-comparison` to validate results, measure warmed execution and allocations, and inspect both assemblies' IL. F# list and array baselines help distinguish compiler optimizations from collection representation costs.

The measurements exclude interpretation, startup, and compilation. IL summaries describe static code before the .NET JIT performs its own optimizations; they are not machine-code performance guarantees. The accompanying correctness and IL checks run in the test suite without timing thresholds.

## Explicit FFI IL expansion

An FFI provider can mark individual typed bindings with `FScriptInlineFunctions()`, independently of `Pure`. The .NET backend copies eligible IL into the caller after source emission, remaps its control flow and locals, and cleans up argument slots and exposed integer constants. Exact forwarding adapters collapse to direct calls. This is an explicit backend transformation, not a JIT inlining hint.

Effects and possible failures remain ordered. Unmarked aliases of the same native method remain calls, and unsupported reachable bodies receive diagnostics rather than silently ignoring the opt-in. The initial subset excludes exception handlers, type initializers, instance methods, private references, pointers, open generics, and stack-sensitive implementations. Provider authors must ensure removing the call frame is semantically safe. See [the Inline contract](./compile-to-dotnet.md#explicit-ffi-inline-contracts) for eligibility and examples.

Expansion is also bounded by size: 16-byte tiny and 100-byte candidate screens, post-cleanup growth checks, and a cumulative caller budget. Exact forwarding adapters avoid duplication; oversized or unprofitable bodies remain ordinary typed calls. These are conservative starting heuristics, not a guarantee of optimal native code. See [the size policy](./compile-to-dotnet.md#size-and-growth-policy).


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

Materialized `List.filter` operations in both compiled targets record selection decisions in compact 32-bit masks, count accepted elements, then allocate and fill an exactly sized result array. Inputs of up to 32 elements keep their mask in a scalar; larger inputs allocate one bit per element rounded to mask words. Empty results reuse the empty array. Workers build masks a word at a time and copy selected elements from a local readonly span, without growing element buffers. This trades compact decision storage and a second traversal for fewer result-buffer allocations; allocation savings depend on input size and selection density. Known callbacks retain direct typed calls; unknown callbacks use a typed runtime helper.

Predicates run exactly once per element in source order, including effectful predicates. A failing predicate stops the operation immediately. The second traversal only copies accepted values and does not invoke the predicate again. These are private backend bit operations; no bit-operation syntax or public IR change is introduced. Pure pipeline fusion can still eliminate materialized filters entirely.
