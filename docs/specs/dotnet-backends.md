# C# and .NET backends

`FScript.Backend.CSharp` emits a C# project from validated LIR. `FScript.Backend.DotNet` has a separate assembly optimization pipeline and uses Roslyn for verified PE emission. Generated code contains ordinary compiled methods and typed delegates where functions escape, and depends on BCL support helpers rather than the AST evaluator, LIR interpreter, or ScriptHost.

Both backends share `LirMonomorphization` and `LirSimplification` with Go. Reachable polymorphic uses receive concrete instantiations; reachable open results and effectful generalized initializers receive diagnostics. The Go emitter retains its target-specific arity raising, tail loops, and collection fusion.

## Assembly optimization

The .NET assembly pipeline additionally beta-reduces known nonrecursive calls with fresh lexical identities, propagates known partial applications, and hoists their top-level captures so effects execute once in source order. Recursive components are detected before expansion. Saturated calls use methods with multiple typed parameters. Exported or genuinely escaping functions retain typed delegates.

Saturated self calls in tail position become loops, including calls behind lets, branches, and pattern decisions. Next arguments are evaluated into temporaries before parameter reassignment. Escaping closures snapshot captures to preserve each iteration's immutable values. Mutual recursion and calls with pending work remain direct recursive calls.

Known list callbacks use typed loops and direct calls for mapping, iteration, folding, filtering, choosing, collecting, existence tests, and searches. Immediate helper blocks become static methods instead of delegate invocations. Constant boolean terms and branches are simplified while preserving observable evaluation. The CLR JIT can inline small native helpers.

Reachability starts at `Run`, result formatting, exported bindings, and the executable entry point. Unused generated methods, fields, and support types are removed. Reached constructors and interface implementations remain available. Native aggregates use immutable classes/structs without synthesized CLR record printing, equality, cloning, or deconstruction; FScript equality and formatting use dedicated reachable helpers. User provider assemblies are not trimmed.

These passes apply only to `DotNetBackend.generate`. The C# target, serialized LIR, LIR interpreter, and Go backend retain their existing optimization policy. Roslyn remains the final IL emitter; FScript makes the optimization decisions before emission.

Namespace qualification is not a reachability edge to a namespace's syntax body. Reaching a generated type or method through its fully qualified name retains that declaration and its actual dependencies, without retaining unrelated stdlib members in the same namespace. This rule applies to both libraries and executables.

## Public API

- `CSharpBackend.generate : CSharpOptions -> Lir.Program -> Result<GeneratedFile list, CompilerDiagnostic list>`
- `DotNetBackend.generate : DotNetOptions -> Lir.Program -> Result<GeneratedBinary list, CompilerDiagnostic list>`
- `ClrBindings.bindMethod : string -> MethodBase -> Lir.Program -> Lir.Program`
- `ClrBindings.bindType : string -> System.Type -> Lir.Program -> Lir.Program`

CLR linkage is serializable. Exact public overloads, instance receivers, and constructors are explicit. Native type projections use their actual CLR type. Existing extension providers use `ClrBindings.bindProvider`: generated typed adapters marshal values through `FScript.Extensibility`, invoke the provider directly, and service compiled callbacks and tasks through `ExternalCallContext`. Opaque values retain their SDK payload and immutable field snapshots. Providers taking `HostContext` receive the process working directory and compiled default functions. The CLI preserves provider metadata from environment loading rather than executing provider factories twice during compilation. No interpreter or ScriptHost participates in generated execution. Go rejects CLR function imports with `FSB4010`.

## Representation and execution

Compiled `Env.ScriptName` contains the running assembly or executable filename; `Env.Arguments` contains its command-line arguments. The BASIC sample uses that name in its usage message, so compiled output does not advertise an interpreter invocation.

Closed exported bindings are specialization roots even when a library has no program entry. An export with an open polymorphic signature receives `FSC6009`; it cannot silently disappear during pruning. A known closure is devirtualized only when its captured values are available in the caller's lexical scope. Otherwise its delegate environment is preserved.

Scalars use `long`, `double`, `bool`, and `string`. Functions use typed `Func<A,B>` delegates. Lists use arrays, options use typed value structs, maps use sorted immutable entry arrays, tuples and records use generated immutable records, and unions use enums or sealed typed cases. Recursive records/unions use CLR references. Collection operations allocate changed values and retain source order. Structural equality and result formatting are generated per semantic type, including recursive aggregate equality and non-equality of functions/tasks.

Match targets are evaluated once. Typed checks/projections, repeated bindings, guards, and deterministic ordered-map selection follow the LIR pattern plans. An unbound variable key selects the first remaining entry; other key/value patterns select their first local match. Selection is committed before later clauses or remainder checks. Failures are ordinary runtime failures. Top-level initialization executes once and in source order through a lazy initializer outside CLR type initialization, allowing spawned tasks to call compiled functions without a type-initialization deadlock.

## Output and verification

C# output includes `Program.cs`, `Runtime.cs`, and `Program.csproj`. .NET output includes the assembly and, for console applications, a runtime configuration. External assembly references are explicit. Source generation does not invoke an installed compiler. Assembly generation uses Roslyn in-process with release optimization, deterministic output, unsafe code disabled, and nullable checks enabled. Compiler errors and warnings prevent publication of an output assembly.

Tests load emitted assemblies, JIT every closed generated method, execute representative semantic programs and CLR calls, and build generated C# as an independent project. This checks valid IL and execution behavior without relying on FScript hosting.

The standalone BASIC and headless graphics assemblies are also checked with ILVerify against the installed .NET runtime and emitted managed dependencies. C# output is syntax-checked and normalized before publication; generated projects explicitly include compiler-owned source files so stale files cannot enter a later build.

SDK programs additionally include `RuntimeSDK.cs` and explicit references to the extension SDK and FSharp.Core. Colocated managed dependencies are resolved transitively and copied beside emitted assemblies. Generated programs never load the source file or a serialized IR at runtime. The headless graphics sample exercises opaque graphics resources, geometry values, and callback invocation entirely through compiled code.
