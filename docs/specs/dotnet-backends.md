# C# and .NET backends

`FScript.Backend.CSharp` emits a C# project from validated LIR. `FScript.Backend.DotNet` has a separate assembly optimization pipeline and uses Roslyn for verified PE emission. Generated code contains ordinary compiled methods and typed delegates where functions escape, and depends on BCL support helpers rather than the AST evaluator, LIR interpreter, or ScriptHost.

Both backends share `LirMonomorphization` and `LirSimplification`. Reachable polymorphic uses receive concrete instantiations; reachable open results and effectful generalized initializers receive diagnostics.

For target selection and executable workflows, see [Compilation](../guides/compilation.md). The user-facing [optimization guide](../guides/compiler-optimizations.md) compares both targets and illustrates the limits described below.

## Purity contracts

`PurityContracts` contains `PureFunctions` and `PureTypes` sets of registered script names. The compiler API's explicit contract overloads default to empty sets through legacy entry points. The CLI discovers optional `FScriptPureFunctions`/`FScriptPureTypes` provider methods returning `IEnumerable<string>`. Function contracts include totality and effects; type contracts include deep immutability and safe registered-field reads. Provider initialization and callbacks must satisfy the documented conditional guarantee. Declarations are trusted; protected runtime contracts cannot be overridden.

`LirEffects` performs conservative contract-aware analysis shared by specialization and simplification. Recursive summaries start observable, avoiding circular proofs of termination. Unproven failures, non-exhaustive matches, external reads without type guarantees, and unknown callback effects remain observable. Pure calls can be pruned only with pure arguments and a correctly retained application boundary. Generated-member/dependency pruning then removes newly unreachable code. This does not change the AST evaluator or SDK validation.

## Assembly optimization

Both native consumers run `LirCommonSubexpressions.optimize` after specialization, simplification, and any target-specific LIR reduction. Dominance-based value numbering reuses identical typed pure expressions through lexical bindings and initialized globals. Keys omit source spans and retain exact floating-point bits. Trusted external-type immutability is required for external value observations and dependencies. Function- or task-containing result values are not shared, including aggregates whose elements contain these handles. Observable operations clear availability; branches, short-circuit operands, match guards, and loop bodies maintain separate conditional scopes. Earlier effectful guards invalidate availability for later alternatives. Function bodies start with empty availability and do not acquire caches across invocations. Administrative temporaries that have no sharing purpose are removed. The output uses existing LIR nodes and is validated before emission; artifacts and interpreted entry points do not enable this pass automatically. No memoization or runtime cache is introduced.

The .NET assembly pipeline additionally beta-reduces known nonrecursive calls with fresh lexical identities, propagates known partial applications, and hoists their top-level captures so effects execute once in source order. Recursive components are detected before expansion. Saturated calls use methods with multiple typed parameters. Exported or genuinely escaping functions retain typed delegates.

Saturated self calls in tail position become loops, including calls behind lets, branches, and pattern decisions. Next arguments are evaluated into temporaries before parameter reassignment. Escaping closures snapshot captures to preserve each iteration's immutable values. Mutual recursion and calls with pending work remain direct recursive calls.

Known list callbacks use typed loops and direct calls for mapping, iteration, folding, filtering, choosing, collecting, existence tests, and searches. Immediate helper blocks become static methods instead of delegate invocations. Constant boolean terms and branches are simplified while preserving observable evaluation. The CLR JIT can inline small native helpers.

Reachability starts at `Run`, result formatting, exported bindings, and the executable entry point. Unused generated methods, fields, and support types are removed. Reached constructors and interface implementations remain available. Native aggregates use immutable classes/structs without synthesized CLR record printing, equality, cloning, or deconstruction; FScript equality and formatting use dedicated reachable helpers. Ordinary assembly generation preserves provider assemblies; executable publishing can trim them as described below.

These passes apply only to `DotNetBackend.generate`. The C# target, serialized LIR, and LIR interpreter retain their existing optimization policy. Roslyn remains the final IL emitter; FScript makes the optimization decisions before emission.

Namespace qualification is not a reachability edge to a namespace's syntax body. Reaching a generated type or method through its fully qualified name retains that declaration and its actual dependencies, without retaining unrelated stdlib members in the same namespace. This rule applies to both libraries and executables.

## Public API

- `CSharpBackend.generate : CSharpOptions -> Lir.Program -> Result<GeneratedFile list, CompilerDiagnostic list>`
- `DotNetBackend.generate : DotNetOptions -> Lir.Program -> Result<GeneratedBinary list, CompilerDiagnostic list>`
- `DotNetBackend.generateExecutable : DotNetExecutableOptions -> DotNetOptions -> Lir.Program -> Result<GeneratedBinary list, CompilerDiagnostic list>`
- `ClrBindings.bindMethod : string -> MethodBase -> Lir.Program -> Lir.Program`
- `ClrBindings.bindType : string -> System.Type -> Lir.Program -> Lir.Program`

CLR linkage is serializable. Exact public overloads, instance receivers, and constructors are explicit. Native type projections use their actual CLR type. Existing extension providers use `ClrBindings.bindProvider`: generated typed adapters marshal values through `FScript.Extensibility`, invoke the provider directly, and service compiled callbacks and tasks through `ExternalCallContext`. Opaque values retain their SDK payload and immutable field snapshots. Providers taking `HostContext` receive the process working directory and compiled default functions. The CLI preserves provider metadata from environment loading rather than executing provider factories twice during compilation. No interpreter or ScriptHost participates in generated execution.

## Representation and execution

Provider classes may expose public static parameterless `FScriptCompiledFunctions` and `FScriptCompiledTypes` methods returning `IEnumerable<KeyValuePair<string, MethodInfo>>` and `IEnumerable<KeyValuePair<string, Type>>` respectively. Keys name registered script bindings. Compiler-only discovery links reachable declarations to exact typed CLR methods/types, overriding the legacy SDK adapter for those bindings. Metadata factories and unused bindings do not become runtime roots. This optional BCL contract preserves the frozen SDK registration records. Typed entry points must preserve the registered semantics and use immutable or snapshot-compatible external representations; unsupported shapes can use explicit adapters.

CLR type reachability is computed from retained expression/function signatures, patterns, constructors, and type tokens, including recursive references. Unused imported types are removed before resolving their native metadata. Emitted dependency copying starts at the PE's actual assembly references and follows the supplied adjacent managed closure; simply loading a provider during frontend registration does not retain its assembly.

Compiled `Env.ScriptName` contains the running assembly or executable filename; `Env.Arguments` contains its command-line arguments. The BASIC sample uses that name in its usage message, so compiled output does not advertise an interpreter invocation.

Closed exported bindings are specialization roots even when a library has no program entry. An export with an open polymorphic signature receives `FSC6009`; it cannot silently disappear during pruning. A known closure is devirtualized only when its captured values are available in the caller's lexical scope. Otherwise its delegate environment is preserved.

Scalars use `long`, `double`, `bool`, and `string`. Functions use typed `Func<A,B>` delegates. Lists use arrays, options use typed value structs, maps use sorted immutable entry arrays, tuples and records use generated immutable records, and unions use enums or sealed typed cases. Recursive records/unions use CLR references. Collection operations allocate changed values and retain source order. Structural equality and result formatting are generated per semantic type, including recursive aggregate equality and non-equality of functions/tasks.

Match targets are evaluated once. Typed checks/projections, repeated bindings, guards, and deterministic ordered-map selection follow the LIR pattern plans. An unbound variable key selects the first remaining entry; other key/value patterns select their first local match. Selection is committed before later clauses or remainder checks. Failures are ordinary runtime failures. Top-level initialization executes once and in source order through a lazy initializer outside CLR type initialization, allowing spawned tasks to call compiled functions without a type-initialization deadlock.

## Output and verification

`DotNetExecutableOptions` contains the explicit portable .NET `RuntimeIdentifier` and an `AheadOfTime` flag. `generateExecutable` accepts console output only. False produces a self-contained single-file bundle; true uses Native AOT. Both return exactly one executable (`.exe` for Windows targets). The CLI enables Unix execute permissions when writing Unix targets. DLL generation and the other backends do not invoke SDK publishing.

The CLI forms are `--target dotnet --single-file --runtime <rid>` and `--target dotnet --aot --runtime <rid>`. Missing runtime identifiers, runtime identifiers without a publishing mode, and executable flags on other targets are rejected. Runtime identifiers use .NET spelling, including `osx-arm64`, `linux-x64`, and `win-x64`. Unsupported identifiers produce `FSN7101` with the supplied value, supported identifiers, and a correction for recognized macOS aliases before invoking the publishing SDK. With source invocation, application arguments must follow `dotnet run --`; options consumed by `dotnet run` cannot be validated by FScript. Native AOT cross-OS compilation is rejected; cross-architecture support depends on the installed toolchain.

Publishing stages the optimized PE and managed dependency closure in an isolated temporary SDK project. Its `CoreCompile` placeholder is replaced with the optimized PE before publish targets consume it. Single-file publishing embeds the runtime and native runtime assets for extraction. Native AOT consumes the same PE, with SDK trimming and native compilation. Managed bundles enable trimming for static bindings and disable it for reachable legacy SDK dispatch. Only IL2040 (missing resources requested for removal by dependency linker descriptors, including optional FSharp.Core compiler metadata) is suppressed; reflection, trimming and AOT compatibility warnings remain errors. Remaining unbundled publish assets cause a diagnostic rather than incomplete output. Debug-symbol files are optional and are not returned.

Publishing requires a .NET 10 SDK on PATH and, for Native AOT, the platform native toolchain and compatible dependencies. SDK restore may need network access. `FSN7101` reports invalid publishing options, `FSN7102` reports an unavailable SDK command, `FSN7103` reports SDK publishing failures/timeouts or incomplete bundling, and `FSN7104` reports Native AOT cross-OS requests. The compiler captures SDK diagnostics and cleans its temporary project. Existing unrelated output-directory files are preserved.

C# output includes `Program.cs`, `Runtime.cs`, and `Program.csproj`. .NET output includes the assembly and, for console applications, a runtime configuration. External assembly references are explicit. Source generation does not invoke an installed compiler. Ordinary assembly generation requires the .NET 10 runtime but not an installed SDK; Roslyn dependencies ship with FScript. Running the compiler from source through `dotnet run` and SDK publishing do require the SDK. Assembly generation uses Roslyn in-process with release optimization, deterministic output, unsafe code disabled, and nullable checks enabled. Compiler errors and warnings prevent publication of an output assembly.

Tests load emitted assemblies, JIT every closed generated method, execute representative semantic programs and CLR calls, and build generated C# as an independent project. This checks valid IL and execution behavior without relying on FScript hosting.

Native AOT reports `FSN7105` when the optimized PE still contains reachable legacy `SdkInterop` dispatch. Such calls require typed companions or explicit CLR bindings, allowing individual FFI implementations to be inlined/trimmed without retaining a provider factory. Managed bundle mode continues to support legacy dispatch. Integration tests execute both output modes from a directory containing only the executable, with absent runtime roots, and cover direct typed FFI plus external values with an unused reflection-based sibling.

The standalone BASIC and headless graphics assemblies are also checked with ILVerify against the installed .NET runtime and emitted managed dependencies. C# output is syntax-checked and normalized before publication; generated projects explicitly include compiler-owned source files so stale files cannot enter a later build.

SDK programs additionally include `RuntimeSDK.cs` and explicit references to the extension SDK and FSharp.Core. Colocated managed dependencies are resolved transitively and copied beside emitted assemblies. Generated programs never load the source file or a serialized IR at runtime. The headless graphics sample exercises opaque graphics resources, geometry values, and callback invocation entirely through compiled code.
