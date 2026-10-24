# C# and .NET backends

`FScript.Backend.CSharp` emits a C# project from validated LIR. `FScript.Backend.DotNet` compiles the same generated source to a deterministic PE assembly using Roslyn. Generated code contains ordinary compiled methods and typed delegates and depends on BCL support helpers rather than the AST evaluator, LIR interpreter, or ScriptHost.

Both backends share `LirMonomorphization` and `LirSimplification` with Go. Reachable polymorphic uses receive concrete instantiations; reachable open results and effectful generalized initializers receive diagnostics. The Go emitter retains its target-specific arity raising, tail loops, and collection fusion.

## Public API

- `CSharpBackend.generate : CSharpOptions -> Lir.Program -> Result<GeneratedFile list, CompilerDiagnostic list>`
- `DotNetBackend.generate : DotNetOptions -> Lir.Program -> Result<GeneratedBinary list, CompilerDiagnostic list>`
- `ClrBindings.bindMethod : string -> MethodBase -> Lir.Program -> Lir.Program`
- `ClrBindings.bindType : string -> System.Type -> Lir.Program -> Lir.Program`

CLR linkage is serializable. Exact public overloads, instance receivers, and constructors are explicit. Native type projections use their actual CLR type. Unknown callbacks without a binding fail generation; no interpreter callback is invoked by generated code. Go rejects CLR function imports with `FSB4010`.

## Representation and execution

Scalars use `long`, `double`, `bool`, and `string`. Functions use typed `Func<A,B>` delegates. Lists use arrays, options use typed value structs, maps use sorted immutable entry arrays, tuples and records use generated immutable records, and unions use enums or sealed typed cases. Recursive records/unions use CLR references. Collection operations allocate changed values and retain source order. Structural equality and result formatting are generated per semantic type, including recursive aggregate equality and non-equality of functions/tasks.

Match targets are evaluated once. Typed checks/projections, repeated bindings, guards, and map backtracking follow the LIR pattern plans. Failures are ordinary runtime failures. Top-level initialization executes once and in source order through a lazy initializer outside CLR type initialization, allowing spawned tasks to call compiled functions without a type-initialization deadlock.

## Output and verification

C# output includes `Program.cs`, `Runtime.cs`, and `Program.csproj`. .NET output includes the assembly and, for console applications, a runtime configuration. External assembly references are explicit. Source generation does not invoke an installed compiler. Assembly generation uses Roslyn in-process with release optimization, deterministic output, unsafe code disabled, and nullable checks enabled. Compiler errors and warnings prevent publication of an output assembly.

Tests load emitted assemblies, JIT every closed generated method, execute representative semantic programs and CLR calls, and build generated C# as an independent project. This checks valid IL and execution behavior without relying on FScript hosting.
