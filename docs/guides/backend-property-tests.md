# Backend conformance property tests

`tests/FScript.Conformance.Tests/BackendPrograms.fs` generates bounded,
well-typed source programs. Each program runs through four paths:

1. The source interpreter, which supplies the reference value and console calls.
2. The LIR interpreter after artifact serialization and deserialization.
3. Generated C#, compiled in memory with Roslyn in Release mode.
4. The .NET assembly backend, including its native optimizations and IL rewriting.

Successful runs must agree on the formatted result and ordered `Console.writeLine`
arguments. Console capture preserves embedded newlines within a single call.
Failure programs must agree on explicit raise or integer division by zero.
Compilation errors, warnings, and unexpected runtime exceptions fail the property; they do not count as equivalent script failures. Failure programs have no
console effects, so this campaign does not compare output preceding a failure.

## Generated programs and limits

The typed expression grammar includes integer, boolean, and string literals,
integer addition/subtraction, scalar equality, conditionals, and int lambda
application with parameter references and shadowing. Generation does not call the
parser or type checker to select or discard inputs.

Nine program shapes guarantee coverage of scalar/tuple/list/option results,
captured closures and polymorphic functions, generic records/unions, list
map/filter/fold and map lookup, tail recursion, guarded exhaustive matches,
ordered console effects, and the two failure categories. Expression depth is at most three, lists contain at most
four elements, recursion uses at most six steps, and integer literals are in
`-12..12`. Unicode, quotes, backslashes, and newlines exercise string formatting.
These bounds avoid overflow and unbounded computation in correctly executing
programs.

This is a bounded language subset, not exhaustive syntax coverage. It excludes
floating-point arithmetic, host-managed/opaque types, asynchronous tasks,
mailboxes, filesystem effects, arbitrary externs, and executable packaging.
The [runtime FFI campaign](runtime-ffi-property-tests.md),
[mailbox campaign](mailbox-property-tests.md), and
[inference campaign](inference-property-tests.md) cover their respective contracts.
C# and .NET share frontend and code-generation machinery, so four-way agreement
cannot detect every common defect. The source evaluator remains independent of
the typed compiler pipeline.

## Running and replaying

```sh
dotnet test tests/FScript.Conformance.Tests/FScript.Conformance.Tests.fsproj -c Release --filter FullyQualifiedName~BackendPropertyTests
```

FsCheck 3.6.0 runs eight programs per shape for each seed, `104729` and `130363`,
for 144 four-way comparisons. Every seed and shape has a separate NUnit test name.
The budget is deliberately smaller than the inference and runtime campaigns:
each candidate and each shrink compiles two native assemblies. Assemblies use
collectible load contexts, and the fixture runs without parallel console capture.

Failures print generator version 1, the original and minimized programs, the
expression trees, the mismatch or unexpected exception, and a complete command
that selects the failing shape/seed test. For example:

```sh
FSCRIPT_BACKEND_REPLAY='123,4567,24' dotnet test tests/FScript.Conformance.Tests/FScript.Conformance.Tests.fsproj -c Release --filter 'Name~Scalars_104729'
```

Use the seed, gamma, size, and test filter printed by the failure. The example
numbers above are placeholders. `FSCRIPT_BACKEND_REPLAY=seed,gamma` replays a stream;
adding `,size` replays the failing case at that size and repeats shrinking. Invalid
replay settings fail rather than silently falling back to a new random stream.
Exact replay requires the same FsCheck version and generator version.

Shrinking reduces literals, typed subexpressions, list elements, and recursion
bounds while preserving the program shape and intended failure category. Lambda
bodies retain their binders, so reductions cannot introduce free variables.
Generator checks compile programs and their immediate shrinks, including a forced
parameter-reference case. An injected mismatch test checks that shrinking makes
progress and that replay reproduces both the original and minimized counterexample. Property callbacks use
ordinary exceptions; only the final FsCheck outcome is reported to NUnit.

The separate [FFI inlining campaign](ffi-inline-property-tests.md) compares ordinary
and rewritten foreign calls, including effects, exception regions, and independent
IL verification.
