# FFI inlining and IL rewriting tests

`tests/FScript.Backend.DotNet.Tests/InlinePropertyTests.fs` compares ordinary
calls with rewritten IL and checks source-level FFI calls against a direct host
reference. `InlineTests.fs` keeps fixed regressions for eligibility, growth
budgets, dependency pruning, and escaping bindings.

## Differential campaigns

The IL campaign builds small valid callers with Mono.Cecil, independently of
FScript lowering and Roslyn. It executes the same binary before and after
`DotNetIlInlining.rewrite`, comparing values, exception types/messages, and the
complete ordered host-effect history. Unexpected exceptions fail the property.

Fourteen caller forms cover direct calls, live stack values, repeated calls,
parameter mutation/address-taking, bounded loops, switches, catch/finally regions,
32/64-bit constant folding, checked arithmetic, and branch entries into constant
sequences. Twelve kernels cover argument ordering/reuse, branches, loops, integer
conversion, division, effects, void returns, and closed public generic helpers.
Structural checks require small arithmetic kernels to expand and all marker
methods to disappear. Fixed regressions require loop calls with live caller stack
values to remain ordinary calls, while a loop called on an empty stack can expand.

The source campaign covers captured partial applications, effectful arguments,
list callbacks, and unused results. Both ordinary and opted-in .NET output must
match a direct host reference, including effects before division/overflow failures.
The host reference evaluates the captured first argument once and stops at the
first failure. These checks do not depend solely on agreement between two builds
that share the same lowering passes.

Seeds `104729` and `130363` run twenty IL cases per caller form and four source
cases per source shape: 560 IL comparisons plus 32 source comparisons. Inputs mix
small integers with zero, negative one, and signed 64-bit limits. Caller loops run
at most five times; the 32-bit drain kernel receives at most twenty positive steps.
Shrinking reduces operands and loop bounds while retaining the selected kernel
and caller form. A harness regression checks shrinking and exact failure replay.

## Running and replaying

```sh
dotnet test tests/FScript.Backend.DotNet.Tests/FScript.Backend.DotNet.Tests.fsproj -c Release --filter FullyQualifiedName~Inline
```

Failures print generator version 1, original/reduced inputs, the failing source
for source cases, and a complete `FSCRIPT_INLINE_REPLAY=seed,gamma,size` command
that selects the failing NUnit test. Use the printed values and filter. Omitting
`,size` replays a stream. Exact replay requires the same generator and FsCheck
3.6.0 versions. Invalid replay settings fail explicitly.

## Independent IL verification

Install the independent verifier into the path used by the tests:

```sh
dotnet tool install dotnet-ilverify --tool-path .out/ilverify --version 10.0.0
```

The generated-caller verifier checks all 168 caller/kernel combinations both
before and after rewriting, so invalid test fixtures cannot masquerade as
rewriter bugs. The other ILVerify tests check compiled FScript programs and native
collection representations. These tests skip when the verifier is absent.

This campaign checks the current supported static managed FFI subset. It does not
prove arbitrary foreign methods safe to expand. Eligibility checks continue to
reject unsupported frames and inaccessible references, and provider authors retain
the documented promise of call-site independence. See the
[foreign expansion contract](../specs/dotnet-backends.md#explicit-foreign-il-expansion)
and the separate [four-way conformance campaign](backend-property-tests.md).
