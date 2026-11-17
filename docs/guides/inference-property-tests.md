# Inference property tests

The language test suite uses FsCheck to test generalization and captured opaque
types. Run the campaign with:

```sh
dotnet test tests/FScript.Language.Tests/FScript.Language.Tests.fsproj -c Release --filter FullyQualifiedName~InferencePropertyTests
```

Every property runs 150 samples for each of the fixed seeds `104729` and `130363`.
The four generic property families each run for a record, a union, a record inside
a union, and a union inside a record. Together with the original five properties,
the campaign runs 6,300 generated samples.

No clock-based seed or inference-based candidate filtering is used. Generated
types are concrete combinations of `int`, `bool`, `string`, lists, options,
tuples, and two-parameter generic records and unions. Type nesting is bounded at
two levels and transport chains at six steps. Samples grow with FsCheck's size
parameter, from zero to forty.

## Properties and independent expectations

- Pure identity bindings must instantiate at two different concrete types.
- Closures reading a captured ordinary parameter must retain that parameter's
  type. Two incompatible requirements must fail inference.
- Closures reading one allocated opaque `Cell<'a>` must retain that instance's
  type, including uses through aliases and aggregates.
- Separate calls to a forwarded factory must produce independently instantiated
  opaque instances.
- An identity callback paired with a closure over an opaque instance must remain
  polymorphic even when the instance's type becomes fixed.
- Generic constructors must instantiate independently. One pure generic value
  containing an empty list and an independent identity callback must allow both
  to remain polymorphic. Lists of differently instantiated concrete generic
  values must be rejected, with homogeneous lists as acceptance controls.
- A closure capturing a generic aggregate containing one allocated opaque cell
  must retain that cell's type through projection and pattern matching.
- Separate calls to factories returning generic aggregates of opaque cells must
  instantiate independently, including when the factory itself passes through
  generic record or union transports.
- A generic aggregate containing an opaque cell and an unrelated callback must
  keep the callback polymorphic while rejecting conflicting uses of the cell.

Generated transport steps include aliases, forwarding calls, tuple and structural
record projections, option and list matches, closures, branches, and local lets.
Generic transports add record projection, record patterns, record updates, and
union construction and matching. Generic declarations use two type parameters,
and samples vary inferred and explicit generic declarations and constructor type
arguments. The generic container in each new scenario is mandatory, so shrinking
away optional transports cannot turn it into a test of an ordinary tuple alone.
A fake host registration defines
`Cell.create` and `Cell.read`; its implementations throw if executed. These tests
run parsing and inference only and do not create mailbox workers.

The generator derives expected result types directly from its type description.
It never asks the type checker which programs to keep. Rejection cases also run
an acceptance control with compatible uses. Parsing happens outside the expected
type-error handler, so malformed source cannot count as a successful rejection.
Accepted programs must have the exact expected concrete result type; merely
accepting a program is insufficient.

These properties exercise the contracts in [supported types](../specs/supported-types.md#type-inference-model)
and [generic provider types](../specs/host-managed-types.md#generic-provider-types).
They do not claim exhaustive syntax coverage, runtime correctness, or inference
correctness outside the generated subset.

## Shrinking and replay

On failure, FsCheck removes transport steps, simplifies nested types, removes
optional annotations, and reduces the two distinct primitive types to `int` and
`string`. Generic payload shapes can reduce to their contents or shrink their
contents while retaining the enclosing record or union. The scenario renderer
always retains the required generic container, allocation, capture, and the
conflicting uses. A negative example cannot shrink into an unrelated parse error
or lose the distinction between its two required types.

Failure output contains the original and reduced source, expectations, generator
version, actual result or exception, and a command like:

```sh
FSCRIPT_INFERENCE_REPLAY='123,4567,20' dotnet test tests/FScript.Language.Tests/FScript.Language.Tests.fsproj -c Release --filter 'FullyQualifiedName~InferencePropertyTests.CapturedOpaqueInstancesRemainMonomorphic'
```

Generic failures add an aggregate name to the filter, for example:

```sh
FSCRIPT_INFERENCE_REPLAY='123,4567,20' dotnet test tests/FScript.Language.Tests/FScript.Language.Tests.fsproj -c Release --filter 'FullyQualifiedName~InferencePropertyTests.GenericAggregatesPreserveCapturedOpaqueTypes&Name~RecordInUnion'
```

Copy the numbers from the failure report. The three fields are the failing
generation seed, its odd gamma, and its size. This replays that one generated case
and its shrinking. Both seed variants of the selected NUnit test use the override.
To replay a complete campaign with another seed, omit the size:

```sh
FSCRIPT_INFERENCE_REPLAY='123,4567' dotnet test tests/FScript.Language.Tests/FScript.Language.Tests.fsproj -c Release --filter FullyQualifiedName~InferencePropertyTests
```

Unset `FSCRIPT_INFERENCE_REPLAY` to restore the fixed campaigns. Generator version
2 adds generic payload shapes and transports. It keeps the original default
seeds, but their generated sequences differ from version 1. Replay requires the
same generator and FsCheck version; replay an older report at its source revision.
Preserve a discovered failure's reduced source as a normal regression test rather
than relying on the seed indefinitely.

The harness tests inject record-transport and generic-payload faults, verify that
shrinking retains their triggers, and replay both the original and reduced
counterexamples. They also run generated acceptance and rejection expectations
after structural reductions. Focused regression cases cover union constructors
forwarding opaque factories by short name, qualified name, and alias, plus
top-level, parameter, and local bindings that shadow constructor names.
