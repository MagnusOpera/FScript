# Runtime FFI property tests

The runtime suite uses FsCheck to exercise the default FFI catalog with bounded
inputs, independent expectations, shrinking, and replay. Run these suites with:

```sh
dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter 'FullyQualifiedName~Property&FullyQualifiedName!~Mailbox'
```

The fixed seeds are `104729` and `130363`. The new campaign runs 3,500 generated
cases. The separate [mailbox campaign](mailbox-property-tests.md) adds 520 cases;
the [inference campaign](inference-property-tests.md) retains its own generator
and replay configuration.

## Coverage and expectations

| Area | Generated cases | Expectations |
| --- | ---: | --- |
| JSON and JSON decoding | 600 | Independently written JSON decodes to the generated typed value; serialization matches that JSON structurally; missing optional fields, invalid shapes, opaque targets, unsupported values, and non-finite numbers follow their contracts. |
| Internal XML encoding and decoding | 400 | Independently constructed elements decode to typed values; supported record roots roundtrip; optional fields, bool-keyed maps, unsupported targets, and unsupported nested values are checked. |
| XML queries | 200 | Paths, descendants, attributes, trimmed text, duplicate order, missing matches, malformed XML, and unsupported query syntax match a document model. |
| Regex | 200 | Escaped literal capture groups return exactly their inputs; noncapturing matches return an empty group list; absent optional captures, mismatches, and invalid patterns have known outcomes. |
| Encoding | 400 | UTF-8, Base64, hex, and URI transforms match standard encoders and decoders; invalid bytes, malformed escapes, invalid UTF-16, and malformed encodings return `None`. |
| Hash | 200 | MD5 and SHA-256 match standard byte digests for generated UTF-8 text and binary data. |
| Guid | 100 | Varied ignored arguments produce lowercase canonical version-four GUID strings with the expected variant bits. Exact entropy and collision-free batches are not test oracles. |
| Console | 100 | Text output, line input, and repeated EOF preserve their documented values. |
| Registry permissions | 200 | An independently declared capability matrix predicts function availability and filesystem/mailbox metadata. |
| Filesystem | 500 | Operation sequences match a file/directory model; root escapes and denied paths preserve private fixture sentinels; lexical helpers preserve generated components. |
| Task | 600 | Finite task graphs match pure folds, preserve collection order and repeated handles, retain composite result types, suppress callbacks on failed prerequisites, join effects and collect failures, select a controlled ready input, and reject invalid bounds and empty `whenAny`. |

`FfiPropertyTests.ExpectedCatalog` explicitly lists all 40 registered externs and
their required permissions. A catalog guard fails if a default extern is added,
removed, or changes permissions without updating that inventory. Its entries
correspond to the property groups above and the existing mailbox properties.
This covers the default catalog, rather than arbitrary third-party providers or
all runtime hosting and package-loading APIs.

`Decode.fs` is an internal codec implementation, not a public `Decode` module in
scripts. The runtime grants friend access to `FScript.Runtime.Tests`, allowing
direct tests of its actual JSON/XML codec functions. No `Xml.serialize` or
`Xml.deserialize` extern is introduced.

## Bounds and deliberate subsets

- General samples contain at most 16 Unicode scalar tokens, 64 bytes, eight
  integers, and an integer between -100 and 100. Shrinking removes Unicode
  scalars without splitting surrogate pairs.
- Typed codec data has record/tuple nesting depth two, at most three fields,
  and at most four list or map entries. Integer atoms include `int64` endpoints;
  finite float atoms are exact quarter fractions. Options contain scalar values.
- XML codec inputs use legal XML text and record roots. Roundtrip parsing
  preserves whitespace. XML's unsupported tuple/map encoding and lossy nested
  option representations are excluded from the positive roundtrip generator,
  with unsupported encoding forms tested separately. XML map decoding is tested
  directly for valid bool keys.
- Regex patterns are bounded literal constructions, not arbitrary expressions
  with potentially unbounded backtracking.
- Filesystem sequences contain at most sixteen optional operations over twelve
  logical file paths and three directories, plus a required initial write.
  Enumeration comparisons ignore filesystem iteration order but reject duplicate
  entries. Every case uses and removes its own temporary root; escape attempts
  target sibling paths inside the same private fixture directory. Existing fixed
  tests cover platform-dependent symlink behavior.
- Task graphs contain at most eight transformations or input jobs and delays
  from zero through three milliseconds. Scheduling time is never an expected
  result. Controlled `whenAny` inputs use a pending completion source and an
  already-completed handle; the pending source is completed in `finally`.
  A five-second watchdog fails and releases that owned source if a regression
  incorrectly waits for it; the deadline is never a selection oracle.
  Successful effects are compared as multisets when their order is unspecified.

Console properties are nonparallelizable and restore process input/output in
`finally`. Temporary-directory names and GUID output use host entropy for their
normal purposes; seeds reproduce logical test inputs, not those incidental
values. All generated script programs and Task workers are finite.

The typed-data generator constructs schemas and values together. Neither it nor
the negative-case generators filter candidates through FScript to decide which
expectations to retain. Roundtrips are paired with independent writers, document
models, or standard byte expectations. Runtime comparisons retain value tags,
so an integer and an integral float cannot compare equal merely because they
format the same way.

## Shrinking and replay

The shared runner reports only the final counterexample to NUnit. Assertions
inside properties throw ordinary exceptions, avoiding NUnit failure records for
each intermediate shrinking attempt. Shrinkers remove list entries and optional
operations, simplify numbers and text, and reduce codec trees while preserving
homogeneous collections and the XML record root.

Failure reports include generator version 1, original and reduced inputs, the
actual discrepancy or exception, and a command like:

```sh
FSCRIPT_RUNTIME_REPLAY='3137673393014453711,17132616515343599983,0' dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release --filter 'FullyQualifiedName~CodecPropertyTests.JsonRejectsNonFiniteNumbers'
```

The fields are seed, odd gamma, and size. All three replay the single failing
generation step and its shrinking. Omitting the size starts a complete campaign
with that seed. Both fixed-seed variants of a selected NUnit method use the
override. Unset `FSCRIPT_RUNTIME_REPLAY` to restore normal execution.

This variable is independent of `FSCRIPT_MAILBOX_REPLAY` and
`FSCRIPT_INFERENCE_REPLAY`. Replay requires the same generator and FsCheck
version. Preserve discovered failures as regression cases; the non-finite JSON
regression also runs through both compiled targets and SDK forwarding.

Harness tests inject a failure, verify its reduction to a singleton zero, and
check that replay regenerates both original and reduced arguments. Run all
repository checks with `make build`, `make test`, `make smoke-tests`, and
`make verify-changelog` before proposing a change.
