# Recorded comparison

Measured on 2026-10-25, Apple M3 / Arm64, macOS 27.0.1, .NET SDK 10.0.401, runtime 10.0.12. Compiler baseline: `668b4d8` (FScript 0.85.0), with only this benchmark added. No backend optimization changes were made for the initial comparison. The follow-up below measures the capture and record optimizations added afterward.

Five exports matched the equivalent F# programs across 32 validation seeds. Every measured batch checksum also matched across implementations. All closed generated methods passed CLR JIT preparation; the generated library references only System.Private.CoreLib and System.Linq, with no FScript compiler or interpreter dependency.

## Non-tiered optimized JIT

Three independent processes, each with 500 invocations per batch and 9 batches per variant. Tiered compilation and ReadyToRun disabled for both languages. The table gives the median of the three process medians, their range, and bytes allocated per invocation. Allocation is consistent across the three processes after rounding.

```sh
make benchmark-dotnet-comparison output=out/benchmarks/dotnet-vs-fsharp-run1
# Repeat with run2 and run3 output directories.
```

| Workload | Implementation | ms / call | Process-median range (ms) | Allocated bytes / call |
| --- | --- | ---: | ---: | ---: |
| `numeric` | fscript | 0.34971 | 0.34851–0.34977 | 192 |
| `numeric` | fsharp-lists | 0.36198 | 0.36143–0.36547 | 24 |
| `pipeline` | fscript | 0.10432 | 0.10063–0.10471 | 921,827 |
| `pipeline` | fsharp-lists | 0.11000 | 0.10564–0.11006 | 636,226 |
| `pipeline` | fsharp-arrays | 0.04765 | 0.04657–0.04783 | 309,791 |
| `pipeline_nominal` | fscript | 0.13945 | 0.13492–0.13975 | 1,161,899 |
| `pipeline_nominal` | fsharp-lists | 0.11063 | 0.10596–0.11091 | 636,226 |
| `pipeline_nominal` | fsharp-arrays | 0.04839 | 0.04719–0.04858 | 309,791 |
| `routing` | fscript | 0.08254 | 0.08046–0.08303 | 606,080 |
| `routing` | fsharp-lists | 0.12327 | 0.11267–0.12494 | 565,360 |
| `routing` | fsharp-arrays | 0.08630 | 0.08509–0.08715 | 325,384 |
| `sorting` | fscript | 0.08545 | 0.08267–0.08743 | 455,753 |
| `sorting` | fsharp-lists | 0.06896 | 0.06656–0.06942 | 355,818 |
| `sorting` | fsharp-arrays | 0.05875 | 0.05868–0.06094 | 267,060 |

FScript is close to F# on the numeric recurrence. Routing is faster than the F# list version and close to the array version, while allocating substantially more than arrays. The record pipeline is over twice as slow as F# arrays, and quicksort is slower than both F# baselines. There is no general performance winner here.

The explicitly nominal pipeline is a diagnostic variant, not a proposed optimization: it allocates more and runs slower than the ordinary pipeline despite returning the same result. Annotations alone do not remove the backend's representation adaptations.

Source-to-library FScript compilation took 3.62–3.63 seconds in these optimized-JIT processes, excluded from execution timing. F# build time was not measured, so this is not a compilation-speed comparison.

## Default runtime policy

A second set of three processes used the same 500 × 9 protocol, with `DOTNET_TieredCompilation` and `DOTNET_ReadyToRun` unset. No concurrent build or test processes ran during either set of measurements. Allocations match the preceding table.

```sh
env -u DOTNET_TieredCompilation -u DOTNET_ReadyToRun dotnet benchmarks/FScript.DotNet.Comparison/bin/Release/net10.0/FScript.DotNet.Comparison.dll 500 9 out/benchmarks/dotnet-vs-fsharp-default1
# Repeat with default2 and default3 output directories.
```

| Workload | Implementation | ms / call | Process-median range (ms) |
| --- | --- | ---: | ---: |
| `numeric` | fscript | 0.32391 | 0.32389–0.32408 |
| `numeric` | fsharp-lists | 0.32354 | 0.32350–0.32383 |
| `pipeline` | fscript | 0.09200 | 0.09190–0.09240 |
| `pipeline` | fsharp-lists | 0.10642 | 0.10504–0.10647 |
| `pipeline` | fsharp-arrays | 0.04406 | 0.04391–0.04417 |
| `pipeline_nominal` | fscript | 0.12366 | 0.12363–0.12419 |
| `pipeline_nominal` | fsharp-lists | 0.10675 | 0.10498–0.10679 |
| `pipeline_nominal` | fsharp-arrays | 0.04458 | 0.04450–0.04461 |
| `routing` | fscript | 0.07490 | 0.07470–0.07636 |
| `routing` | fsharp-lists | 0.11273 | 0.11157–0.11317 |
| `routing` | fsharp-arrays | 0.08152 | 0.07888–0.08260 |
| `sorting` | fscript | 0.05456 | 0.05443–0.05468 |
| `sorting` | fsharp-lists | 0.06319 | 0.06304–0.06433 |
| `sorting` | fsharp-arrays | 0.04741 | 0.04697–0.04757 |

The numeric kernels are effectively tied under the default policy. FScript quicksort now beats the F# list version, while still trailing F# arrays. The pipeline and routing comparisons keep the same broad shape. These changes occur with identical IL and allocation counts, showing why compiler IL counts alone cannot predict runtime performance. This experiment does not isolate which JIT feature accounts for the changes.

FScript compilation takes 1.35–1.42 seconds under this policy; that work is still excluded from execution timing. Raw reports are in `out/benchmarks/dotnet-vs-fsharp-default{1,2,3}/`.

## Original IL observations

- Both numeric kernels contain a backward branch and no own-assembly direct-call cycle: their tail recursion becomes loops. Both retain one captured-function invocation site in the loop.
- The FScript numeric loop (`NativeFunction1`) contains 3 multiplication instructions; the F# loop (`Lists::loop@20`) contains 5. Repeated arithmetic has been shared in FScript's IL. Those counts exclude the captured step function, which has another multiplication in each implementation. The JIT can still optimize F#'s repeated operations; the IL difference does not establish a machine-code advantage.
- FScript allocates 192 bytes per numeric invocation versus 24 for F#. Closure scaffolding remains even though the recursion itself becomes a loop.
- The pipeline IL constructs named `v_Event` and structural `Shape_...` record representations. Extra record/array adaptations are visible alongside the higher measured allocation. The explicitly nominal version adds about 240 KB per invocation, so it exposes another conversion cost rather than fixing it.
- Quicksort retains a direct-call cycle in all three implementations. FScript's cycle crosses lifted helpers: checking only immediate self-calls would incorrectly suggest recursion had disappeared. The tuple accumulator and intermediate immutable collections remain allocation candidates.
- `UnusedPayload` is absent from the generated FScript types and present in the F# library. This demonstrates the backend's generated-member pruning; it is not a comparison with a trimmed or AOT-published F# executable.

FScript's DLL is 18,944 bytes and the F# reference DLL is 28,672 bytes. The latter contains both list and array variants, and both sizes exclude dependencies. These sizes do not support a fair binary-size ranking.

The initial comparison identified redundant nominal/structural conversions, closure scaffolding, and collection/tuple allocations. The follow-up implements the first two priorities without changing the workload algorithms.

## Artifacts and limits

Local raw data and resolved IL are under `out/benchmarks/dotnet-vs-fsharp-run{1,2,3}/`. They are generated artifacts, excluded from Git; the source programs, harness, and this summary are checked in. Source SHA-256: `9AB955DC2E4AE296C18D474052D8D4066E89B3408741400F272FDC9359EECB48`.

These are warmed synchronous microbenchmarks on one development machine, without statistical confidence intervals. Source equivalence does not imply identical representation. The IL inspection excludes FSharp.Core/BCL implementations and unknown delegate targets; see [the methodology](./README.md). No runtime, startup, AOT, FFI, task, or whole-application superiority is established.


## Capture and record optimization follow-up

The capture milestone (`01d548c`) specializes known captured functions and removes dead recursive closure environments after tail-loop lowering. The subsequent record pass scalar-replaces internal temporaries and builds compatible return shapes directly. Workload source and algorithms are unchanged.

Three paired processes on the same machine compare the saved original and optimized DLLs, alongside F#. Each process checks 32 seeds, JIT-prepares every closed generated method, warms each variant, then rotates/reverses 500-call batches over 9 rounds. This uses the compiled measurement loop through `CompareArtifacts.fsx`, with `DOTNET_gcServer=0` (verified workstation GC) and no tiered-compilation/ReadyToRun overrides. Both DLLs run under the same host and policy. These paired measurements supersede comparisons between separate historical timing runs; compilation is excluded.

```sh
DOTNET_gcServer=0 dotnet fsi benchmarks/FScript.DotNet.Comparison/CompareArtifacts.fsx \
  out/benchmarks/dotnet-vs-fsharp-run1/FScript.Benchmark.Workloads.dll \
  out/benchmarks/optimized-run1/FScript.Benchmark.Workloads.dll \
  out/benchmarks/paired-workstation-1
# Repeat with paired-workstation-2 and paired-workstation-3.
```

The table gives the median of process medians, their range, and allocation per call. Allocations agree across all three processes after rounding.

| Workload | Implementation | ms / call | Process-median range (ms) | Allocated bytes / call |
| --- | --- | ---: | ---: | ---: |
| `numeric` | before | 0.33737 | 0.32462–0.34170 | 192 |
| `numeric` | after | 0.32445 | 0.32399–0.32468 | 0 |
| `numeric` | fsharp-lists | 0.32975 | 0.32423–0.33252 | 24 |
| `pipeline` | before | 0.09321 | 0.09268–0.09493 | 921,827 |
| `pipeline` | after | 0.04891 | 0.04865–0.05123 | 521,731 |
| `pipeline` | fsharp-lists | 0.10401 | 0.10360–0.10904 | 636,226 |
| `pipeline` | fsharp-arrays | 0.04366 | 0.04344–0.04541 | 309,791 |
| `pipeline_nominal` | before | 0.12438 | 0.12426–0.12941 | 1,161,899 |
| `pipeline_nominal` | after | 0.07758 | 0.07696–0.07955 | 761,803 |
| `pipeline_nominal` | fsharp-lists | 0.10427 | 0.10297–0.10913 | 636,226 |
| `pipeline_nominal` | fsharp-arrays | 0.04364 | 0.04358–0.04563 | 309,791 |
| `routing` | before | 0.07341 | 0.07326–0.07706 | 606,080 |
| `routing` | after | 0.06164 | 0.05925–0.06322 | 456,744 |
| `routing` | fsharp-lists | 0.10925 | 0.10603–0.11332 | 565,360 |
| `routing` | fsharp-arrays | 0.08079 | 0.07736–0.08382 | 325,384 |
| `sorting` | before | 0.05229 | 0.05216–0.05402 | 455,753 |
| `sorting` | after | 0.05181 | 0.05143–0.05376 | 455,753 |
| `sorting` | fsharp-lists | 0.07971 | 0.05900–0.08137 | 355,818 |
| `sorting` | fsharp-arrays | 0.04536 | 0.04512–0.04593 | 267,060 |

The record pipeline takes about 48% less time (1.9× throughput) and allocates 43% fewer bytes. The explicit-nominal pipeline takes about 38% less time and allocates 34% fewer bytes. Routing takes about 16% less time and allocates 25% fewer bytes. Numeric allocation falls from 192 to zero bytes per call; its timing ranges overlap, so the clear result there is allocation and delegate elimination. Quicksort allocation is unchanged and its timing ranges overlap.

The numeric export's reachable generated methods have no delegate calls or `newobj` instructions and retain a loop backedge. Record update/projection/width-conversion regression tests also execute without allocating containers. Pipeline arrays, final record values, and some boundary adaptations remain: the nominal pipeline still allocates about 240 KB more than the structural version. The optimized pipeline remains about 12% slower than the F# array reference here and allocates more, despite improving substantially. No pipeline fusion or collection algorithm rewrite was introduced.

The original DLL is 18,944 bytes; the optimized DLL is 13,824 bytes, excluding dependencies. Their SHA-256 hashes are `33AA0D82715850866D167D836873280EA6D26CDAA46A8C00AABBCFA868A4CFFD` and `F9204B0F610175E1F006B0D846D71BD73ECB02F504F5D10F789589D27F73EA0C`. Both saved sources match the source hash above. Raw paired reports are under `out/benchmarks/paired-workstation-{1,2,3}/`; resolved optimized IL and the standalone run are under `out/benchmarks/optimized-run1/`.

These remain development-machine microbenchmarks, not confidence intervals or whole-application performance guarantees. The FSI host is explicit in this follow-up; its default server-GC configuration differs from the standalone harness, so workstation GC is selected and recorded rather than silently mixing those policies.


## Collection, tuple, and list-view milestones

Starting at the previous committed milestone (`dbf288f`), this follow-up adds representation propagation (`ec1b82c`), pure pipeline fusion (`9badc9d`), tuple scalar replacement/value carriers (`d5a355e`), and private list views with direct span copies. No workload source or algorithm was changed. Fusion is deliberately limited to proven-pure, total regions; side effects and possible failures retain their stage order.

Three new paired processes use the same workstation-GC artifact runner, 32 validation seeds, 500 invocations per batch and 9 rounds. `before` is the saved output of `dbf288f`; `after` includes all four optimizations. Both artifacts and F# references run under the same process settings. The table reports medians of process medians, their range, and bytes per call.

```sh
DOTNET_gcServer=0 dotnet fsi benchmarks/FScript.DotNet.Comparison/CompareArtifacts.fsx \
  out/benchmarks/optimized-run1/FScript.Benchmark.Workloads.dll \
  out/benchmarks/all4-optimized/FScript.Benchmark.Workloads.dll \
  out/benchmarks/paired-collections-1
# Repeat with paired-collections-2 and paired-collections-3.
```

| Workload | Implementation | ms / call | Process-median range (ms) | Allocated bytes / call |
| --- | --- | ---: | ---: | ---: |
| `numeric` | before | 0.32403 | 0.32365–0.32479 | 0 |
| `numeric` | after | 0.32389 | 0.32368–0.32434 | 0 |
| `numeric` | fsharp-lists | 0.32333 | 0.32318–0.32546 | 24 |
| `pipeline` | before | 0.04909 | 0.04909–0.05014 | 521,731 |
| `pipeline` | after | 0.00870 | 0.00812–0.00870 | 0 |
| `pipeline` | fsharp-lists | 0.10417 | 0.10303–0.10725 | 636,226 |
| `pipeline` | fsharp-arrays | 0.04378 | 0.04354–0.04531 | 309,791 |
| `pipeline_nominal` | before | 0.07965 | 0.07724–0.08143 | 761,803 |
| `pipeline_nominal` | after | 0.00869 | 0.00812–0.00870 | 0 |
| `pipeline_nominal` | fsharp-lists | 0.10432 | 0.10323–0.10722 | 636,226 |
| `pipeline_nominal` | fsharp-arrays | 0.04360 | 0.04359–0.04573 | 309,791 |
| `routing` | before | 0.06186 | 0.06124–0.06353 | 456,744 |
| `routing` | after | 0.04027 | 0.03798–0.04882 | 245,336 |
| `routing` | fsharp-lists | 0.11822 | 0.10871–0.11991 | 565,360 |
| `routing` | fsharp-arrays | 0.07865 | 0.07756–0.07924 | 325,384 |
| `sorting` | before | 0.05276 | 0.05204–0.05356 | 455,753 |
| `sorting` | after | 0.04583 | 0.04455–0.04598 | 315,108 |
| `sorting` | fsharp-lists | 0.06682 | 0.05893–0.06979 | 355,818 |
| `sorting` | fsharp-arrays | 0.04501 | 0.04464–0.04649 | 267,060 |

The record pipeline has about 5.6× throughput versus the previous milestone; the nominal variant about 9.2×. Both now allocate zero bytes per call: compatible representations flow through the pipeline, the range feeds a bounded loop, and mapped records are scalar-replaced. The F# array reference retains its map/filter/fold arrays and records, so this result measures an optimization difference as well as code generation.

Routing has about 1.5× throughput and allocates 46% fewer bytes. Its intermediate arrays disappear, while tagged union values and payload records remain. Quicksort has about 1.15× throughput and allocates 31% fewer bytes. It retains its recursive algorithm; tuple accumulators use value carriers, tails use views, and native cons/append copy directly into result buffers without singleton/iterator intermediates. Its timing is close to F# arrays, with overlapping process-median ranges, but it still allocates more (315 KB versus 267 KB). Numeric timing and allocation remain effectively unchanged.

Additional regressions demonstrate zero-allocation traversal of a cached 1,000-element list, owned array boundaries for exports/callbacks, ordered field/operand effects, retained failures, and captured-array representation consistency. Independent ILVerify accepts every class and method in the generated benchmark assembly, including the generic view runtime. Every closed generated method also passes CLR JIT preparation. The view uses a normal struct enumerator rather than returning a span enumerator, preserving independent verifiability.

The new DLL is 15,872 bytes versus 13,824 before: runtime view helpers trade some code size for reduced execution allocations. Its SHA-256 is `662BFFC2CB9D2AFA2E189FD107F31E4C0A627CCC9564E357A04531F931B0241C`; the source hash is unchanged. Raw data is under `out/benchmarks/paired-collections-{1,2,3}/`, and the new IL/DLL under `out/benchmarks/all4-optimized/`. These are warmed development-machine measurements, not whole-application or AOT performance guarantees.


## Compact-mask filters

Measured on 2026-10-26 with the same machine, runtime and paired-artifact methodology. The saved `b745687` output is `before`; `after` replaces growing list buffers in materialized filters with compact selection masks and exactly sized output arrays. Predicates run once, in order, before the copy pass. Masks are built a word at a time; native workers cache a local readonly span and visit selected bits using `BitOperations.TrailingZeroCount`. Arrays and private list views share this strategy, and unknown callbacks use typed runtime helpers. FScript syntax, public IR and the interpreter are unchanged.

Three independent paired processes use workstation GC and the default tiered compilation/ReadyToRun policy, 500 invocations per batch, 9 rounds, and 32 correctness seeds. No builds, other tests or diagnostic tracing ran concurrently with timing. The table reports medians of process medians and their range.

```sh
DOTNET_gcServer=0 dotnet fsi benchmarks/FScript.DotNet.Comparison/CompareArtifacts.fsx \
  out/benchmarks/all4-optimized/FScript.Benchmark.Workloads.dll \
  out/benchmarks/mask-filter-final/FScript.Benchmark.Workloads.dll \
  out/benchmarks/paired-mask-final-1
# Repeat with paired-mask-final-2 and paired-mask-final-3.
```

| Workload | Implementation | ms / call | Process-median range (ms) | Allocated bytes / call |
| --- | --- | ---: | ---: | ---: |
| `numeric` | before | 0.32406 | 0.32399–0.32478 | 0 |
| `numeric` | after | 0.32409 | 0.32386–0.32480 | 0 |
| `numeric` | fsharp-lists | 0.32349 | 0.32338–0.33247 | 24 |
| `pipeline` | before | 0.00871 | 0.00822–0.00872 | 0 |
| `pipeline` | after | 0.00871 | 0.00826–0.00871 | 0 |
| `pipeline` | fsharp-lists | 0.10452 | 0.10445–0.10929 | 636,226 |
| `pipeline` | fsharp-arrays | 0.04385 | 0.04371–0.04548 | 309,791 |
| `pipeline_nominal` | before | 0.00871 | 0.00871–0.00872 | 0 |
| `pipeline_nominal` | after | 0.00871 | 0.00870–0.00871 | 0 |
| `pipeline_nominal` | fsharp-lists | 0.10484 | 0.10430–0.10922 | 636,226 |
| `pipeline_nominal` | fsharp-arrays | 0.04369 | 0.04351–0.04532 | 309,791 |
| `routing` | before | 0.03776 | 0.03741–0.03950 | 245,336 |
| `routing` | after | 0.03873 | 0.03789–0.04050 | 245,336 |
| `routing` | fsharp-lists | 0.11359 | 0.10931–0.11823 | 565,360 |
| `routing` | fsharp-arrays | 0.08178 | 0.08020–0.08966 | 325,384 |
| `sorting` | before | 0.04708 | 0.04669–0.04821 | 315,108 |
| `sorting` | after | 0.02603 | 0.02554–0.03483 | 163,544 |
| `sorting` | fsharp-lists | 0.06975 | 0.06847–0.07343 | 355,818 |
| `sorting` | fsharp-arrays | 0.04712 | 0.04546–0.05547 | 267,060 |

Quicksort takes about 45% less time (1.8× throughput) and allocates 48% fewer bytes than the saved compiler output. It also has about 1.8× throughput and 39% fewer allocated bytes than this F# array reference. The before/after process-median timing ranges do not overlap. The other workloads retain their allocation counts and have overlapping timing ranges; their pure pipelines already eliminate materialized filters.

This isolates a filter implementation change in FScript: quicksort, input generation, tail views and concatenation order are unchanged. The F# array reference still has a copying tail and a different concatenation association, so this is a whole-workload comparison rather than an isolated ranking of filter implementations or compilers. The mask strategy trades compact decision storage and a second traversal for avoiding growable element buffers. It does not guarantee lower allocation for every input or selection density.

An initial mask implementation produced the same allocation reduction but mixed timing results. Separate optimized ARM64 JIT inspection exposed repeated view bounds checks and mask accesses. Caching a local span and processing masks by word produced the final results above. Diagnostic disassembly ran separately with tiering disabled; those runs are not included in the timing table. Independently verified helper interfaces expose arrays/offsets rather than returning spans.

All five exports match their applicable F# references across the 32 seeds, and every closed generated method passes CLR JIT preparation. Independent ILVerify accepts all classes and methods of the final benchmark DLL. Regression tests cover empty/all/none selections, 31/32/33 and 64/65-element boundaries, known and unknown callbacks, effect ordering, first predicate failure and the exact-output allocation budget. The full solution tests, smoke tests and warning-free build pass; the website documentation build and version checks also pass.

The final DLL is 16,896 bytes versus 15,872 before. Its SHA-256 is `7BCE6E05749049A10D21B49BD06A59022832940EF76E206AD355EA4A85898D6B`; the before hash is `662BFFC2CB9D2AFA2E189FD107F31E4C0A627CCC9564E357A04531F931B0241C`. The workload source hash is unchanged. Raw reports are under `out/benchmarks/paired-mask-final-{1,2,3}/`; the final DLL and IL are under `out/benchmarks/mask-filter-final/`. Results are warmed managed .NET development-machine measurements, not Native AOT or whole-application guarantees.
