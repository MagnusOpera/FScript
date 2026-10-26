# Frontend optimization results

Measured on 2026-10-26, Apple M3 (arm64), .NET 10.0.12, SDK 10.0.401,
Release builds, `DOTNET_TieredCompilation=0`, with other runtime settings unchanged.
Baseline engine: commit `7e69b2d`; current: the accompanying working-tree changes.
Both binaries were built before measurement and run sequentially without builds
or tests, in baseline/current then current/baseline order. Each process ran five
operations per batch after warmup; the table takes the median of two process
medians (seven batches per process). Allocation values are bytes per operation.

The parser now indexes an array rather than a linked token list. Resolved inference
indexes symbols and reference occurrences instead of repeatedly scanning global
lists. Tuple inference appends once in source order after reverse accumulation.
Task-sharing closure capture looks up referenced symbols rather than filtering the
whole environment. Public result ordering, spans, and lexer APIs are retained.

| Size | Workload/phase | Before ms | After ms | Speedup | Before bytes | After bytes |
| ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 200 | bindings/lex | 0.1125 | 0.1149 | 0.98x | 547,678 | 547,678 |
| 200 | bindings/parse | 18.8216 | 0.1612 | 116.76x | 710,462 | 652,822 |
| 200 | bindings/infer | 0.1589 | 0.1599 | 0.99x | 651,454 | 651,454 |
| 200 | bindings/resolve | 3.3617 | 1.2944 | 2.60x | 4,367,814 | 2,504,446 |
| 200 | bindings/analyze | 0.2261 | 0.1439 | 1.57x | 618,430 | 610,430 |
| 200 | bindings/compile | 4.4206 | 2.2850 | 1.93x | 7,246,766 | 5,384,278 |
| 200 | tuple/lex | 0.0192 | 0.0200 | 0.96x | 86,486 | 86,486 |
| 200 | tuple/parse | 0.5189 | 0.0268 | 19.40x | 112,094 | 102,334 |
| 200 | tuple/infer | 0.1341 | 0.0178 | 7.53x | 718,246 | 87,846 |
| 200 | tuple/resolve | 0.2518 | 0.1477 | 1.71x | 1,049,702 | 429,294 |
| 200 | tuple/analyze | 0.0503 | 0.0507 | 0.99x | 193,006 | 193,006 |
| 200 | tuple/compile | 0.3683 | 0.2686 | 1.37x | 1,451,350 | 831,822 |
| 1000 | bindings/lex | 1.0221 | 1.0243 | 1.00x | 2,670,734 | 2,670,734 |
| 1000 | bindings/parse | 491.7462 | 1.2306 | 399.60x | 3,479,454 | 3,191,414 |
| 1000 | bindings/infer | 1.0785 | 1.0694 | 1.01x | 3,430,382 | 3,430,382 |
| 1000 | bindings/resolve | 59.4430 | 9.3696 | 6.34x | 60,963,806 | 13,203,886 |
| 1000 | bindings/analyze | 2.9620 | 0.8305 | 3.57x | 3,144,046 | 3,104,046 |
| 1000 | bindings/compile | 68.4652 | 20.8388 | 3.29x | 87,770,094 | 40,011,056 |
| 1000 | tuple/lex | 0.0931 | 0.0925 | 1.01x | 418,310 | 418,310 |
| 1000 | tuple/parse | 12.9815 | 0.1222 | 106.19x | 539,454 | 491,294 |
| 1000 | tuple/infer | 3.3007 | 0.0771 | 42.78x | 16,353,446 | 401,446 |
| 1000 | tuple/resolve | 3.8862 | 0.7309 | 5.32x | 17,857,206 | 1,927,998 |
| 1000 | tuple/analyze | 0.2793 | 0.2798 | 1.00x | 1,089,918 | 1,089,918 |
| 1000 | tuple/compile | 4.1643 | 1.0297 | 4.04x | 18,732,454 | 2,804,126 |

These are synthetic workload results, not a universal speedup claim. `parse`
includes lexing; `infer`, `resolve`, and `compile` receive pre-parsed programs.
`analyze` receives a pre-resolved program, so its timing excludes inference.
`compile` includes inference, HIR/LIR lowering, optimization, and validation; it
excludes native backend emission, SDK publishing, and running generated code.
Task-sharing timings use ordinary function declarations; FFI sharing correctness
is checked by automated tests and sample smoke tests. Unchanged lexing and ordinary
binding inference are controls; small timing differences there are noise.

Remaining profiling candidates include structural expression-type map keys,
pattern-binding lookup, environment substitution/generalization, and native
backend optimization/emission. This run does not establish them as bottlenecks.
Use representative application scripts and allocation/CPU traces before changing them.

Reproduce with the commands in [README.md](README.md). Raw local run outputs are
under `.out/performance-front-end/*-final-r*.txt` (ignored build artifacts).
