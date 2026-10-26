# Interpreter optimization results

Measured on 2026-10-26, Apple M3 (arm64), .NET 10.0.12, SDK 10.0.401,
Release builds, `DOTNET_TieredCompilation=0`, with other runtime settings unchanged.
Baseline interpreter implementations are from commit `7e69b2d`; current uses the
accompanying working-tree changes. Frontend preparation is outside timed regions.
Both runners were built before measurement and run sequentially without builds
or tests, in baseline/current, current/baseline, then baseline/current order. Inputs contain 2,000
items (recursion uses `fib 20`). Each process measures five operations per batch;
the table takes the median of three process medians (five batches per process).
Allocation values are bytes per complete script execution.

Changes:

- LIR `List.collect` retains callback order, validates results, and concatenates chunks once rather than repeatedly copying the growing output.
- LIR `List.iter` discards callback results without constructing a result list.
- AST record/map equality checks matching keys without constructing temporary sets, including equality used by `List.distinct`.
- AST `List.choose` builds selected values directly while preserving debugger suspension and first-failure behavior.

| Workload/engine | Before ms | After ms | Speedup | Before bytes | After bytes |
| --- | ---: | ---: | ---: | ---: | ---: |
| map/ast | 0.4574 | 0.4600 | 0.99x | 1,614,406 | 1,614,406 |
| map/lir | 0.1710 | 0.1697 | 1.01x | 925,278 | 925,278 |
| iter/ast | 0.7716 | 0.7665 | 1.01x | 2,604,830 | 2,604,830 |
| iter/lir | 0.3463 | 0.3282 | 1.06x | 1,262,366 | 1,134,366 |
| choose/ast | 0.5328 | 0.5077 | 1.05x | 1,662,414 | 1,581,398 |
| choose/lir | 0.2799 | 0.2798 | 1.00x | 1,356,182 | 1,356,182 |
| collect/ast | 0.7336 | 0.7304 | 1.00x | 2,222,350 | 2,222,350 |
| collect/lir | 27.6151 | 0.2976 | 92.79x | 129,182,782 | 1,502,718 |
| fold/ast | 0.6677 | 0.6667 | 1.00x | 2,333,854 | 2,333,854 |
| fold/lir | 0.4281 | 0.4258 | 1.01x | 1,825,790 | 1,825,790 |
| loop/ast | 0.5464 | 0.5452 | 1.00x | 1,931,966 | 1,931,966 |
| loop/lir | 0.0533 | 0.0536 | 0.99x | 358,070 | 358,070 |
| curried/ast | 1.1892 | 1.1843 | 1.00x | 3,534,902 | 3,534,902 |
| curried/lir | 0.6360 | 0.6308 | 1.01x | 2,644,214 | 2,644,214 |
| records/ast | 1.0025 | 0.9982 | 1.00x | 2,911,742 | 2,911,742 |
| records/lir | 0.4660 | 0.4625 | 1.01x | 1,697,550 | 1,697,550 |
| record-equality/ast | 1.7952 | 0.9947 | 1.80x | 6,223,246 | 3,119,246 |
| record-equality/lir | 0.6131 | 0.6155 | 1.00x | 2,558,606 | 2,558,606 |
| map-equality/ast | 1.2714 | 0.8031 | 1.58x | 4,302,406 | 2,446,406 |
| map-equality/lir | 0.4091 | 0.4067 | 1.01x | 1,693,822 | 1,693,822 |
| recursion/ast | 7.0794 | 7.0711 | 1.00x | 13,337,942 | 13,337,942 |
| recursion/lir | 4.3479 | 4.3555 | 1.00x | 15,249,550 | 15,249,550 |

This compares before/after within each engine, not engine throughput in isolation:
AST execution uses `Eval.evalProgram`; LIR execution uses `LirInterpreter.execute`
and includes validation and runtime initialization. Parsing, inference, and LIR
compilation are excluded. Recursion, map, fold, curried calls, and record construction
are controls; small timing differences in unchanged workloads are measurement noise.

One current process reported `fold/ast` at 1.1129 ms; its other two runs were
0.6667 and 0.6634 ms, consistent with baseline runs at 0.6626–0.6704 ms.
All three processes are included in the median above.

These synchronous synthetic workloads do not measure task scheduling, FFI latency,
loaded-program invocation, cold startup, or debugger overhead. Callback order,
selected/discarded values, empty chunks, first-failure behavior, map key/value
semantics, and debugger pause/resume are covered by regression tests. Existing
host-managed value tests retain coverage of visible-field comparisons.

Reproduce with [README.md](README.md). Raw local run outputs are under
`.out/performance-interpreter/*-final-r*.txt` (ignored build artifacts).
