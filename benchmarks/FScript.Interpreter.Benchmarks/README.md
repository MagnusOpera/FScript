# Interpreter performance

`make benchmark-interpreters iterations=5 size=2000` measures ordinary AST
execution and LIR execution without a debugger. Parsing, type inference, and
compilation happen once before each workload's measurements. Each timed operation
executes the whole script with a fresh runtime; the LIR path includes validation
and initialization through `LirInterpreter.execute`. This is not a benchmark of
repeated calls into a loaded program. Compare before/after within each engine.

Workloads cover mapping, iteration, selection, collection, folds, loops, curried
calls, record construction, record/map equality, and recursion. The runner warms
each operation for at least five runs and 500 ms, then reports medians of five
batches for time and current-thread allocations. Workloads are synchronous and
do not exercise task scheduling, FFI latency, or debugger overhead.

Build baseline and changed runners into separate directories before measuring:

```sh
dotnet build benchmarks/FScript.Interpreter.Benchmarks -c Release -o .out/interpreters/baseline
# Apply interpreter changes, then:
dotnet build benchmarks/FScript.Interpreter.Benchmarks -c Release -o .out/interpreters/current
DOTNET_TieredCompilation=0 dotnet .out/interpreters/baseline/FScript.Interpreter.Benchmarks.dll 5 2000
DOTNET_TieredCompilation=0 dotnet .out/interpreters/current/FScript.Interpreter.Benchmarks.dll 5 2000
```

Run sequentially, alternate runner order between repetitions, and avoid other
builds/tests during measurement. Keep runtime and machine settings identical.
Disabling tiered compilation is a benchmark setting, not an application default.
See [RESULTS.md](RESULTS.md) for measured changes.
