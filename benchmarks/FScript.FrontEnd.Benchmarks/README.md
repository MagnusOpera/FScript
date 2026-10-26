# Frontend performance

Run `make benchmark-front-end iterations=5 size=200` to measure lexing, parsing
(including lexing), ordinary inference, resolved inference, task-sharing analysis,
and the complete shared compiler pipeline (inference through validated optimized
LIR). This does not measure native backend emission, SDK publishing, or execution.
Inference and compilation receive a pre-parsed program; task-sharing analysis
receives a pre-resolved program. Each operation produces a fresh result.

The runner reports median elapsed time and current-thread allocated bytes per
operation over seven batches, after at least five operations and 500 ms of warmup
per phase. These are single-threaded workloads. Timing assertions are deliberately
excluded from unit tests. `bindings` contains independent annotated integer
functions; `tuple` contains a wide integer tuple. Use several sizes to check scaling
and realistic scripts before extrapolating these synthetic results.

For a before/after comparison, build this runner before modifying engine sources,
then build the changed runner into a different directory:

```sh
dotnet build benchmarks/FScript.FrontEnd.Benchmarks -c Release -o .out/front-end/baseline
# Apply the engine changes, then:
dotnet build benchmarks/FScript.FrontEnd.Benchmarks -c Release -o .out/front-end/current
DOTNET_TieredCompilation=0 dotnet .out/front-end/baseline/FScript.FrontEnd.Benchmarks.dll 5 200
DOTNET_TieredCompilation=0 dotnet .out/front-end/current/FScript.FrontEnd.Benchmarks.dll 5 200
```

Build both runners before measuring. Use the same runtime and architecture, run
sequentially without other builds or tests, and alternate baseline/current order
across repetitions. Disabling tiered compilation here isolates optimized code;
it is a benchmark setting, not an application configuration recommendation.
See [RESULTS.md](RESULTS.md) for the measured optimization results.
