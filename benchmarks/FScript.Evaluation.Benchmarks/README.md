# Evaluator benchmark

Measure normal execution without a debugger. Each workload is parsed and inferred
once, warmed for at least 500 ms and 20 evaluations, then measured in five batches.
The output reports median milliseconds and allocated bytes per evaluation. Timing
assertions are deliberately excluded from automated tests.

```sh
make benchmark-evaluation iterations=20 size=10000
```

For a comparison with a published engine, extract its NuGet package and compile
this same runner against its `FScript.Language.dll`. Build both runners before
measuring and run them sequentially, alternating their order across repetitions:

```sh
dotnet build benchmarks/FScript.Evaluation.Benchmarks -c Release -o /tmp/eval-current
dotnet build benchmarks/FScript.Evaluation.Benchmarks -c Release \
  -p:LanguageAssembly=/absolute/path/to/published/FScript.Language.dll -o /tmp/eval-baseline

dotnet /tmp/eval-baseline/FScript.Evaluation.Benchmarks.dll 20 10000
dotnet /tmp/eval-current/FScript.Evaluation.Benchmarks.dll 20 10000
```

Use the same .NET runtime, architecture, and environment for both processes. Avoid
other builds or benchmark processes while measuring. `DOTNET_TieredCompilation=0`
can isolate optimized execution from tiered compilation; report this setting when
using it. CLI startup is a separate measurement: time the published and current
managed CLI DLLs with the same `dotnet` executable and input script. Do not compare
a native apphost with a managed DLL, or include compilation in the timing.
