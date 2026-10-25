# Static task-sharing analysis

Task-sharing analysis is an opt-in, warning-only frontend pass over a resolved,
typed program. It does not run the script. The existing AST interpreter, LIR
interpreter, generated programs, and task scheduler contain no tracking hooks.
Normal inference and execution do not invoke the pass or allocate its state.

## Entry points

```sh
fscript analyze program.fss
fscript analyze program.fss --config fscript.toml
```

`analyze` parses and type-checks the script, resolves imports and its configured
extern environment, and reports task-sharing warnings. It never evaluates script
initializers or invokes registered FFI implementations. Loading a configured
provider can still execute its registration factory and metadata methods, exactly
as other environment-loading commands do; this is not a sandbox for untrusted
providers. `--config`, `--no-config`, `--root`, `--no-default-externs`, and repeated
`--extern-assembly` flags use the ordinary CLI environment rules.

Warnings go to stderr and do not cause a nonzero exit code. Parse, type,
configuration, and filesystem errors do. Analysis is a separate command; compilation
does not run the pass or discover concurrency metadata. Check a script with
`analyze` before compiling when warnings are wanted.

## Diagnostics

- `FST1001`: an external instance may be accessed or captured by concurrent tasks,
  and its type has no shareability guarantee.
- `FST1002`: a host function may be called by concurrent tasks, and its hidden
  state has no concurrency guarantee. This can occur without instance arguments.

Each warning carries a primary source span and a related access/capture span.
These are potential-sharing warnings, not proof of a data race. Safe programs may
warn, and absence of a warning does not establish safety.

## Analysis model and limits

The pass follows lexical symbol identities, aliases, closure captures, known
FScript function applications (including partial application), aggregate contents,
and pattern bindings. Host-function results can retain argument aliases and can
alias other results from the same binding. Without a freshness contract,
independence of factory results is not assumed.

Logical task clocks model `Task.spawn` and straightforward `Task.await` ordering,
including aliases of task handles. Parent access before a fork or after a proven
await is ordered. Parent access while a task may run and sibling task accesses
can warn. Ordering facts from conditional branches survive only when established
in every branch; mutually exclusive branches are not compared with one another.

This is a bounded, best-effort analysis, not a whole-program concurrency proof.
Loops are analyzed for two symbolic iterations to reveal repeated spawn sites;
recursive function expansion stops at an active recursive call. Collections and
unknown results use coarse unions of possible aliases. Unknown callbacks,
external threads, host scheduling, hidden global state shared across different
FFI bindings, and aliases invisible to FScript cannot be fully modeled. Arbitrary
FFI callback execution and synchronization inside the host are not inferred.
Top-level function entry points are analyzed independently; unrelated host
invocations are not assumed concurrent.

## Trusted provider contracts

Providers may expose public static parameterless companions returning
`IEnumerable<string>` of registered script names:

```csharp
public static IEnumerable<string> FScriptThreadSafeTypes() => new[] { "Client" };
public static IEnumerable<string> FScriptThreadSafeFunctions() => new[] { "Host.log" };
```

A thread-safe type promises that its instances can be shared and their supported
operations can run concurrently. A thread-safe function promises its execution,
including initialization and hidden state, is safe for concurrent invocation.
These are trusted guarantees; incorrect declarations can hide actual problems.
Unknown contract names are errors. No SDK registration shape changes are needed.
Companions are read only when analysis is explicitly requested.

Purity and concurrency remain separate concepts. `PureTypes` implies immutable,
shareable instances. `PureFunctions` exempts the function's own hidden effects,
but does not make its arguments' types shareable. Thread-safe functions need not
be pure: logging can be thread-safe and still observable. A function contract does
not suppress an independent warning about an unclassified captured instance.

## .NET API and hosting

`TaskSharingAnalysis.analyze registration options resolved` returns a list of
`Diagnostic` records. `TaskSharingAnalysis.defaults` contains no thread-safety
claims. Options contain `ThreadSafeTypes` and `ThreadSafeFunctions` sets; resolved
program purity contracts are also honored. The pass never calls FFI implementations.

Hosts can request analysis before interpreting:

```fsharp
let loaded =
    ScriptHost.loadSourceWithTaskSharingAnalysis
        registration
        PurityContracts.empty
        TaskSharingAnalysis.defaults
        (fun warning -> hostLog warning.Message)
        source
```

The callback receives all diagnostics before evaluation starts. The host controls
warning display and may reject execution by throwing from the callback. File and
already-parsed entry points are `loadFileWithTaskSharingAnalysis` and
`loadProgramWithTaskSharingAnalysis`. The latter also accepts programs resolved
through a host's include resolver. Analysis and evaluation share one inference
result. Existing `ScriptHost` load/invoke APIs remain unchanged and skip analysis.
There is no runtime instrumentation or change to evaluation order in either mode.

`TaskSharingContracts.fromProviders` in Runtime reads thread-safety metadata from
provider `MethodInfo` values when explicitly called. Hosting does not need a
compiler dependency.

Runtime's `TaskSharingContracts.withRuntime` recognizes the actual built-in
`Console.writeLine` and `Console.readLine` implementation delegates as thread-safe.
The CLI and opt-in ScriptHost loaders apply these facts automatically; direct
Language analyzer callers can explicitly apply the Runtime helper. Names alone
never grant this exemption: a provider's replacement console implementation needs
its own contract. Console calls remain effectful, and task I/O ordering remains
unspecified. No purity contracts or optimizer behavior are changed.
