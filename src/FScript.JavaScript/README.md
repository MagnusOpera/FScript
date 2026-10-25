# @magnusopera/fscript

Embeddable FScript language engine: parser, type inference, and evaluator.

This package exposes parser, inference, evaluation, exported-function invocation, REPL-like sessions, virtual imports, and JS-provided externs for Node and browser-compatible ESM hosts.

Provide complete environments with `registration({types, unions, functions})` and pass them as `{registration: environment}`. `HostTypes.describe` declares visible fields; `scriptConstructible: true` enables ordinary script records, while the default retains an opaque JavaScript payload through `HostTypes.wrap` / `unwrap`. `HostTypes.describeUnion`, `wrapUnion`, and `unwrapUnion` support registered nominal cases. `T.named(name)` references those types in signatures.

`HostFunctions.create({name, parameters, result, invoke})` builds a typed function from explicit script types. Handlers receive tagged arguments and an invocation context with `apply` for synchronous script callbacks. Existing `extern` declarations still work. Fable task execution and Promise-returning handlers are unsupported.

See the [JavaScript hosting reference](https://fscript.magnusopera.io/manual/embedding/fable-javascript) for descriptor tables and examples. Run the local sample after building: `node examples/hosting.mjs`.

## Exit status

JavaScript treats `Env.setExitCode` as a no-op for source portability. It does not change process status or enforce a shell-code range. Arguments are still evaluated normally, and the program's returned value is unchanged. The website continues displaying that value. Existing `run`, `evaluate`, and `invoke` APIs retain their tagged-value results.
