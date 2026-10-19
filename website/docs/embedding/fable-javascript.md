---
id: fable-javascript
title: Fable and JavaScript Hosts
slug: /embedding/fable-javascript
sidebar_label: Fable and JavaScript
---

FScript can run in JavaScript hosts through the Fable-compiled `@magnusopera/fscript` package. The package targets Node and browser-compatible ESM environments and exposes the core language pipeline without the .NET runtime extern catalog.

Use this package when a JavaScript application needs to parse, type-check, evaluate, load, or invoke FScript code directly in-process.

## Install

```bash
npm install @magnusopera/fscript
```

```javascript
import {
  T,
  extern,
  HostTypes,
  HostFunctions,
  registration,
  run,
  load,
  invoke,
  getValue,
  listFunctions,
  listValues,
  parse,
  infer,
  evaluate,
  formatValue,
  fromJs,
  toJs,
  createSession,
  resetSession,
  submit,
} from "@magnusopera/fscript";
```

## Run a script

`run` parses, infers, and evaluates a source string.

```javascript
const result = run("let x = 41\nx + 1");

console.log(result);
// { kind: "int", value: 42n }
```

FScript `int` values are returned as JavaScript `bigint`. Safe JavaScript integer numbers are accepted as input arguments.

## Load and invoke exports

Use `export let` on top-level bindings that the host should call repeatedly.

```javascript
const loaded = load(`
export let add x y = x + y
export let answer = 42
`);

const sum = invoke(loaded, "add", [1, 2]);
const answer = getValue(loaded, "answer");
```

`listFunctions(loaded)` and `listValues(loaded)` return the exported callable and value names.

## Register JavaScript externs

Externs are host functions with explicit type schemes. Arguments are passed as tagged FScript values, and the return value must be a tagged value or a supported plain JavaScript value.

```javascript
const shout = extern({
  name: "Host.shout",
  arity: 1,
  scheme: T.scheme(T.func(T.string, T.string)),
  invoke(args) {
    return { kind: "string", value: `${args[0].value}!` };
  },
});

const result = run('Host.shout "fable"', { externs: [shout] });
```

## Register host types and functions

JavaScript hosts can register opaque types, script-constructible record types, and nominal unions. These use the same interpreter rules as .NET extensions. JavaScript has no CLR reflection: provide the visible field and function types explicitly with `T`.

```javascript
import {T, HostTypes, HostFunctions, registration, run} from "@magnusopera/fscript";

const nodeType = HostTypes.describe({
  name: "Node",
  fields: {Value: T.int},
});
const typeRegistration = registration({types: [nodeType]});

const makeNode = HostFunctions.create({
  name: "Host.makeNode",
  parameters: [T.int],
  result: T.named("Node"),
  invoke([value]) {
    const state = {token: Symbol("node")};
    return HostTypes.wrap(typeRegistration, "Node", {Value: value}, state);
  },
});
const environment = registration({types: [nodeType], functions: [makeNode]});

const node = run("Host.makeNode 42", {registration: environment});
console.log(node.fields.Value.value); // 42n
const state = HostTypes.unwrap(typeRegistration, "Node", node);
```

`Node` cannot be constructed or updated by script code. Scripts can read `node.Value`, match its visible fields, and pass the original value through functions and containers. The payload is available only to the JavaScript host. Hosts may mutate it through explicit functions; visible fields remain snapshots. Equality compares visible fields recursively and ignores payloads.

`HostTypes.wrap` converts and validates an exact set of visible fields before creating a value. It copies array/record inputs into interpreter values. The returned host tag and its visible snapshot are frozen, with the payload retained in private weak-key storage. Copies made using object spread, JSON, or structured cloning are not valid host handles. Neither formatted text nor the host tag contains the payload. The host owns resource lifetime and cloning.

For script-constructible records, opt in explicitly:

```javascript
const pointType = HostTypes.describe({
  name: "Point",
  fields: {X: T.float, Y: T.float},
  scriptConstructible: true,
});
const points = registration({types: [pointType]});
const point = run("let p = { X = 1.5; Y = 2.5 }\n{ p with X = 3.5 }", {
  registration: points,
});
const fields = HostTypes.unwrap(points, "Point", point);
// fields.X is {kind: "float", value: 3.5}
```

Constructible types use ordinary record values. They have no hidden payload; passing a non-null payload to `wrap` fails. They follow ordinary structural record equality, construction, and update rules. `HostTypes.fields(value)` returns tagged visible fields for either kind of record.

### Type descriptor

`HostTypes.describe(definition)` copies and freezes this explicit descriptor. Hidden members are omitted entirely.

| Field | Type / default | Purpose |
| --- | --- | --- |
| `name` | Required string | Script-visible type name, used with `T.named(name)` |
| `fields` | Required object mapping field names to `T` descriptors | Visible field names and their closed types |
| `scriptConstructible` | Boolean, default `false` | `false` requires a host-created opaque value; `true` permits script record construction and updates |

The bridge maps `scriptConstructible: false` to the interpreter's `HostManaged = true` metadata. A descriptor by itself does not install a global type. Registration validates duplicate/reserved names and references to other registered types.

### Union descriptor

`HostTypes.describeUnion({name, cases})` describes a script-constructible nominal union. Cases with `null` payload types are nullary; use `T.tuple([...])` for multiple payload fields.

| Field | Type | Purpose |
| --- | --- | --- |
| `name` | Required string | Nominal union type name |
| `cases` | Required nonempty object | Map each case name to a `T` payload descriptor or `null` |

```javascript
const modeType = HostTypes.describeUnion({
  name: "Mode",
  cases: {Off: null, On: T.named("Node")},
});
const modes = registration({types: [nodeType], unions: [modeType]});
const on = HostTypes.wrapUnion(modes, "Mode", "On", node);
const taggedUnion = HostTypes.unwrapUnion(modes, "Mode", on);
```

Constructed unions also cross the boundary through the ordinary `{kind: "union", typeName, caseName, value}` representation. Helpers validate case membership and payload types. Referenced opaque payloads remain authentic host handles.

### Function descriptor

`HostFunctions.create(definition)` derives the closed curried scheme and arity from explicitly supplied JavaScript types. It uses the same invocation mechanism as `extern`. JavaScript handlers receive tagged arguments and return tagged values or supported plain JavaScript values; integral JavaScript numbers become script `int`, so return `{kind: "float", value: 1}` when an integral floating-point result is required.

| Field | Type | Purpose |
| --- | --- | --- |
| `name` | Required string | Script-visible function name, such as `Host.makeNode` |
| `parameters` | Nonempty array of `T` descriptors | Curried arguments in order; use `[T.unit]` for a function taking `()` |
| `result` | Required `T` descriptor | Authoritative return type |
| `invoke` | Synchronous `(taggedArguments, context) => value` | Host implementation |

Use `extern({name, arity, scheme, invoke})` when defining polymorphic schemes with `T.forall`. Its existing fields and calling convention remain supported; the invocation context is an additional second argument. Both helpers validate argument/result values at interpreter boundaries. `invoke(loaded, name, args)` also validates exported-function arguments and results against their inferred types.

### Extension registration

`registration(definition)` snapshots and validates the complete environment, returning an opaque registration handle. Missing arrays default to empty. Descriptors may be authored directly using the fields above or produced by `HostTypes` helpers.

| Field | Type | Purpose |
| --- | --- | --- |
| `types` | Array of type descriptors | Register opaque and constructible record types |
| `unions` | Array of union descriptors | Register nominal union types and cases |
| `functions` | Array of externs | Register functions produced by `extern` or `HostFunctions.create` |

Pass the handle as `options.registration` to `run`, `load`, `parse`, or `createSession`. It persists through inference, evaluation, imported code, and session reset. Optional `options.externs` are added to its functions; duplicate names fail. No registrations leak between environments. Legacy extern-list calls continue to work.

`infer(program, registrationOrExterns?)` and `evaluate(typedProgram, registrationOrExterns?)` accept a complete replacement registration or a function-list override. A list override retains the existing registered types/unions. Omit the argument to retain the handle's full environment.

### Script callbacks

The handler's second argument is a read-only invocation context:

| Method | Purpose |
| --- | --- |
| `apply(callable, arguments)` | Apply an authentic script callable to an array of plain/tagged arguments and return a tagged result |
| `checkRuntime()` | Check the active runtime before continuing host work |
| `spawnTask(callable)` | Reserved bridge operation; fails explicitly in Fable builds |
| `awaitTask(task)` | Reserved bridge operation; fails explicitly in Fable builds |

```javascript
const apply = HostFunctions.create({
  name: "Host.apply",
  parameters: [T.func(T.int, T.int), T.int],
  result: T.int,
  invoke([fn, value], context) {
    return context.apply(fn, [value]);
  },
});
run("Host.apply (fun n -> n + 1) 41", {externs: [apply]});
```

Callback application requires a closed declared function type; use `HostFunctions.create` for these signatures. Open polymorphic callback types fail explicitly rather than weakening nominal validation.

Callable handles are bound to their originating runtime and can only be used while it is active. Reusing handles from expired or foreign invocations fails, including inside containers. Callback language errors preserve their source spans. Exported functions remain callable through `invoke`; output-only callable descriptions are not a separate detached invocation API. Authentic type-token handles can round-trip through `fromJs`/`toJs`.

Run the [complete JavaScript hosting sample](https://github.com/MagnusOpera/FScript/blob/main/src/FScript.JavaScript/examples/hosting.mjs) after `make fable-build` with `node src/FScript.JavaScript/examples/hosting.mjs`. It demonstrates explicit payload mutation, an immutable visible snapshot, a constructible record, and a script callback.

The facade remains synchronous. Promise-returning externs are rejected. JavaScript workers, asynchronous tasks, CLR attributes, assembly/NuGet discovery, and `fscript.toml` loading are not part of JavaScript hosting.

## Resolve imports from virtual sources

Browser and Node Fable hosts do not use the .NET file-backed resolver. Imports must resolve through virtual paths supplied by the host.

```javascript
const loaded = load(
  'import "shared.fss" as Shared\nexport let value = Shared.inc 41',
  {
    rootDirectory: "/",
    entryFile: "/main.fss",
    sources: {
      "/shared.fss": "let inc x = x + 1",
    },
  },
);
```

The host may also provide `resolveImport(path)` for lazy source lookup. Imported paths must stay under `rootDirectory` and end in `.fss`.

## Use a browser session

The JavaScript facade includes a small stateful session API for browser sandbox and REPL-like hosts. It retains declarations between submissions and returns a result only when the submission contains an expression.

```javascript
const session = createSession({
  rootDirectory: "/",
  entryFile: "/main.fss",
});

submit(session, "let add x y = x + y");
const result = submit(session, "add 20 22");

console.log(result.text);
// "42"
```

`resetSession(session)` clears retained declarations.

## Tagged values

Values crossing the JavaScript boundary are represented structurally:

| FScript value | JavaScript shape |
| --- | --- |
| `()` | `{ kind: "unit" }` |
| `42` | `{ kind: "int", value: 42n }` |
| `3.14` | `{ kind: "float", value: 3.14 }` |
| `true` | `{ kind: "bool", value: true }` |
| `"text"` | `{ kind: "string", value: "text" }` |
| `[1; 2]` | `{ kind: "list", values: [...] }` |
| `(1, "x")` | `{ kind: "tuple", values: [...] }` |
| `{ name = "Ada" }` | `{ kind: "record", fields: { ... } }` |
| `Some 1` / `None` | `{ kind: "option", value: taggedValue \| null }` |
| Map | `{ kind: "map", entries: [{ key: taggedKey, value: taggedValue }, ...] }` |
| Union case value | `{ kind: "union", typeName, caseName, value: taggedPayload \| null }` |
| Type token | Authentic `{ kind: "type", name: "..." }` handle |
| Opaque host value | Authentic `{ kind: "host", typeName, fields }` handle; payload is private |

Functions, externals, tasks, and unapplied union constructors use authentic opaque handles at the JavaScript boundary. Callback handles can be applied through the active host invocation context. Constructed union values use the tagged union form above. Exported functions should be called through `invoke`.

Input conversion accepts plain strings, booleans, bigint integers, numbers, arrays (lists), and plain objects (records). `null`/`undefined` become script `()`; represent `None` with `{kind: "option", value: null}`. Use tagged forms to distinguish tuples, maps, options, and explicitly floating-point numbers. Tagged map keys must be strings, integers, booleans, or payload-free union values. Fabricated `host`, `type`, and `opaque` tags cannot be converted back as ordinary input values. Retain the authentic handles for round trips.

## API lookup

| Entry point | Purpose |
| --- | --- |
| `run(source, options?)` | Parse, infer, and evaluate; return the final tagged value |
| `parse(source, options?)` | Produce an opaque parsed-program handle |
| `infer(program, registrationOrExterns?)` | Infer with the retained registration or an explicit override |
| `evaluate(typedProgram, registrationOrExterns?)` | Evaluate with the retained registration or an explicit override |
| `load(source, options?)` | Load once and retain exported functions/values |
| `invoke(loaded, name, args)` | Call an exported function with converted arguments |
| `getValue(loaded, name)` | Read an exported non-callable value |
| `listFunctions(loaded)`, `listValues(loaded)` | Return exported names |
| `createSession(options?)`, `submit(session, source)`, `resetSession(session)` | Create, advance, and reset persistent state |
| `formatValue(value)` | Format a plain/tagged input as FScript text |
| `fromJs(value)`, `toJs(internalValue)` | Convert to/from an internal Fable value; `toJs` returns a tagged object, not an untagged primitive |
| `extern(definition)` | Build an extern with `name`, `arity`, `scheme`, and `invoke` |
| `HostTypes` | Describe types/unions, wrap snapshots and validate payload retrieval |
| `HostFunctions.create(definition)` | Derive arity and a scheme from explicit parameter/result types |
| `registration(definition)` | Validate and retain types, unions, and functions |
| `T` | Build type/scheme descriptors |

Options accept `registration`, `externs`, `rootDirectory`, `entryFile`, `sources`, and `resolveImport`. Imports need the virtual resolver options shown above. Most callers should use `run` or `load`/`invoke` rather than the separate compiler stages.

`T` exposes `unit`, `int`, `float`, `bool`, `string`, `typeToken`, `named(name)`, `var(id)`, `list(type)`, `task(type)`, `option(type)`, `map(valueType)` (string keys), `mapOf(keyType, valueType)`, `tuple(types)`, `record(fields)`, `func(argument, result)`, `scheme(type)`, and `forall(variableIds, type)`. Use repeated `T.var(id)` values and list the same IDs in `T.forall` for polymorphic schemes. The descriptors are facade values, not source annotation strings.

## Errors

Language errors are thrown as JavaScript `Error` objects with structured fields:

```javascript
try {
  run("let = 1");
} catch (error) {
  console.log(error.kind);  // "fscript-error"
  console.log(error.phase); // "parse", "type", "eval", or "host"
  console.log(error.span);
}
```

## Current limits

The Fable package exposes the core language engine, virtual imports, exported invocation, tagged values, sessions, and JavaScript-provided externs. It does not include .NET runtime externs such as `Fs.*`, `Console.*`, `Task.spawn`, or `Task.await`.

Try the browser runtime in the [FScript sandbox](/sandbox).

## .NET and JavaScript environments

Both hosts use the same language rules for provided records, opaque snapshots, and nominal unions. The .NET SDK additionally derives descriptors and typed conversions from CLR types. JavaScript authors supply descriptors explicitly and retain ordinary JavaScript payload objects through `HostTypes`.

CLR attributes, assembly providers, and NuGet environments belong to .NET hosting. `fscript.toml` does not load CLR assemblies into a JavaScript host or the browser playground. See [Host-Managed Types](./host-managed-types) and [Environment Configuration](../reference/environment-configuration).
