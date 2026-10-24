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
| Type token | `{ kind: "type", name: "..." }` (output only) |

Functions, externals, tasks, and unapplied union constructors are opaque at the JavaScript boundary. Constructed union values use the tagged union form above. Exported functions should be called through `invoke`.

Input conversion accepts plain strings, booleans, bigint integers, numbers, arrays (lists), and plain objects (records). `null`/`undefined` become `None`. Use tagged forms to distinguish tuples, maps, options, and explicitly floating-point numbers. Tagged map keys must be strings, integers, booleans, or payload-free union values. Output-only `type` and `opaque` tags cannot be converted back as ordinary input values.

## API lookup

| Entry point | Purpose |
| --- | --- |
| `run(source, options?)` | Parse, infer, and evaluate; return the final tagged value |
| `parse(source, options?)` | Produce an opaque parsed-program handle |
| `infer(program, externs?)` | Produce a typed-program handle, using the parsed handle's externs unless overridden |
| `evaluate(typedProgram, externs?)` | Evaluate a typed handle and return a tagged value |
| `load(source, options?)` | Load once and retain exported functions/values |
| `invoke(loaded, name, args)` | Call an exported function with converted arguments |
| `getValue(loaded, name)` | Read an exported non-callable value |
| `listFunctions(loaded)`, `listValues(loaded)` | Return exported names |
| `createSession(options?)`, `submit(session, source)`, `resetSession(session)` | Create, advance, and reset persistent state |
| `formatValue(value)` | Format a plain/tagged input as FScript text |
| `fromJs(value)`, `toJs(internalValue)` | Convert to/from an internal Fable value; `toJs` returns a tagged object, not an untagged primitive |
| `extern(definition)` | Build an extern with `name`, `arity`, `scheme`, and `invoke` |
| `T` | Build type/scheme descriptors |

Options accept `externs`, `rootDirectory`, `entryFile`, `sources`, and `resolveImport`. Imports need the virtual resolver options shown above. Most callers should use `run` or `load`/`invoke` rather than the separate compiler stages.

`T` exposes `unit`, `int`, `float`, `bool`, `string`, `typeToken`, `var(id)`, `list(type)`, `task(type)`, `option(type)`, `map(valueType)` (string keys), `tuple(types)`, `record(fields)`, `func(argument, result)`, `scheme(type)`, and `forall(variableIds, type)`. Use repeated `T.var(id)` values and list the same IDs in `T.forall` for polymorphic schemes. The descriptors are facade values, not source annotation strings.

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

## .NET host-managed values and assembly environments

Host-managed opaque payloads, `HostFunctions.create` typed F# handlers, and `[<FScriptExternProvider>]` assembly discovery belong to the .NET bridge. The JavaScript facade does not support authoring host-managed values; attempts to export them fail rather than discard state or metadata. `fscript.toml` does not load CLR assemblies into the JavaScript host or browser playground.

See [Host-Managed Types](./host-managed-types) and [Environment Configuration](../reference/environment-configuration) for .NET embedding and CLI/editor usage. Existing JavaScript extern APIs remain unchanged.
