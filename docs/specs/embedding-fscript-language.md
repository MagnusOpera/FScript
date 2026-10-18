# Embedding `FScript.Language`

This document describes the host-facing interface of `FScript.Language`:
- running the interpreter pipeline,
- discovering script functions,
- invoking functions,
- supported host-visible types and values.

Note on CLI host behavior:
- The `fscript` CLI injects `let Env` into script execution.
- The `Environment` type is provided by stdlib.
- That injection is a CLI host convenience, not part of the `FScript.Language` core API contract.
- Hosts can provide filesystem denied glob patterns through `HostContext.DeniedPathGlobs`.

## Namespace and entry points

Reference `MagnusOpera.FScript.Language` and use namespace `FScript.Language`.

Primary entry points are in module `FScript`:

- `FScript.parse : string -> Program`
- `FScript.infer : Program -> TypeInfer.TypedProgram`
- `FScript.inferWithExterns : ExternalFunction list -> Program -> TypeInfer.TypedProgram`
- `FScript.eval : TypeInfer.TypedProgram -> Value`
- `FScript.evalWithExterns : ExternalFunction list -> TypeInfer.TypedProgram -> Value`
- `FScript.run : string -> Value` (parse + infer + eval without externs)

JavaScript hosts should use the Fable-compiled package surface instead of raw F# discriminated unions. The ESM package is published as `@magnusopera/fscript` and exports:

- `run(source, options?)`
- `parse(source, options?)`
- `infer(program, externs?)`
- `evaluate(typedProgram, externs?)`
- `load(source, options?)`
- `invoke(loaded, name, args)`
- `listFunctions(loaded)`
- `listValues(loaded)`
- `getValue(loaded, name)`
- `formatValue(value)`
- `toJs(value)`
- `fromJs(value, expectedType?)`
- `createSession(options?)`
- `submit(session, source)`
- `resetSession(session)`
- `T` type and scheme builders
- `extern({ name, arity, scheme, invoke })`

The JavaScript package targets Node and browser-compatible ESM hosts. It embeds `FScript.Language` through Fable and does not include the .NET `FScript.Runtime` extern catalog.

## Running scripts

```fsharp
open FScript.Language

let source = "let inc x = x + 1\ninc 41"
let result = FScript.run source

match result with
| VInt n -> printfn "Result: %d" n
| other -> printfn "Unexpected value: %s" (Pretty.valueToString other)
```

## Running with external functions

Use `inferWithExterns` and `evalWithExterns` when the script needs host functions.

```fsharp
open FScript.Language

let toUpperExtern =
    { Name = "toUpper"
      Scheme = Forall([], TFun(TString, TString))
      Arity = 1
      Impl =
        function
        | [ VString s ] -> VString (s.ToUpperInvariant())
        | _ -> raise (EvalException { Message = "toUpper expects string"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) }) }

let source = "toUpper \"fscript\""
let program = FScript.parse source
let typed = FScript.inferWithExterns [ toUpperExtern ] program
let result = FScript.evalWithExterns [ toUpperExtern ] typed
```

## JavaScript embedding

Use `run` for one-shot execution and `load` when the host will invoke exported functions repeatedly.

```javascript
import { T, extern, run, load, invoke, getValue } from "@magnusopera/fscript";

const shout = extern({
  name: "Host.shout",
  arity: 1,
  scheme: T.scheme(T.func(T.string, T.string)),
  invoke(args) {
    return { kind: "string", value: `${args[0].value}!` };
  }
});

const value = run("Host.shout \"fable\"", { externs: [shout] });
// value = { kind: "string", value: "fable!" }

const loaded = load("export let add x y = x + y\nexport let answer = 42");
const sum = invoke(loaded, "add", [1, 2]);
const answer = getValue(loaded, "answer");
```

JavaScript values crossing the host boundary use tagged objects:

- `{ kind: "unit" }`
- `{ kind: "int", value: 42n }`
- `{ kind: "float", value: 3.14 }`
- `{ kind: "bool", value: true }`
- `{ kind: "string", value: "text" }`
- `{ kind: "list", values: [...] }`
- `{ kind: "tuple", values: [...] }`
- `{ kind: "record", fields: { ... } }`
- `{ kind: "map", entries: [{ key, value }] }`
- Map entry keys may be tagged strings, ints, bools, or payload-free union cases.
- `{ kind: "option", value: null | taggedValue }`
- `{ kind: "union", typeName, caseName, value: null | taggedValue }`
- `{ kind: "type", name }`
- `{ kind: "opaque", valueType }`

`int` values are emitted as JavaScript `bigint`. Inputs may use `bigint`, safe integer `number`, string, or tagged `{ kind: "int", value }` forms. Functions, externals, tasks, and union constructors are opaque values; exported functions are invoked through `invoke`.

JavaScript errors thrown by the facade have `kind: "fscript-error"`, `phase` (`parse`, `type`, `eval`, or `host`), `message`, and span data when the underlying language stage provides it.

### JavaScript sessions

Browser sandbox and REPL-like hosts can use the stateful session API:

```javascript
const session = createSession({
  rootDirectory: "/",
  entryFile: "/main.fss"
});

submit(session, "let add x y = x + y");
const result = submit(session, "add 20 22");
// result = { kind: "session-result", hasValue: true, value, text: "42", retainedCount: 1 }

resetSession(session);
```

Sessions retain submitted declarations, inferred binding schemes, runtime values, and type declarations. Expression-only submissions are inferred and evaluated incrementally against that retained state and return a formatted `text` result plus the tagged `value`. Adding a type declaration rebuilds the session from its retained program so the declaration can safely affect record-shape resolution. The browser session API mirrors the CLI REPL's core state model, but it does not include console-specific controls such as prompts, EOF handling, or default .NET runtime externs.

### JavaScript imports

Fable builds do not use the .NET file-backed import resolver. A JavaScript host must supply imports through virtual paths:

```javascript
const loaded = load("import \"shared.fss\" as Shared\nexport let value = Shared.inc 41", {
  rootDirectory: "/",
  entryFile: "/main.fss",
  sources: {
    "/shared.fss": "let inc x = x + 1"
  }
});
```

The facade also accepts `resolveImport(path)` when imports should be loaded lazily. In JavaScript, imports are confined to the virtual `rootDirectory`, must resolve to `.fss` paths, and must return source text from the host-provided source map or callback. File system reads and symlink checks remain .NET-only behavior.

## Loading once and invoking by name

Use `FScript.Runtime.ScriptHost` when a host needs reusable loading and direct function invocation.
Only top-level exported bindings are exposed through this API.

```fsharp
open FScript.Language
open FScript.Runtime

let externs = Registry.all { RootDirectory = "."; DeniedPathGlobs = [] }
let loaded = ScriptHost.loadSource externs "export let add x y = x + y"
let result = ScriptHost.invoke loaded "add" [ VInt 1L; VInt 2L ]
```

## Getting function metadata

After inference, function descriptors can be produced with `Descriptor.describeFunctions`.

```fsharp
open FScript.Language

let src = "let add x y = x + y\nlet rec fact n = if n = 0 then 1 else n * fact (n - 1)"
let typed = src |> FScript.parse |> FScript.infer

let functions =
    Descriptor.describeFunctions typed Map.empty

for f in functions do
    let args = f.Parameters |> List.map Types.typeToString |> String.concat " -> "
    printfn "%s : %s -> %s" f.Name args (Types.typeToString f.ReturnType)
```

`FunctionDescriptor` exposes:
- `Name`
- `Parameters` (`Type list`)
- `ReturnType` (`Type`)
- `Scheme` (`Scheme option`)
- `IsRecursive`
- `Span`

## Invoking functions

Function invocation is expression-based: evaluate a script where the target function is called.

```fsharp
open FScript.Language

let src = "let add x y = x + y\nadd 1 2"
let value = FScript.run src
```

You can also evaluate to a function value:

```fsharp
open FScript.Language

let value = FScript.run "let add1 x = x + 1\nadd1"

match value with
| VClosure _ -> printfn "Function value returned"
| _ -> printfn "Not a function"
```

## Using `nameof` for exported capability keys

`nameof` returns the bound identifier name as a string and type-checks that the identifier exists.

```fsharp
open FScript.Language

let src = "let run_step x = x\nnameof run_step"
let value = FScript.run src
// value = VString "run_step"
```

## Supported types

Host-visible static types are represented by `Type`:

- `TUnit`
- `TInt`
- `TFloat`
- `TBool`
- `TString`
- `TList of Type`
- `TTuple of Type list`
- `TRecord of Map<string, Type>`
- `TMap of Type * Type`
- `TOption of Type`
- `TFun of Type * Type`
- `TNamed of string`
- `TUnion of string * Map<string, Type option>`
- `TTypeToken`
- `TVar of int`

Use `Types.typeToString` for display/diagnostics.
The postfix script type syntax `'a map` corresponds to `TMap(TString, 'a)`.
The explicit `Value map[Key]` syntax corresponds to `TMap(Key, Value)`; supported key types are `string`, `int`, `bool`, and unions with no payload-bearing cases. `Json.serialize` encodes string, integer, and boolean map keys as JSON object property names; maps containing union keys return `None`.

## Runtime values

Evaluation returns `Value`:

- `VUnit`
- `VInt of int64`
- `VFloat of float`
- `VBool of bool`
- `VString of string`
- `VList of Value list`
- `VTuple of Value list`
- `VRecord of Map<string, Value>`
- `VMap of Map<MapKey, Value>`
- `VOption of Value option`
- `VUnionCase of string * string * Value option`
- `VUnionCtor of string * string`
- `VTypeToken of Type`
- `VClosure of string * Expr * Env ref`
- `VExternal of ExternalFunction * Value list`

Use `Pretty.valueToString` for a readable string form.

## Errors

`FScript.Language` raises typed exceptions with source spans:

- `ParseException` (`ParseError`)
- `TypeException` (`TypeError`)
- `EvalException` (`EvalError`)

Each error contains:
- `Message`
- `Span` (`Line`/`Column` information)

## Embedding cookbook

### 1. Parse/eval a file with `import`
Use import-aware parsing when executing scripts from disk.

```fsharp
open FScript.Language

let root = "/path/to/workspace"
let entry = "/path/to/workspace/main.fss"

let program = FScript.parseFileWithIncludes root entry
let typed = FScript.infer program
let value = FScript.eval typed
```

### 2. Load once, invoke many times
Use `ScriptHost` when you want to invoke exported functions repeatedly.

```fsharp
open FScript.Language
open FScript.Runtime

let hostContext = { RootDirectory = "."; DeniedPathGlobs = [] }
let externs = Registry.all hostContext

let loaded =
    ScriptHost.loadFile externs "./script.fss"

let r1 = ScriptHost.invoke loaded "run" [ VString "build" ]
let r2 = ScriptHost.invoke loaded "run" [ VString "test" ]
```

### 3. Load an in-memory script with `import`
Use resolver-backed loading when the entry script source is in memory and imports must be resolved by the host.

```fsharp
open System.IO
open FScript.Language
open FScript.Runtime

let root = "/virtual/workspace"
let entryFile = Path.Combine(root, "main.fss")
let entrySource = "import \"shared.fss\" as Shared\nexport let run x = Shared.add1 x"

let normalizeVirtualPath (path: string) =
    Path.GetFullPath(path)

let embeddedSources =
    Map [
        normalizeVirtualPath (Path.Combine(root, "shared.fss")), "let add1 x = x + 1"
    ]

let resolver path =
    embeddedSources |> Map.tryFind (Path.GetFullPath(path))

let externs = Registry.all { RootDirectory = root; DeniedPathGlobs = [] }
let loaded =
    ScriptHost.loadSourceWithIncludes externs root entryFile entrySource resolver

let result = ScriptHost.invoke loaded "run" [ VInt 41L ]
```

### 4. Add one custom extern
Define a typed external function and pass it to inference/evaluation.

```fsharp
open FScript.Language

let envExtern =
    { Name = "Env.get"
      Scheme = Forall([], TFun(TString, TOption TString))
      Arity = 1
      Impl =
        function
        | [ VString key ] ->
            System.Environment.GetEnvironmentVariable(key)
            |> Option.ofObj
            |> VOption
        | _ ->
            raise (EvalException { Message = "Env.get expects one string"; Span = Span.mk (Span.pos 0 0) (Span.pos 0 0) }) }

let src = "Env.get \"HOME\""
let program = FScript.parse src
let typed = FScript.inferWithExterns [ envExtern ] program
let value = FScript.evalWithExterns [ envExtern ] typed
```

### 5. Export descriptor discovery
After inference, get function descriptors to expose callable surface in hosts.

```fsharp
open FScript.Language

let typed = "export let run x = x" |> FScript.parse |> FScript.infer
let descriptors = Descriptor.describeFunctions typed Map.empty
```

### 6. Exported function signatures without evaluation
Use `FScript.Runtime.ExportSignatures.fromTypedProgram` when a host needs exported function signatures from typed AST without executing script bodies.

```fsharp
open FScript.Language
open FScript.Runtime

let typed = "export let add (x: int) (y: int) = x + y" |> FScript.parse |> FScript.infer
let signatures = ExportSignatures.fromTypedProgram typed
```

## Registering host-managed types

Use `ExtensionRegistration` and the registration-aware inference/evaluation, `ScriptHost`, or `Interactive` entry points to provide host type descriptors alongside extern functions. `HostInterop.create` validates visible field snapshots; `HostInterop.unwrap` gives trusted host functions access to the opaque payload. Existing extern-list APIs retain their empty-type-registration behavior. See [host-managed types](./host-managed-types.md) for the complete contract and API names.

## Environment initialization

`FScript.Runtime.EnvironmentConfiguration` parses/normalizes manifests and merges caller defaults and overrides into `EnvironmentSettings`, without loading providers. `EnvironmentLoader.load` creates an atomic `ExtensionRegistration` through the existing provider protocol. Embedders can inspect effective settings before deciding to load executable assemblies. `EnvironmentConfiguration.resolveForCli` adds CLI-only beside-entry discovery and `--no-config` handling; the existing `resolve` API remains explicit for embedders/editor environments. Existing explicit-registration APIs remain unchanged. Registration-aware inference with local-variable telemetry is also available to editor hosts. See [environment configuration](./environment-configuration.md).

## CLR-backed host type descriptors

For host-managed values backed by real CLR types, use the independent SDK's `[<FScriptHostType("Name")>]` and `[<FScriptVisible>]` attributes with `HostTypes.describe<'T>()`, `wrap registration value`, and `unwrap<'T> value`. The same mapping derives field types and snapshots property values; hidden state remains the original CLR payload. Descriptor creation does not execute getters. See [host-managed types](./host-managed-types.md#optional-clr-projection-helpers) for mappings, registration requirements, and failures. The lower-level Language helpers remain available for custom descriptors/payloads.

Set `ScriptConstructible = true` on fully visible F# record types to accept ordinary script-created records. Descriptor initialization rejects hidden constructor fields. `HostTypes.wrap` then emits `VRecord`, and `unwrap<'T>` constructs a CLR record from the fields instead of retrieving an opaque payload. The default opaque mode remains available for hidden host state. See [provided record construction](./host-managed-types.md#script-constructible-provided-types).


## Typed host function declaration

Use `HostFunctions.create registration name handler` for concrete synchronous F# handlers. It returns an SDK `ExternalFunction`; adapt its containing registration with `ExtensionBridge.toLanguageRegistration` for native embedding entry points, deriving the monomorphic scheme, arity, and conversions from the handler signature. Register all referenced host types explicitly; use the manual record API for script callbacks, polymorphic schemes, or runtime call-context access. See [host function adapters](./host-functions.md).

Registrations also expose `Unions: HostUnionDescriptor list`; inference, evaluation, and interactive sessions retain these named union definitions and constructors across submissions. Qualified constructors such as `Protocol.Mode.Enabled 42` resolve through the same environment as external functions.

## Independent extension contract

Assembly providers use `FScript.Extensibility` attributes and return SDK `ExternalFunction list` or `ExtensionRegistration`, with no arguments or one sealed SDK `HostContext`. Existing provider assemblies must rebuild. SDK helpers and values do not depend on interpreter assemblies. Runtime adapts registrations using `ExtensionBridge.toLanguageRegistration`; native embedding APIs retain their Language representations. Authoring helpers live in the SDK, not Runtime.

Provider trust, atomic loading, configuration selection, and reload rules are unchanged. SDK callback/task handles are valid only in their originating active evaluation; handler contexts expire on return. Runtime enforces limits and rejects foreign/expired handles. An SDK `ExtensionException` carries a message and optional inner cause; callback errors preserve script locations.

## NuGet-distributed extensions

`[[runtime.packages]]` declares a required package `name` and optional exact `version`; an omitted version selects latest stable. The shared Runtime loader restores dependencies without the .NET SDK and discovers the existing SDK `[FScriptExternProvider]` methods in explicitly requested package runtime assemblies. Providers return the same SDK registrations used by local DLLs. `EnvironmentLoader.loadWithInfo` additionally exposes resolved package versions and concrete assembly paths; `load` retains its registration-only result. Parsing settings does not restore or execute packages. See [environment configuration](environment-configuration.md) for cache, dependency selection, trust, and failure rules.

NuGet environments isolate their managed dependencies as one graph, unifying only SDK/FSharp.Core contracts with the host. All providers in that graph can exchange shared CLR numerical payloads. Host applications that reference provider CLR libraries directly should register those libraries through their typed APIs rather than casting private NuGet-context payloads to a default-context type.

## Debug sessions (.NET)

`FScript.Runtime.DebugSession` accepts a language registration, parsed program,
and `stopOnEntry`. Subscribe to `Events`, configure breakpoints, then call `Run`
on a dedicated execution thread (or the process main thread for native GUI
extensions). Event handlers should enqueue work and return promptly; the adapter
uses an asynchronous event queue so protocol I/O never blocks interpreter hooks.
A control thread uses `Threads`, `StackTrace`, `Scopes`, `Variables`,
`Evaluate`, `Resume`, `Pause`, and `Cancel`. Inspection returns safe DTOs, not raw
`Value` instances. Dispose the session after execution. An optional task scheduler
lets hosts control scheduling of background FScript continuations.

`DebugLaunch.create` prepares a file session with the shared manifest loader and
CLI environment binding. See [debugger behavior](debugger.md) for callback affinity,
watch restrictions, reference lifetime, and cancellation. The Extensibility SDK
contract is unchanged, and Fable has no debugger support.
