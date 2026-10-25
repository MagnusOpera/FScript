---
id: register-externs
title: Register Extern Functions
slug: /embedding/register-externs
---

Externs are host functions exposed to scripts. For concrete, synchronous F# handlers, use `HostFunctions.create` to derive the type scheme, arity, and value conversion from the handler's CLR signature.

## Declare a typed handler

```fsharp
open FScript.Extensibility

let add =
    HostFunctions.create HostInterop.empty "Host.add"
        (fun (left: int64) (right: int64) -> left + right)

let registration =
    { HostInterop.empty with Functions = [add] }
let loaded =
    FScript.Runtime.ScriptHost.loadSourceWithRegistration
        (FScript.Runtime.ExtensionBridge.toLanguageRegistration registration) "Host.add 20 22"
// loaded.LastValue = VInt 42L
```

The helper returns an ordinary `ExternalFunction` with scheme `int -> int -> int` and arity `2`. Scripts may partially apply it as usual. Every top-level function arrow is treated as another curried parameter; the CLR handler runs after the full argument list has been supplied. A `unit -> 'a` handler has arity one and is called with `()`.

Give parameters concrete types when F# cannot infer them. `int64` matches FScript's 64-bit integers; CLR `int` also maps to script `int`, with checked Int32 conversion on input. Generated schemes are monomorphic: use manual externs for polymorphic schemes.

## Supported types and registration

Arguments and results use the same [CLR mappings as host-provided types](./host-managed-types.md#supported-clr-field-mappings): scalars, unit, lists/sets/one-dimensional arrays, options, tuples/struct tuples, string-keyed F# maps, and attributed host types. No separate conversion policy is introduced.

Pass a registration containing every host descriptor referenced by the handler, including types inside empty containers or `None` results. Unsupported signatures, missing/mismatched descriptors, conflicting CLR projections, null handlers, and invalid names fail at function creation, before script execution. Callbacks, .NET delegates, tasks/async values, arbitrary objects, and unannotated records are unsupported. Returning a function is interpreted as more curried parameters, rather than a separately marshaled function value.

All arguments are decoded and validated before any part of the handler runs, so a bad later argument cannot trigger earlier curried side effects. Results are validated and converted into interpreter values. Opaque arguments recover their typed payload; opaque results snapshot visible fields and retain their payload. Constructible records are rebuilt from script fields. Handler failures identify the script function without exposing reflection exception wrappers. Null values are rejected; use options for absence.

Create the function during environment/provider initialization and reuse it. The helper does not register a function globally, discover CLR methods, or load assemblies.

## Capture host context

A handler can close over configuration or resources without exposing them as script arguments:

```fsharp
// In an assembly provider, use its HostContext parameter.
let hostContext = HostContext(".", [], [])

let slugifyExtern =
    HostFunctions.create HostInterop.empty "Host.slugify"
        (fun (text: string) -> text.Trim().ToLowerInvariant().Replace(" ", "-"))
let rootExtern =
    HostFunctions.create HostInterop.empty "Host.root"
        (fun () -> hostContext.RootDirectory)
let externs = slugifyExtern :: rootExtern :: hostContext.DefaultFunctions
```

When an assembly provider accepts `HostContext`, capture that parameter inside its handlers. The host owns lifetime and synchronization of captured resources.

## The function descriptor behind the helper

`HostFunctions.create` builds an `FScript.Extensibility.ExternalFunction` record. This record is the function descriptor consumed by inference and execution:

| Field | Type | Purpose |
| --- | --- | --- |
| `Name` | `string` | Script-visible name, such as `Host.add`. |
| `Scheme` | `FunctionScheme` | Authoritative script type, used for inference and boundary validation. |
| `Arity` | `int` | Number of script arguments required before `Impl` runs. |
| `Impl` | `ExternalCallContext -> ScriptValue list -> ScriptValue` | Host implementation receiving interpreter values and returning an interpreter value. |

For the typed `add` handler above, the helper generates the equivalent of this manual declaration, with argument/result validation and CLR conversion supplied by the adapter:

```fsharp
let addDescriptor : ExternalFunction =
    { Name = "Host.add"
      Scheme = Forall([], TFun(TInt, TFun(TInt, TInt)))
      Arity = 2
      Impl = fun _ args ->
          match args with
          | [ VInt left; VInt right ] -> VInt(left + right)
          | _ -> failwith "Host.add expects two integers" }
```

`Forall([], ...)` has no quantified type variables: this is the concrete script type `int -> int -> int`. `TFun` nests to represent curried parameters. The record contains the handler as well as its metadata; it is ready to put in a registration's `Functions` list.

## Manual external functions

Use the record API directly for callbacks, `ExternalCallContext`, polymorphic schemes, or arity-zero values. `ExternalCallContext` provides `Apply`, `SpawnTask`, `AwaitTask`, and `CheckRuntime` for interacting with the interpreter. Schemes are authoritative whether generated or written manually; manual implementations own their argument decoding and result encoding.

## Higher-order externs (callback into script)

When an extern receives a script function as argument, use `ExternalCallContext.Apply` to invoke it.

```fsharp
let applyTwiceExtern =
    { Name = "Host.applyTwice"
      Scheme = Forall([], TFun(TFun(TInt, TInt), TFun(TInt, TInt)))
      Arity = 2
      Impl =
        fun ctx args ->
            match args with
            | [ fn; VInt n ] ->
                let once = ctx.Apply fn (VInt n)
                ctx.Apply fn once
            | _ ->
                failwith "Host.applyTwice expects (int -> int) and int" }
```

## Design tips

- Keep extern APIs small and explicit.
- Prefer pure externs when possible.
- Return option-like outcomes for recoverable failures.
- Choose stable names; extern names are script-level API surface.

## Register functions together with host types

Use `ExtensionRegistration` when an environment also exposes [host-managed types](./host-managed-types). The registration itself is also plain data:

```fsharp
type ExtensionRegistration =
    { Types: HostTypeDescriptor list
      Unions: HostUnionDescriptor list
      Functions: ExternalFunction list }
```

`ExtensionRegistration` defines the types and functions supplied together to one host/session or returned by an assembly provider. It does not load assemblies or execute a script itself.

| Field | Type | Purpose |
| --- | --- | --- |
| `Types` | `HostTypeDescriptor list` | Named type definitions available for script annotations, field checking, and host-value validation. |
| `Unions` | `HostUnionDescriptor list` | Named union types, cases, and payload types used by script construction, matching, and boundary validation. |
| `Functions` | `ExternalFunction list` | Callable host functions and their authoritative schemes, used by execution and editor tooling. |

The entries are the [type descriptors](./host-managed-types.md#the-type-descriptor-behind-the-helper) and function descriptors described above. `HostInterop.empty` sets all three lists to empty:

```fsharp
let registration =
    { HostInterop.empty with Functions = externs }

let loaded =
    FScript.Runtime.ScriptHost.loadSourceWithRegistration
        (FScript.Runtime.ExtensionBridge.toLanguageRegistration registration)
        "Host.slugify \"Hello World\""
```

`HostInterop.ofExterns externs` is equivalent when no host types are registered. Existing extern-list APIs remain supported. Registration-aware `ScriptHost`, inference/evaluation, and interactive APIs preserve type metadata and opaque payloads.

Schemes remain authoritative at host-call boundaries. Arguments/results must have their declared interpreter shapes, including nested records, unions, and options. A `VMap` is not a `VRecord`; a visible-field record cannot stand in for a host-managed value. Host adapters should construct the appropriate `ScriptValue` cases or use the opt-in `HostTypes` helpers for [attributed host-managed objects](./host-managed-types).

## Assembly providers for CLI and editor

The existing `[<FScriptExternProvider>]` attribute marks a public static provider method in a compiled .NET assembly:

```fsharp
open FScript.Extensibility

type AppExternProvider =
    [<FScriptExternProvider>]
    static member Provide() : ExtensionRegistration =
        { HostInterop.empty with Functions = [ slugifyExtern ] }
```

Providers accept either no parameters or one `HostContext` and return either `ExternalFunction list` or `ExtensionRegistration`. The loader discovers marked methods; no separate signature file or language-level extern declarations are needed. Registered schemes drive execution checks and editor inference/completion. Duplicate function names and host-type conflicts fail initialization. Return the provider's own functions/types; the loader adds `Registry.all` separately when defaults are enabled.

Load an assembly explicitly:

```bash
fscript --extern-assembly ./extensions/AppExterns.dll script.fss
```

Or list it in a shared `fscript.toml`:

```toml
[runtime]
extern-assemblies = ["./extensions/AppExterns.dll"]
```

Run `fscript --config ./fscript.toml script.fss`, or select that manifest in VS Code. See [Environment Configuration](../reference/environment-configuration) for paths, precedence, selection, and reload.

Provider registration executes host code and may have side effects. The CLI trusts explicitly supplied providers; VS Code requires workspace trust before loading configured assemblies. Parsing a manifest alone does not execute them.

## SDK dependency and lifetime

Extension projects reference only `MagnusOpera.FScript.Extensibility` 1.x and open `FScript.Extensibility`. The SDK targets `netstandard2.1` and has no dependency on the interpreter. Its unsigned assembly identity remains `1.0.0.0` throughout compatible 1.x releases; package and file versions follow releases. Existing extensions must rebuild against this SDK.

Embedders additionally reference Runtime. Convert an SDK registration using `ExtensionBridge.toLanguageRegistration` before passing it to native `ScriptHost` APIs. Those APIs still use `FScript.Language.Value`; SDK values and interpreter values are distinct representations. Perform CLR conversion in SDK handlers, or inspect interpreter values explicitly at the embedding boundary.

`HostContext` is a sealed read-only class exposing `RootDirectory`, `DeniedPathGlobs`, and `DefaultFunctions`. Providers can filter host-supplied defaults without referencing Runtime. Return only provider-owned functions when the loader also enables defaults, to avoid duplicate registrations.

Manual callbacks and tasks are opaque SDK handles. Use `ExternalCallContext.Apply`, `SpawnTask`, `AwaitTask`, and `CheckRuntime`; never inspect a closure or task worker. An invocation context expires when its handler returns. Handles may pass between calls in the same active host evaluation, but foreign or expired handles fail. SDK conversion failures use `ExtensionException`, preserving an inner cause where available; callback failures retain their script source location.

## Distribute through NuGet

Package your SDK-only provider as a .NET library with its normal package dependencies. The public static method bearing `[FScriptExternProvider]` remains the discovery entry point and returns the type/function registration; no additional attribute is required. Users declare `[[runtime.packages]]` with your package `name` and optional exact `version` in their manifest. FScript restores dependencies and loads compatible runtime assets without building the package or requiring the .NET SDK. See [Environment Configuration](../reference/environment-configuration#nuget-packages).

Complementary extensions may share opaque CLR values, such as NDArray. Depend on the numerical provider’s package, use its descriptors for handler validation, and return only your own registration entries. Provider dependencies are discovered once and all packages in the manifest share one managed dependency context. Do not re-register another provider’s types or functions.

## Compiler purity guarantees

Providers can optionally declare `Pure` information for native compilation. Functions promise deterministic, effect-free, successful termination for valid inputs; external types promise deeply immutable visible data and payloads with safe, terminating registered field reads and structural observers. Constructors and ordinary methods do not inherit a receiver type's guarantee.

The compiler reads optional `FScriptPureFunctions()` and `FScriptPureTypes()` provider methods returning registered script names. These declarations are trusted; ordinary interpreter execution and SDK validation remain unchanged. See [Pure FFI and external values](../compiler/compile-to-dotnet.md#pure-ffi-and-external-values) for the contract, examples, callback rules, and compiler API.

Scripts can call the core `Env.setExitCode` operation without a registered extern. It updates execution-local status rather than terminating the host. .NET hosts can read `LoadedScript.ExitCode` or use `ScriptHost.invokeWithExitCode`; returned values remain separate from status.
