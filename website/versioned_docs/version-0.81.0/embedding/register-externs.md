---
id: register-externs
title: Register Extern Functions
slug: /embedding/register-externs
---

Externs are host functions exposed to scripts.

Each extern defines:

- name,
- type scheme,
- arity,
- implementation.

## Minimal pattern

```fsharp
{ Name = "My.add"
  Scheme = Forall([], TFun(TInt, TFun(TInt, TInt)))
  Arity = 2
  Impl = fun ctx args -> ... }
```

## Why schemes matter

Type schemes are injected into type inference, so script calls are type-checked before evaluation.

## Real function injection example

```fsharp
open FScript.Language

let slugifyExtern =
    { Name = "Host.slugify"
      Scheme = Forall([], TFun(TString, TString))
      Arity = 1
      Impl =
        fun _ args ->
            match args with
            | [ VString s ] ->
                s.Trim().ToLowerInvariant().Replace(" ", "-")
                |> VString
            | _ ->
                failwith "Host.slugify expects one string argument" }
```

Use it with runtime externs:

```fsharp
open FScript.Runtime

let hostContext =
    { HostContext.RootDirectory = "."
      DeniedPathGlobs = [] }

let externs = slugifyExtern :: Registry.all hostContext
```

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

Use `ExtensionRegistration` when an environment also exposes [host-managed types](./host-managed-types). Its `Types` list supplies visible field descriptors; `Functions` contains the same extern records shown above:

```fsharp
let registration =
    { HostInterop.empty with Functions = externs }

let loaded =
    ScriptHost.loadSourceWithRegistration registration
        "Host.slugify \"Hello World\""
```

`HostInterop.ofExterns externs` is equivalent when no host types are registered. Existing extern-list APIs remain supported. Registration-aware `ScriptHost`, inference/evaluation, and interactive APIs preserve type metadata and opaque payloads.

Schemes remain authoritative at host-call boundaries. Arguments/results must have their declared interpreter shapes, including nested records, unions, and options. A `VMap` is not a `VRecord`; a visible-field record cannot stand in for a host-managed value. Host adapters should construct the appropriate `Value` cases or use the opt-in `HostTypes` helpers for [attributed host-managed objects](./host-managed-types).

## Assembly providers for CLI and editor

The existing `[<FScriptExternProvider>]` attribute marks a public static provider method in a compiled .NET assembly:

```fsharp
open FScript.Runtime

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
