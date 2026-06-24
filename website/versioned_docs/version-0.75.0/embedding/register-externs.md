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
