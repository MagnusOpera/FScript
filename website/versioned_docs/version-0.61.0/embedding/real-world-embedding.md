---
id: real-world-embedding
title: Real-World Embedding
sidebar_label: Real-World Embedding
slug: /embedding/real-world-embedding
---

This page shows a practical host flow with real concerns:

1. load a script (with `import` support),
2. inject host functions,
3. resolve exported function types,
4. execute exported functions safely.

The example uses an in-memory source resolver so the host controls where imported source comes from.

## End-to-end host example (F#)

```fsharp
open System
open System.IO
open FScript.Language
open FScript.Runtime

// 1) Entry script loaded from host memory.
let root = "/virtual/workspace"
let entryFile = Path.Combine(root, "main.fss")
let entrySource =
    """
import "shared/validation.fss" as Validation

[<export>]
let run (name: string) =
  let normalized = Host.normalizeName name
  Validation.validate normalized
"""

// 2) Imported files also come from host memory.
let virtualSources =
    Map [
        Path.Combine(root, "shared/validation.fss"),
        """
let validate (name: string) =
  if String.toUpper name = name then
    $"VALID:{name}"
  else
    $"INVALID:{name}"
"""
    ]

let resolveImportedSource (fullPath: string) : string option =
    virtualSources |> Map.tryFind fullPath

// 3) Inject host function(s) as externs.
let normalizeNameExtern =
    { Name = "Host.normalizeName"
      Scheme = Forall([], TFun(TString, TString))
      Arity = 1
      Impl =
        fun _ args ->
            match args with
            | [ VString s ] -> VString (s.Trim().ToUpperInvariant())
            | _ -> failwith "Host.normalizeName expects one string argument" }

let hostContext =
    { HostContext.RootDirectory = root
      DeniedPathGlobs = [] }

let externs = normalizeNameExtern :: Registry.all hostContext

// 4) Load script once (imports + extern typing + evaluation environment).
let loaded =
    ScriptHost.loadSourceWithIncludes
        externs
        root
        entryFile
        entrySource
        resolveImportedSource

// 5) Resolve exported function type/signature before execution.
let runSignature = loaded.ExportedFunctionSignatures |> Map.find "run"
let parameterTypes = runSignature.ParameterTypes |> List.map Types.typeToString
let returnType = Types.typeToString runSignature.ReturnType

printfn "run args: %A" parameterTypes
printfn "run return: %s" returnType

// 6) Execute exported function.
let result = ScriptHost.invoke loaded "run" [ VString "  Ada  " ]

match result with
| VString value -> printfn "Result: %s" value
| _ -> failwith "Unexpected result type"
```

## Why this pattern works well in production

- **Load once, invoke many times**: keep parsed/inferred/evaluated script state in memory.
- **Extern injection is explicit**: only functions you register are callable.
- **Resolver is host-owned**: imports can come from DB, cache, or controlled virtual FS.
- **Type resolution before invoke**: host can validate contracts before calling exports.

## Common extensions

- cache `LoadedScript` by tenant/project/version,
- enforce root and deny-path policies via `HostContext`,
- validate `ExportedFunctionSignatures` against host-side contracts,
- wrap invocation in timeout/cancellation boundaries at host level.
