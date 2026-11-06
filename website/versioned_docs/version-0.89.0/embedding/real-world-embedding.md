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

## Export host-callable bindings

Use `export let` on top-level bindings that the host must discover and invoke through embedding APIs such as `ScriptHost`.

Bindings are part of the runtime host surface only when they are explicitly declared with `export let`. This makes the host-callable contract intentional: the host can list exported functions, resolve their signatures, read exported values, and invoke exported functions after loading the script.

## End-to-end host example (F#)

```fsharp
open System
open System.IO
open FScript.Language
open FScript.Runtime

// 1) Entry script loaded from host memory.
let root = "/virtual/workspace"
let entryFile = Path.Combine(root, "main.fss")
let normalizeVirtualPath (path: string) = Path.GetFullPath(path)
let entrySource =
    """
import "shared/validation.fss" as Validation

export let run (name: string) =
  let normalized = Host.normalizeName name
  Validation.validate normalized
"""

// 2) Imported files also come from host memory.
let virtualSources =
    Map [
        normalizeVirtualPath (Path.Combine(root, "shared", "validation.fss")),
        """
let validate (name: string) =
  if String.toUpper name = name then
    $"VALID:{name}"
  else
    $"INVALID:{name}"
"""
    ]

let resolveImportedSource (fullPath: string) : string option =
    virtualSources |> Map.tryFind (Path.GetFullPath(fullPath))

// 3) Inject host function(s) as externs.
let normalizeNameExtern =
    FScript.Extensibility.HostFunctions.create
        FScript.Extensibility.HostInterop.empty "Host.normalizeName"
        (fun (name: string) -> name.Trim().ToUpperInvariant())

let hostContext =
    { HostContext.RootDirectory = root
      DeniedPathGlobs = [] }

let custom =
    ExtensionBridge.toLanguageRegistration
        { FScript.Extensibility.HostInterop.empty with Functions = [normalizeNameExtern] }
let externs = custom.Functions @ Registry.all hostContext

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

In that example, `run` is declared with `export let` because the host needs to find it in the loaded script, inspect its signature, and invoke it through `ScriptHost`.

## Why this pattern works well in production

- **Load once, invoke many times**: keep parsed/inferred/evaluated script state in memory.
- **Extern injection is explicit**: only functions you register are callable; `HostFunctions.create` derives the scheme and conversions from a concrete handler.
- **Resolver is host-owned**: imports can come from DB, cache, or controlled virtual FS.
- **Type resolution before invoke**: host can validate contracts before calling exports.

## Common extensions

- cache `LoadedScript` by tenant/project/version,
- enforce root and deny-path policies via `HostContext`,
- validate `ExportedFunctionSignatures` against host-side contracts,
- isolate execution in a worker process when hard time or memory limits are required; a caller-side timeout alone does not stop interpreter work.

## Add host-managed values without changing the execution bridge

When host functions exchange opaque resources with visible immutable fields, pass an `ExtensionRegistration` through the corresponding loader:

| Existing extern-list API | Registration-aware API |
| --- | --- |
| `ScriptHost.loadSource externs source` | `ScriptHost.loadSourceWithRegistration registration source` |
| `ScriptHost.loadFile externs path` | `ScriptHost.loadFileWithRegistration registration path` |
| `ScriptHost.loadSourceWithIncludes externs root entry source resolver` | `ScriptHost.loadSourceWithIncludesAndRegistration registration root entry source resolver` |

Signature inspection and `ScriptHost.invoke` continue to use the resulting `LoadedScript`. Host-managed values retain metadata and private payloads when passed to exports, returned, captured in closures, or stored in containers. Host calls validate declared argument/result types, so adapters must supply the declared `Value` shapes.

For interactive embedding, initialize `Interactive.createWithRegistration registration`; subsequent submissions retain that environment. See [Host-Managed Types](./host-managed-types) for a complete factory, script-reader, and host round-trip example.

## Load a shared external-function environment

Hosts can use `EnvironmentConfiguration.resolve` to inspect effective roots and assembly paths, then call `EnvironmentLoader.load` when provider execution is authorized. Both return `Result` values for initialization failures. The resulting registration works with the loaders above and the same import resolver pattern.

This is optional: applications can continue registering functions/types directly. A configuration supplies neither an entry script nor an import resolver. See [Environment Configuration](../reference/environment-configuration#embedding-the-same-environment) for the complete initialization example.
