---
id: embedding-overview
title: Embedding Overview
slug: /embedding/overview
---

FScript is designed for host applications that embed a scripting language safely.

## Embedding workflow

1. Reference Language/Runtime for embedding and the independent Extensibility SDK for provider authoring.
2. Build a host context.
3. Register [typed extern handlers](./register-externs.md) and, when needed, [host-provided types](./host-managed-types.md) your scripts can use.
4. Load scripts (with include resolution if needed).
5. Resolve exported function signatures.
6. Execute exported functions and handle results/errors.

## NuGet packages

- [`MagnusOpera.FScript.Language`](https://www.nuget.org/packages/MagnusOpera.FScript.Language)
- [`MagnusOpera.FScript.Runtime`](https://www.nuget.org/packages/MagnusOpera.FScript.Runtime)
- [`MagnusOpera.FScript.Extensibility`](https://www.nuget.org/packages/MagnusOpera.FScript.Extensibility) — independently versioned provider contracts and CLR helpers.

## Recommended reading order

1. [Real-World Embedding (Load, Resolve Type, Execute)](./real-world-embedding)
2. [Fable and JavaScript Hosts](./fable-javascript)
3. [Register Extern Functions](./register-externs)
4. [Host-Provided Types](./host-managed-types)
5. [Environment Configuration](../reference/environment-configuration)
6. [Resolver and Includes](./resolver-and-includes)
7. [Sandbox and Safety](./sandbox-and-safety)

## Executable graphics sample

The [FFI graphics walkthrough](../examples/ffi-graphics.md) puts geometry, camera, lighting, and animation in FScript. A typed .NET provider uploads the generated Utah teapot and renders it with OpenGL. It demonstrates script-constructible scene records, opaque window/mesh resources, typed function adapters, and editor discovery through `fscript.toml`. Run it with `fscript samples/ffi/main.fss`; use `-- --headless` to exercise the scripts without a display.
