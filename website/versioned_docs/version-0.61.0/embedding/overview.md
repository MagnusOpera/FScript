---
id: embedding-overview
title: Embedding Overview
slug: /embedding/overview
---

FScript is designed for host applications that embed a scripting language safely.

## Embedding workflow

1. Reference language/runtime packages.
2. Build a host context.
3. Register extern functions your scripts can call.
4. Load scripts (with include resolution if needed).
5. Resolve exported function signatures.
6. Execute exported functions and handle results/errors.

## NuGet packages

- [`MagnusOpera.FScript.Language`](https://www.nuget.org/packages/MagnusOpera.FScript.Language)
- [`MagnusOpera.FScript.Runtime`](https://www.nuget.org/packages/MagnusOpera.FScript.Runtime)
- [`MagnusOpera.FScript.TypeProvider`](https://www.nuget.org/packages/MagnusOpera.FScript.TypeProvider)

## Recommended reading order

1. [Real-World Embedding (Load, Resolve Type, Execute)](./real-world-embedding)
2. [F# Type Provider and Use Cases](./type-provider)
3. [Register Extern Functions](./register-externs)
4. [Resolver and Includes](./resolver-and-includes)
5. [Sandbox and Safety](./sandbox-and-safety)
