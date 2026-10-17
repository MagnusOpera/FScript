---
id: sandbox-and-safety
title: Sandbox and Safety
slug: /embedding/sandbox-and-safety
---

FScript security is capability-based.

Scripts can only perform actions that your host exposes.

## Practical safety model

- Do not expose risky externs by default.
- Restrict filesystem scope with root/deny policies.
- Treat symlinks and reparse points as sandbox boundary risks; built-in `Fs.*` externs reject those paths.
- Enforce CPU, memory, and execution-time limits in the host; use process isolation when termination must be guaranteed. The interpreter does not provide a general script cancellation API.
- Treat script execution as untrusted input handling.

## Recommended defaults

1. Expose read-only externs first.
2. Add write/network capability only when required.
3. Log extern calls for observability.

## Assembly providers and opaque host state

A configured provider is executable .NET code. Registration may have side effects or access native code; `HostContext` and `fscript.toml` do not sandbox it. The CLI trusts explicitly supplied configurations/assemblies. VS Code gates configured execution on workspace trust while retaining default analysis in Restricted Mode. Hosts can parse and inspect effective configuration without loading providers; see [Environment Configuration](../reference/environment-configuration).

[Host-managed values](./host-managed-types) prevent scripts from constructing or updating opaque host resources and expose only registered visible fields. They preserve private payloads through calls and containers, but do not supply a disposal protocol or resource isolation. The host owns lifetime, synchronization, cloning, and the capabilities available through registered functions.
