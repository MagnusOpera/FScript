---
id: host-permissions
title: Host Permissions
slug: /embedding/host-permissions
---

Hosted interpreter execution starts with **no permissions**. The host selects a policy for each registration; scripts and extension providers cannot grant themselves permissions. CLI execution and compilation keep their unrestricted behavior and do not enforce this hosting policy.

| Permission | Allows |
| --- | --- |
| <span id="none" className="permission-definition"><code>None</code></span> | Pure built-ins and registered functions declaring no capabilities. |
| <span id="io" className="permission-definition"><code>IO</code></span> | Built-in filesystem operations, console output, and host entropy through `Guid.new`. |
| <span id="interactive" className="permission-definition"><code>Interactive</code></span> | Terminal input; `Console.readLine` additionally requires `IO`. |
| <span id="threading" className="permission-definition"><code>Threading</code></span> | Every `Task.*` operation. |
| <span id="native" className="permission-definition"><code>Native</code></span> | External provider loading and calls to SDK/FFI functions. This grants trust in arbitrary .NET/native code. |

`Native` is a trust boundary: arbitrary provider code can access the host directly, so it is not confined by the interpreter's filesystem, IO, or threading checks. Tasks started directly by provider code remain the provider/host's responsibility; the runtime joins tasks started through its own Task API. Built-in implementation details such as `Math` calling the BCL do not require `Native`.

## Configure the runtime host

```fsharp
open FScript.Language
open FScript.Runtime

let context =
    { HostContext.create workspaceRoot with
        Permissions = HostPermissions.IO ||| HostPermissions.Threading
        DeniedPathGlobs = [ ".git" ] }
let registration = Registry.registration context
let loaded =
    ScriptHost.loadSourceWithRegistration registration
        "export let run () = Task.fromResult 42 |> Task.await"
let result = ScriptHost.invoke loaded "run" [VUnit]
```

`Registry.all context` filters standard externs using the context's grants. `Registry.registration context` also attaches those grants to the execution registration. Lexical `Path` and string-only `Glob` remain available without `IO`. Filesystem permission never removes `RootDirectory`, symlink checks, or `DeniedPathGlobs` restrictions.

## Register functions explicitly

Language-level `ExternalFunction` descriptors declare `RequiredPermissions`. `HostInterop.empty` and `HostInterop.ofExterns` grant `None`; use `HostInterop.ofExternsWithPermissions permissions functions` or `HostInterop.withPermissions permissions registration` to grant capabilities. Permission requirements are independent of compiler purity and thread-safety contracts. A trusted host may explicitly declare a pure handler as requiring `None`; the interpreter cannot inspect arbitrary handler code to infer effects.

SDK `ExternalFunction` record layouts are unchanged. `ExtensionBridge.toLanguageRegistration` grants no capabilities and classifies provider-owned functions as requiring `Native`. Forwarded default functions preserve their declared requirements. Use `ExtensionBridge.toLanguageRegistrationWithPermissions permissions registration` to supply the host's grants. Providers only receive the default functions the host supplied; their SDK `HostContext` has no authority to grant permissions.

`EnvironmentLoader.load` and `loadWithInfo` default to no capabilities. Their `loadWithPermissions` and `loadWithInfoAndPermissions` variants take explicit grants. Configured provider code cannot be loaded without `Native`; configuration files cannot grant it.

## Enforcement and lifetimes

Denied externs are removed before `ScriptHost` type inference. Calls are checked again at execution, including returned callables, callback application, SDK default forwarding, and canonical task operations. Checking the SDK task context additionally requires `Threading`, even when the provider itself has `Native`.

`LoadedScript` retains its policy for every exported-function invocation. Direct Language hosts use `Eval.invokeValueWithPermissionsAndExitCode permissions typeDefs functionValue arguments`; the shorter `invokeValueWithExitCode` defaults to none. REPL state retains the policy across submissions. `DebugLaunch.create` also defaults to none; embedding launchers can use `createWithPermissions`, while the CLI/editor debugger explicitly grants all capabilities. Tasks share the same immutable grants, are joined before the invocation returns, and contribute fatal failures to the final error report.

Permissions govern script operations. Host-controlled source loading and import resolvers are trusted host plumbing and may read source without granting script IO. Root/import checks remain in effect. CPU, memory, timeouts, and forced termination still require host governance.

## Migration

The Language/Runtime host API now requires permission declarations on native function descriptors, a policy on native registrations and runtime contexts, and `CheckPermissions` on custom native call contexts. Existing extern-list loading APIs remain default-deny. Use `ScriptHost.loadSourceWithPermissions` or `loadFileWithPermissions` for explicit grants, or switch to registration-based loading for includes and custom types. The independently versioned Extensibility SDK requires no ABI change. New permissions are never granted by a manifest or a provider.
