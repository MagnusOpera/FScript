# Sandbox and Security Specification

## Purpose
This document defines the security model for running FScript programs with host externs.

## Execution model
- FScript executes in-process inside the host .NET application.
- Language-level side effects occur through registered externs.
- Host configuration determines the effective capability surface.
- `Task.spawn` runs FScript thunks concurrently on the host runtime thread pool.
- Concurrent task side effects may interleave with the main script and with other tasks.

## Host context and filesystem boundary
- Host context includes:
  - `RootDirectory : string`
  - `DeniedPathGlobs : string list` (glob patterns matched against root-relative paths)
- The CLI defaults `RootDirectory` to the script file directory in file mode (`fscript script.fss`).
- In stdin and REPL modes (`cat script.fss | fscript`, `fscript`), the CLI defaults `RootDirectory` to the current working directory.
- The CLI allows overriding root with `--root <path>` (or `-r <path>`).
- `import` file resolution is constrained to `RootDirectory`.
- Import paths are resolved relative to the current script file.
- Import paths that traverse existing symlink/reparse-point components are rejected.
- Import cycles are rejected.
- `import` is only available for file-based execution; stdin source mode rejects `import`.

Filesystem extern behavior:
- `Fs.readText` and `Fs.enumerateFiles` resolve candidate paths through `HostCommon.tryResolvePath`.
- `Fs.exists`, `Fs.kind`, `Fs.createDirectory`, and `Fs.writeText` use the same root-confined resolution.
- `Fs.glob` evaluates patterns under `RootDirectory`.
- Access is granted for paths within `RootDirectory` (or exactly equal to it).
- Out-of-bound paths return `None`/`false`/`FsKind.Missing` depending on function shape.
- Existing symlink/reparse-point components from `RootDirectory` to the target are treated as unsafe and out-of-bound.
- Denied paths are matched by glob against root-relative paths.
- Examples: `.git` blocks only the root `.git` subtree, `**/node_modules` blocks all nested `node_modules` subtrees.
- Denied entries are hidden from `Fs.glob` and `Fs.enumerateFiles`.
- `Fs.glob` and `Fs.enumerateFiles` prune denied and unsafe directories before descending into them.
- Read/write/mkdir operations on denied targets fail immediately with `EvalException`.

## Runtime safety behavior
- Extern invocation checks arity and argument type-shape.
- Data/IO externs frequently model operational failures as `None` values.
- Script type misuse raises `TypeException`/`EvalException`.
- Task failures are fatal runtime errors, consistent with foreground evaluation.
- Programs that finish with unawaited tasks fail at runtime instead of silently detaching background work.

## Resource-governance model
- Evaluator execution currently relies on host/runtime process limits.
- CPU, memory, timeout, and cancellation governance are host-level concerns.
- Process/container isolation is a host deployment choice.

## Operational guidance
- Use least-privilege `RootDirectory` settings.
- Register only required extern functions.
- Add host-level timeout/cancellation and resource limits for production.
- Monitor and audit extern usage, especially filesystem externs.

## Extensibility security checklist (for new externs)
- Define exact input and output contracts.
- Validate and normalize host-side inputs and paths.
- Keep capability scope narrow.
- Model recoverable operational failures with `option` values.
- Add tests for allow/deny paths and edge cases.

## Configured providers in editor workspaces

Environment manifests do not sandbox provider code. VS Code Restricted Mode permits discovery and parsing, but blocks configured assembly execution on both sides of the language-client boundary. Default static analysis remains available. Executing a script also trusts providers from its automatically discovered manifest; `--no-config` opts out. See [environment configuration](./environment-configuration.md#trust-and-boundaries).

## Native provider dependencies on macOS

The signed CLI allows JIT and disables hardened-runtime library validation so explicitly configured providers can load native dependencies signed by other teams (for example GLFW in the FFI sample). Providers execute only through assembly flags or a selected manifest, including CLI manifest discovery beside the entry script or in the stdin/REPL working directory. `--no-config` disables manifest discovery; the editor gates configured provider execution on workspace trust. Provider code and its native dependencies execute with the host process's permissions and must be trusted.

## Independent extension contract

Assembly providers use `FScript.Extensibility` attributes and return SDK `ExternalFunction list` or `ExtensionRegistration`, with no arguments or one sealed SDK `HostContext`. Existing provider assemblies must rebuild. SDK helpers and values do not depend on interpreter assemblies. Runtime adapts registrations using `ExtensionBridge.toLanguageRegistration`; native embedding APIs retain their Language representations. Authoring helpers live in the SDK, not Runtime.

Provider trust, atomic loading, configuration selection, and reload rules are unchanged. SDK callback/task handles are valid only in their originating active evaluation; handler contexts expire on return. Runtime enforces limits and rejects foreign/expired handles. An SDK `ExtensionException` carries a message and optional inner cause; callback errors preserve script locations.

## NuGet extension environments

CLI manifest selection can restore and execute NuGet providers as well as local DLLs. These providers and native dependencies are trusted host code, outside script filesystem/runtime-limit enforcement. `--no-config` disables automatic manifest providers. VS Code gates both NuGet restoration and provider execution on workspace trust; parsing and discovery remain available in Restricted Mode. Package build targets/scripts are not run. Restore/source/provider errors fail the environment atomically.

## Debugger inspection

VS Code gates debug launch on workspace trust before probing the CLI or executing
providers. Script execution retains the normal environment and sandbox policy.
Debug adapter stdio is separate from terminal script I/O; a random per-launch
token authenticates the loopback control connection.

Debugger variables and watches expose only registered visible host fields. They
never reflect over opaque payloads or execute functions to render values. Watches
use a syntactic inspection allowlist; unclassified expressions are rejected.
Debugging grants no variable-editing or arbitrary-evaluation capability.
