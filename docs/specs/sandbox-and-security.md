# Sandbox and Security Specification

## Purpose
This document defines the security model for running FScript programs with host externs.

## Execution model
- FScript executes in-process inside the host .NET application.
- Language-level side effects occur through registered externs.
- Host configuration determines the effective capability surface.

## Host context and filesystem boundary
- Host context includes:
  - `RootDirectory : string`
- The CLI defaults `RootDirectory` to the script file directory.
- The CLI allows overriding root with `--root <path>` (or `-r <path>`).
- `import` file resolution is constrained to `RootDirectory`.
- Import paths are resolved relative to the current script file.
- Import cycles are rejected.

Filesystem extern behavior:
- `Fs.readText` and `Fs.enumerateFiles` resolve candidate paths through `HostCommon.tryResolvePath`.
- `Fs.exists`, `Fs.isFile`, `Fs.isDirectory`, `Fs.createDirectory`, and `Fs.writeText` use the same root-confined resolution.
- `Fs.glob` evaluates patterns under `RootDirectory`.
- Access is granted for paths within `RootDirectory` (or exactly equal to it).
- Out-of-bound paths return `None`/`false` depending on function shape.

## Runtime safety behavior
- Extern invocation checks arity and argument type-shape.
- Data/IO externs frequently model operational failures as `None` values.
- Script type misuse raises `TypeException`/`EvalException`.

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
