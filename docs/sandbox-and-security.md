# Sandbox and Security Specification

## Purpose
This document defines the current security model for running FScript programs with host externs.

## Execution model
- FScript executes in-process inside the host .NET application.
- It has no native language feature for direct OS/network/process access.
- Side effects happen only through registered externs.

## Host context and filesystem boundary
- Host context includes:
  - `RootDirectory : string`
- Current CLI sets `RootDirectory` to process current working directory.

Filesystem extern restrictions:
- `Fs.readText` and `Fs.glob` resolve candidate paths through `HostCommon.tryResolvePath`.
- Access is allowed only when target is within `RootDirectory` (or exactly equal to it).
- Paths outside root are denied (`None` returned).

## What is and is not sandboxed

### Enforced today
- Path confinement for `Fs.*` externs to host root.
- Arity/type-shape checks on extern invocation.

### Not enforced today
- No CPU time limits.
- No memory limits.
- No instruction-count quotas.
- No recursion-depth guard beyond .NET runtime limits.
- No cancellation/timeouts in evaluator core.
- No separate process/container isolation.

## Data handling and failure behavior
- Many externs return `None` on operational failures:
  - file not found / invalid path
  - JSON/XML parse/decode failures
  - regex/hash exceptions
- Type misuse in script still raises `TypeException`/`EvalException`.

## Security implications
- Treat scripts as trusted or semi-trusted unless host adds stronger isolation.
- A malicious or accidental script can consume CPU/memory.
- `print` can write arbitrary text to stdout.
- `Fs.glob` can enumerate all files under root.

## Recommended hardening for production
1. Run FScript in a separate sandboxed process/container.
2. Set explicit CPU/memory/time limits.
3. Provide cancellation tokens and evaluation timeout handling.
4. Narrow `RootDirectory` to least privilege.
5. Reduce extern surface to only required functions.
6. Audit and log extern usage (especially `Fs.*`).

## Extensibility security checklist (for new externs)
- Define exact input and output contracts.
- Avoid exposing unrestricted filesystem/network/process APIs.
- Validate and normalize all host-side paths/inputs.
- Prefer safe-failure (`None`) for untrusted data parsing.
- Add tests for denial paths and edge-case behavior.
