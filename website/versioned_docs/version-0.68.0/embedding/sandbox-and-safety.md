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
- Use cancellation and timeouts for execution control.
- Treat script execution as untrusted input handling.

## Recommended defaults

1. Expose read-only externs first.
2. Add write/network capability only when required.
3. Log extern calls for observability.
