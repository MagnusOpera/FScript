---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.94

### 0.94.0

- Preserve verifiable IL when opted-in FFI loops execute inside larger expressions.
- Reject FFI expansion that would reference inaccessible generic method type arguments.
- Add reproducible FFI inlining tests for argument effects, exceptions, closures, and IL control flow.
- Add reproducible four-way backend conformance tests with bounded typed programs, shrinking, and replay.
- Add reproducible property coverage for all default runtime FFI providers, internal codecs, and permission registration.
- Reject non-finite floats during JSON serialization instead of returning invalid JSON across source, compiled, and SDK calls.
- Add reproducible mailbox state-model tests for FIFO delivery, bounded queues, shutdown, and handler failures.
- Extend reproducible inference tests to generic records and unions containing opaque instances and independent callbacks.
- Preserve independent opaque factory types when factories pass through union constructors.
- Add reproducible property tests for type generalization and captured opaque instances, with shrinking and failure replay.
- Preserve opaque instance types through captured closures while keeping unrelated callbacks polymorphic.
- Report concurrent host access from mailbox handlers, state, and messages, respecting completion ordering.
- Document Mailbox usage, shutdown, and the Threading permission in the standard library reference.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.93.0...0.94.0
