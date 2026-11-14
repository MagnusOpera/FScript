---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.89

### 0.89.0

- Remove redundant compiler tests and speed up compiled usage-message coverage.
- Prevent publishing tests from waiting on output pipes held open by idle build workers.
- Join unobserved tasks before script and host invocation returns, and collect all foreground and background failures.
- Put task continuation callbacks first so continueWith, whenAll, and whenAny compose naturally in pipelines.
- Add Task.delay, Task.fromResult, Task.continueWith, Task.whenAll, Task.whenAny, and Task.awaitAll with debugger, compiler, and editor support.
- Reject heterogeneous list elements consistently, including tasks with different result types.
- Keep constant locals captured by nested functions valid during compiler simplification.
- Preserve function-valued results when compiling task awaits and other generic external calls.
- Preserve nested task results and enforce task observation at compiled program Run boundaries.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.88.0...0.89.0
