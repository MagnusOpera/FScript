---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.78.0

- Fixed concurrent type inference potentially reusing fresh type-variable identities.
- Updated the Fable compiler, core library, and packaging SDK to their latest stable releases.
- Kept language-server hints responsive in erroneous files by inferring each recoverable declaration only once.
- Made REPL and JavaScript sessions infer and evaluate new submissions incrementally instead of replaying session history.
- Reduced type inference allocation by avoiding no-op substitution, environment, generalization, and record-key work.
- Reduced script type-checking overhead by skipping editor-only local-variable telemetry during ordinary inference.
- Speed up non-recursive filesystem enumeration patterns without changing their results.
- Fixed the documentation site to identify the latest released version as actively maintained.
- Restored the unreleased-version notice on the `Next` documentation.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.77.1...0.78.0
