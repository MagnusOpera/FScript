---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.81.0

- Replaced script export attributes with contextual `export let` declarations while keeping `export` available as an identifier.
- Completed the standard-library and language reference and improved tutorial order, examples, and core-versus-host guidance.
- Removed the F# TypeProvider package and its compile-time script projection support.
- Added script-constructible CLR record projections with complete visible-field validation and typed conversion from script-created records.
- Added opt-in CLR attributes and helpers to derive host-managed descriptors and snapshot visible properties from real host types.
- Documented host-managed values, extension providers, and shared CLI and VS Code environments.
- Added shared FFI environment configuration for CLI execution and trusted, workspace-specific VS Code completion and diagnostics.
- Added host-managed extension types with visible fields, opaque host state, and safe structural reading and transport.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.80.1...0.81.0
