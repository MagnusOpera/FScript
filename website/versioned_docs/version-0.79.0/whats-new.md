---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.79.0

- Added VS Code syntax and semantic highlighting for numeric type annotations.
- Documented numeric specialization and the language type hierarchy.
- Added the numeric meta-type so arithmetic functions specialize to int or float and reject nonnumeric or mixed operands.
- Fixed remainder typing to accept integers only, matching runtime behavior.
- Restored CI builds on current GitHub runners by removing the obsolete F# parallel-compilation switch.
- Fixed documentation assets and links for hosting the website at `fscript.magnusopera.io`.
- Rewrote the website around concrete FScript behavior and removed vague product slogans.
- Fixed oversized homepage headings to wrap in balanced lines across desktop widths.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.78.1...0.79.0
