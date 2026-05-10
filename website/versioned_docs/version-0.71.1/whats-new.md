---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.71.1

- Fixed LSP go-to-definition for imported union-case usages so constructors like `IdentifierToken` navigate to the declaring case in the imported file instead of failing or landing on the enclosing type header.
- Fixed LSP go-to-definition near dotted injected calls so clicks on `Map.add` no longer jump to an adjacent local argument binding.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.71.0...0.71.1
