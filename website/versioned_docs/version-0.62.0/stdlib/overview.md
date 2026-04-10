---
id: stdlib-overview
title: Stdlib Overview
slug: /stdlib/overview
---

FScript ships with a built-in standard library.

It is loaded automatically and available in every script.

## Modules

- `List`
- `Option`
- `Map`
- `String`
- `Int`
- `Float`
- `Bool`

## Core rules

- Functions are curried.
- Pipe-friendly usage is preferred.
- Map keys are always `string` (map indexer type is fixed to string).
- Many parsing/indexing operations return `option` instead of throwing.

Continue with the module pages for full reference and examples.
