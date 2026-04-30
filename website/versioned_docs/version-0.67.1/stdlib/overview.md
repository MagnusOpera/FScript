---
id: stdlib-overview
title: Stdlib Overview
slug: /stdlib/overview
---

FScript ships with a built-in standard library and a small set of always-available top-level built-ins.

Everything in this section is available in every script without imports.

## Modules

- `List`
- `Option`
- `Map`
- `String`
- `Int`
- `Float`
- `Bool`

## Top-level built-ins

- `Env`
- `print`
- `ignore`

## Core rules

- Functions are curried.
- Pipe-friendly usage is preferred.
- Map keys are always `string` (map indexer type is fixed to string).
- Many parsing/indexing operations return `option` instead of throwing.

## Built-in types

- `type Environment = { ScriptName: string option; Arguments: string list }`
- `type FsKind = File of string | Directory of string | Missing`

## Where to go next

- [Built-ins](./builtins) for `Env`, `Environment`, `FsKind`, `print`, and `ignore`
- [Native Types Reference](/manual/reference/native-types) for list/map indexers and native type access forms
- [List Module](./list)
- [Option Module](./option)
- [Map Module](./map)
- [String Module](./string)
- [Int, Float, Bool Modules](./scalars)
