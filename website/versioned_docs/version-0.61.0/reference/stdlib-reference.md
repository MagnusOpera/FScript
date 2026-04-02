---
id: stdlib-reference
title: Stdlib and Built-ins
slug: /reference/stdlib
---

The complete standard library documentation now lives in the dedicated **Stdlib** section of this manual.

Start here:

- [Stdlib Overview](/manual/stdlib/overview)
- [List Module](/manual/stdlib/list)
- [Option Module](/manual/stdlib/option)
- [Map Module](/manual/stdlib/map)
- [String Module](/manual/stdlib/string)
- [Int, Float, Bool Modules](/manual/stdlib/scalars)
- [Stdlib Recipes](/manual/stdlib/recipes)

This page summarizes how stdlib and runtime externs differ.

## Core built-in groups

- `List.*`
- `Option.*`
- `Map.*`
- `String.*`
- parsing helpers: `Int.tryParse`, `Float.tryParse`, `Bool.tryParse`

## Runtime extern families

- `Fs.*` filesystem utilities
- `Json.*`
- `Xml.*`
- `Regex.*`
- hashing and GUID helpers

## `print`

```fsharp
print "hello"
```

Use `print` for quick script output and debugging.
