---
id: stdlib-regex
title: Regex Module
slug: /stdlib/regex
---

`Regex` is provided by the default .NET runtime extern registry and uses .NET regular-expression syntax.

## `Regex.matchGroups : string -> string -> string list option`

Argument order is `pattern -> input`. Returns the capture-group strings from the **first** match, in group-number order. Group zero (the entire match) is excluded. A successful match without capture groups returns `Some []`; no match or an invalid pattern returns `None`. An unmatched optional group contributes an empty string.

```fsharp
let parts = Regex.matchGroups "([a-z]+)-([0-9]+)" "build-42"
// Some ["build"; "42"]
```

The API does not set a match timeout. Hosts processing untrusted patterns or large inputs should expose a bounded alternative or enforce execution limits outside the interpreter.
