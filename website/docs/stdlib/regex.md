---
id: stdlib-regex
title: Regex Module
slug: /stdlib/regex
---

**Hosted interpreter permissions:** Every function and value on this page requires no host permissions. Higher-order functions still enforce the requirements of their callbacks. These checks do not apply to CLI execution or compilation. See [Host permissions](../embedding/host-permissions).

`Regex` is provided by the default .NET runtime extern registry and uses .NET regular-expression syntax.

## `Regex.matchGroups : string -> string -> string list option`

<a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a>

Argument order is `pattern -> input`. Returns the capture-group strings from the **first** match, in group-number order. Group zero (the entire match) is excluded. A successful match without capture groups returns `Some []`; no match or an invalid pattern returns `None`. An unmatched optional group contributes an empty string.

```fsharp
let parts = Regex.matchGroups "([a-z]+)-([0-9]+)" "build-42"
// Some ["build"; "42"]
```

The API does not set a match timeout. Hosts processing untrusted patterns or large inputs should expose a bounded alternative or enforce execution limits outside the interpreter.
