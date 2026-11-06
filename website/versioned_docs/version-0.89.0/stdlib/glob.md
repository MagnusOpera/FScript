---
id: stdlib-glob
title: Glob Module
slug: /stdlib/glob
---

These core builtins match supplied strings only. They never enumerate files or directories; `Fs.glob` remains the separate filesystem operation.

Patterns use `/` separators and ordinal, case-sensitive matching. `*` matches zero or more UTF-16 code units within one segment; `?` matches one code unit; `**` matches zero or more whole nonempty segments and must occupy an entire pattern segment. Other characters are literal: there are no character classes, brace expansion, negation, or escape syntax. Leading dots are ordinary characters. Inputs are not normalized, and leading/trailing separators participate in matching.

### `Glob.isMatch`

```fsharp
Glob.isMatch : string -> string -> bool
```

Matches the entire supplied path against the pattern. Invalid use of `**` raises an error.

```fsharp
Glob.isMatch "**/*.fs" "Parser.fs"
// true
```

### `Glob.filter`

```fsharp
Glob.filter : string -> string list -> string list
```

Filters supplied paths in their original order using the same matching rules. Performs no directory enumeration.

```fsharp
Glob.filter "src/*.fs" ["src/a.fs";"src/sub/b.fs";"src/a.txt"]
// ["src/a.fs"]
```
