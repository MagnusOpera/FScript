---
id: stdlib-path
title: Path Module
slug: /stdlib/path
---

All functions in this module are core builtins. They manipulate path strings without filesystem access. Rules are fixed on every platform: `/` is the only separator, comparison is case-sensitive and ordinal, and backslashes/drive letters are ordinary characters. The base for relative paths must be supplied explicitly. These functions do not inspect symlinks, check existence, or prove filesystem containment. Normalize inputs explicitly before glob matching if desired.

### `Path.normalize`

```fsharp
Path.normalize : string -> string
```

Collapses repeated `/`, `.` segments, and resolvable `..` segments. Absolute paths cannot ascend above `/`; unresolved leading parents in relative paths are preserved. Removes trailing separators. Empty relative results become `.`.

```fsharp
Path.normalize "a/./b/../c"
// "a/c"
```

### `Path.isAbsolute`

```fsharp
Path.isAbsolute : string -> bool
```

Tests for a leading `/`. Drive letters and backslashes have no special meaning.

```fsharp
Path.isAbsolute "/a"
// true
```

### `Path.combine`

```fsharp
Path.combine : string -> string -> string
```

Joins and normalizes two paths. An absolute right argument replaces the left argument.

```fsharp
Path.combine "a/b" "../c"
// "a/c"
```

### `Path.fileName`

```fsharp
Path.fileName : string -> string
```

Returns the last segment after trimming trailing `/` characters. The root path produces the empty string.

```fsharp
Path.fileName "a/b.txt"
// "b.txt"
```

### `Path.extension`

```fsharp
Path.extension : string -> string option
```

Returns the final extension including its dot, or `None` for a dotfile, absent extension, or trailing dot.

```fsharp
Path.extension "a/b.txt"
// Some ".txt"
```

### `Path.fileNameWithoutExtension`

```fsharp
Path.fileNameWithoutExtension : string -> string
```

Returns the last segment without its final extension. A leading dot alone does not introduce an extension.

```fsharp
Path.fileNameWithoutExtension "a/b.txt"
// "b"
```

### `Path.parent`

```fsharp
Path.parent : string -> string option
```

Returns the parent of a normalized path. A single relative filename has parent `.`; `/` and `.` have no parent.

```fsharp
Path.parent "a/b.txt"
// Some "a"
```

### `Path.relative`

```fsharp
Path.relative : string -> string -> string option
```

Computes a lexical path from the supplied base directory to the target. Returns `None` for mixed absolute/relative paths or unresolved parents remaining in the base after removing the common prefix. Equal paths return `Some "."`.

```fsharp
Path.relative "/a/b" "/a/c/file"
// Some "../c/file"
```
