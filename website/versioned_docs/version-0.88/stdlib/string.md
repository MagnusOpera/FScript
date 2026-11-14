---
id: stdlib-string
title: String Module
slug: /stdlib/string
---

## `String.replace : string -> string -> string -> string`

Argument order is `oldValue -> newValue -> source`.

Replaces all non-overlapping occurrences using ordinal, case-sensitive matching. An empty `oldValue` returns `source` unchanged; an empty `newValue` removes matches. This operation always returns a string.

```fsharp
let text = String.replace "-" "_" "a-b-c"
```

## `String.indexOf : string -> string -> int option`

Argument order is `value -> source`. Matching is ordinal and case-sensitive; returns the first zero-based index or `None` when absent.

```fsharp
let maybeIndex = String.indexOf "script" "fscript"
```

## `String.toLower : string -> string`

Converts a string to lowercase using invariant casing.

```fsharp
let lower = String.toLower "FSCRIPT"
```

## `String.toUpper : string -> string`

Converts a string to uppercase using invariant casing.

```fsharp
let upper = String.toUpper "fscript"
```

## `String.substring : int -> int -> string -> string option`

Argument order is `start -> length -> source`.
Returns `None` for invalid ranges.

```fsharp
let maybeSub = String.substring 0 4 "fscript"
```

## `String.concat : string -> string list -> string`

Joins a list of strings with a separator.

```fsharp
let joined = String.concat "," ["a"; "b"; "c"]
```

## `String.split : string -> string -> string list`

Argument order is `separator -> source`.
Splits on the exact separator string and preserves empty entries, including trailing ones.

```fsharp
let parts = String.split "," "a,b,c"
```

## `String.endsWith : string -> string -> bool`

Argument order is `suffix -> source`.
Checks whether the source ends with the suffix.

```fsharp
let isScript = String.endsWith ".fss" "samples/hello.fss"
```

## Pure support additions

Whitespace helpers use a fixed Unicode White_Space set on every runtime: U+0009–000D, U+0020, U+0085, U+00A0, U+1680, U+2000–200A, U+2028–2029, U+202F, U+205F, and U+3000. U+FEFF is preserved.

Count/size arguments must fit `0..2147483647`; chunk sizes must also be positive. Invalid arguments raise script errors. Collection callbacks run in the documented order and may themselves have effects; these helpers are pure when their callbacks are pure.

### `String.length`

```fsharp
String.length : string -> int
```

Returns the number of UTF-16 code units, not Unicode characters or grapheme clusters.

```fsharp
String.length "abc"
// 3
```

### `String.isEmpty`

```fsharp
String.isEmpty : string -> bool
```

Tests whether the string has zero length.

```fsharp
String.isEmpty ""
// true
```

### `String.isWhiteSpace`

```fsharp
String.isWhiteSpace : string -> bool
```

Tests whether the string is empty or consists only of Unicode whitespace.

```fsharp
String.isWhiteSpace " \t"
// true
```

### `String.trim`

```fsharp
String.trim : string -> string
```

Removes Unicode whitespace from both ends.

```fsharp
String.trim " hi "
// "hi"
```

### `String.trimStart`

```fsharp
String.trimStart : string -> string
```

Removes leading Unicode whitespace.

```fsharp
String.trimStart " hi "
// "hi "
```

### `String.trimEnd`

```fsharp
String.trimEnd : string -> string
```

Removes trailing Unicode whitespace.

```fsharp
String.trimEnd " hi "
// " hi"
```

### `String.splitLines`

```fsharp
String.splitLines : string -> string list
```

Splits CRLF, LF, or CR line endings. Empty and trailing lines are preserved; empty input produces `[""]`.

```fsharp
String.splitLines "a\nb\n"
// ["a";"b";""]
```

### `String.startsWith`

```fsharp
String.startsWith : string -> string -> bool
```

Tests an ordinal, case-sensitive prefix.

```fsharp
String.startsWith "ab" "abc"
// true
```

### `String.contains`

```fsharp
String.contains : string -> string -> bool
```

Tests for an ordinal, case-sensitive substring. The empty substring matches every string.

```fsharp
String.contains "b" "abc"
// true
```

### `String.padLeft`

```fsharp
String.padLeft : int -> string -> string
```

Pads on the left with spaces to the requested UTF-16 width. Strings already at least that wide are unchanged.

```fsharp
String.padLeft 3 "a"
// "  a"
```

### `String.padRight`

```fsharp
String.padRight : int -> string -> string
```

Pads on the right with spaces to the requested UTF-16 width. Strings already at least that wide are unchanged.

```fsharp
String.padRight 3 "a"
// "a  "
```

### `String.repeat`

```fsharp
String.repeat : int -> string -> string
```

Concatenates the string the requested number of times. Zero produces the empty string.

```fsharp
String.repeat 3 "ab"
// "ababab"
```

### `String.compare`

```fsharp
String.compare : string -> string -> int
```

Returns `-1`, `0`, or `1` using ordinal UTF-16 ordering, independently of culture. This is useful for custom comparers over records.

```fsharp
String.compare "a" "b"
// -1
```
