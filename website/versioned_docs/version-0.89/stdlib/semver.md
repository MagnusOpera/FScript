---
id: stdlib-semver
title: SemVer Module
slug: /stdlib/semver
---

These core builtins implement SemVer 2.0.0 parsing and precedence. Core numbers and numeric prerelease identifiers cannot have leading zeros. Identifiers use ASCII letters, digits, and hyphens. A leading `v`, surrounding whitespace, missing components, empty identifiers, and empty `-`/`+` suffixes are rejected. Numeric components remain strings so their size is not limited by the script integer range. Comparisons use numeric precedence for numeric identifiers and ordinal ASCII ordering otherwise; a release sorts after its prereleases. Build metadata is retained when formatting but ignored for precedence.

### `SemVer.tryParse`

```fsharp
SemVer.tryParse : string -> ({ Build: string; Major: string; Minor: string; Patch: string; PreRelease: string }) option
```

Parses a strict SemVer 2.0.0 string into `{ Major: string; Minor: string; Patch: string; PreRelease: string; Build: string }`. Absent prerelease/build components are empty strings. Invalid input returns `None`.

```fsharp
SemVer.tryParse "1.2.3-rc.1+build" |> Option.map (fun v -> v.Major)
// Some "1"
```

### `SemVer.format`

```fsharp
SemVer.format : { Build: string; Major: string; Minor: string; Patch: string; PreRelease: string } -> string
```

Formats a parsed or constructed version record. Invalid components raise an error.

```fsharp
SemVer.tryParse "1.2.3-rc.1+build" |> Option.map SemVer.format
// Some "1.2.3-rc.1+build"
```

### `SemVer.compare`

```fsharp
SemVer.compare : string -> string -> int option
```

Returns `Some -1`, `Some 0`, or `Some 1` according to SemVer precedence, or `None` if either input is invalid. Build metadata does not affect precedence.

```fsharp
SemVer.compare "1.0.0-rc.2" "1.0.0-rc.10"
// Some -1
```
