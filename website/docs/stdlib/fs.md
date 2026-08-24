---
id: stdlib-fs
title: Fs Module
slug: /stdlib/fs
---

The `Fs` module provides filesystem and path helpers through the default runtime extern registry.

## Functions

- `Fs.readText : string -> string option`
- `Fs.exists : string -> bool`
- `Fs.kind : string -> FsKind`
- `Fs.createDirectory : string -> bool`
- `Fs.writeText : string -> string -> bool`
- `Fs.combinePath : string -> string -> string`
- `Fs.parentDirectory : string -> string option`
- `Fs.extension : string -> string option`
- `Fs.fileNameWithoutExtension : string -> string`
- `Fs.glob : string -> string list option`
- `Fs.enumerateFiles : string -> string -> string list option`

`FsKind` is `File of string | Directory of string | Missing`. Glob results are sorted normalized paths. A pattern without a path separator searches only the requested directory; patterns such as `**/*.fs` search recursively.

```fsharp
match Fs.kind "samples/basic/basic.fss" with
| File path -> Fs.readText path
| _ -> None
```

The .NET host applies its configured root and denied-path sandbox. Generated Go uses paths relative to the native process working directory and its operating-system permissions. All functions on this page are supported by the Go backend.
