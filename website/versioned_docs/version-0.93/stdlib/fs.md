---
id: stdlib-fs
title: Fs Module
slug: /stdlib/fs
---

**Hosted interpreter permissions:** Filesystem reads, writes, inspection, and enumeration require `IO`. `Fs.combinePath`, `Fs.parentDirectory`, `Fs.extension`, and `Fs.fileNameWithoutExtension` require none. These checks do not apply to CLI execution or compilation. See [Host permissions](../embedding/host-permissions).

`Fs` provides filesystem operations through the default .NET runtime extern registry. Paths used for I/O resolve against the host's `RootDirectory`, not the script's import alias or the current imported file. The CLI defaults to the entry script's directory; [environment configuration](../reference/environment-configuration) and `--root` can override it.

## Read and inspect

| Function | Signature | Behavior | Host permissions |
| --- | --- | --- | --- |
| `Fs.readText` | `string -> string option` | Reads the whole file; missing or unsafe paths return `None`. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| `Fs.exists` | `string -> bool` | Tests whether a file or directory exists; denied or unsafe paths return `false`. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| `Fs.kind` | `string -> FsKind` | Returns `File path`, `Directory path`, or `Missing`. Returned paths are root-relative with `/` separators. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |

```fsharp
let text = Fs.readText "settings.txt" |> Option.defaultValue ""
match Fs.kind "settings.txt" with
| File path -> Console.writeLine $"file: {path}"
| Directory path -> Console.writeLine $"directory: {path}"
| Missing -> Console.writeLine "missing"
```

## Create and write

| Function | Signature | Argument order and behavior | Host permissions |
| --- | --- | --- | --- |
| `Fs.createDirectory` | `string -> bool` | Creates the directory and its missing parents. Existing directories succeed. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| `Fs.writeText` | `string -> string -> bool` | `path -> content`; writes/replaces the file and creates missing parent directories. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |

These functions return `false` for unsafe paths or ordinary create/write failures. Denied paths raise an evaluation error.

```fsharp
let saved = Fs.writeText "output/report.txt" "report ready"
Console.writeLine $"saved={saved}"
```

## Work with path strings

These helpers manipulate strings without reading the filesystem or checking the sandbox. Path interpretation follows the host platform.

| Function | Signature | Behavior | Host permissions |
| --- | --- | --- | --- |
| `Fs.combinePath` | `string -> string -> string` | `left -> right`; combines segments and normalizes separators to `/`. | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| `Fs.parentDirectory` | `string -> string option` | Returns the parent portion, or `None` if absent. | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| `Fs.extension` | `string -> string option` | Returns the extension including `.`, or `None` if absent. | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |
| `Fs.fileNameWithoutExtension` | `string -> string` | Returns the final filename without its extension. | <a className="permission-tag permission-tag--none" href="../embedding/host-permissions#none" title="None permission definition">None</a> |

```fsharp
let path = Fs.combinePath "src" "main.fss"
let extension = Fs.extension path // Some ".fss"
let name = Fs.fileNameWithoutExtension path // "main"
```

## Discover files

| Function | Signature | Behavior | Host permissions |
| --- | --- | --- | --- |
| `Fs.glob` | `string -> string list option` | Searches under the root and matches root-relative paths. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |
| `Fs.enumerateFiles` | `string -> string -> string list option` | `directory -> pattern`; matches paths relative to that directory and returns root-relative paths. | <a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> |

```fsharp
let scripts = Fs.glob "**/*.fss" |> Option.defaultValue []
let directFiles = Fs.enumerateFiles "src" "*.fss" |> Option.defaultValue []
let nestedFiles = Fs.enumerateFiles "src" "**/*.fss" |> Option.defaultValue []
```

`*` matches within one path segment; `**` crosses directories; `**/` also permits zero directories; `?` matches one character. Separators normalize to `/`. A separator-free `enumerateFiles` pattern searches only the selected directory. A pattern containing `/` allows recursive traversal.

A valid search with no matches returns `Some []`. Invalid, unavailable, or unsafe search roots return `None`. Denied files are hidden, denied or symlink/reparse-point directories are skipped, and ordering is not guaranteed. Some inaccessible directories may be skipped rather than fail the whole search.

## Filesystem boundaries

Root checks reject paths that escape the root and symlink/reparse-point paths. Hosts can also set root-relative `DeniedPathGlobs`. Denied reads raise an error; `exists` and `kind` hide denied entries. `readText` can propagate I/O errors for an existing unreadable file; its option return is not a general exception handler.

These rules apply to built-in filesystem externs. Custom .NET providers are executable host code and must enforce their own access policy. See [Sandbox and Safety](../embedding/sandbox-and-safety).
