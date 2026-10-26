---
id: stdlib-console
title: Console Module
slug: /stdlib/console
---

The `Console` module exposes basic text I/O through host-provided runtime externs.

## `Console.writeLine : string -> unit`

Writes one line of text to standard output.

```fsharp
Console.writeLine "hello"
Console.writeLine $"args={Env.Arguments}"
```

## `Console.readLine : unit -> string option`

Reads one line from standard input.
Returns `None` on end-of-input.

```fsharp
match Console.readLine () with
| Some text -> Console.writeLine $"echo: {text}"
| None -> Console.writeLine "no input"
```

## Notes

- `Console.writeLine` accepts a `string`, so convert non-string values explicitly or use string interpolation.
- `Console.readLine` is a zero-argument function and should be called as `Console.readLine ()`.
- `Console.readLine` returning `None` usually means stdin is exhausted or unavailable.
- Console I/O is provided by the host runtime, not by the pure language core.

## Example

```fsharp
Console.writeLine "What is your name?"

let greeting =
  match Console.readLine () with
  | Some name -> $"Hello {name}"
  | None -> "Hello"

Console.writeLine greeting
```
