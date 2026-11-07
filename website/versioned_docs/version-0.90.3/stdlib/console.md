---
id: stdlib-console
title: Console Module
slug: /stdlib/console
---

**Hosted interpreter permissions:** `Console.writeLine` requires `IO`. `Console.readLine` requires both `IO` and `Interactive`. These checks do not apply to CLI execution or compilation. See [Host permissions](../embedding/host-permissions).

The `Console` module exposes basic text I/O through host-provided runtime externs.

## `Console.writeLine : string -> unit`

<a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a>

Writes one line of text to standard output.

```fsharp
Console.writeLine "hello"
Console.writeLine $"args={Env.Arguments}"
```

## `Console.readLine : unit -> string option`

<a className="permission-tag permission-tag--io" href="../embedding/host-permissions#io" title="IO permission definition">IO</a> <a className="permission-tag permission-tag--interactive" href="../embedding/host-permissions#interactive" title="Interactive permission definition">Interactive</a>

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
