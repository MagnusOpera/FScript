---
id: compile-to-go
title: Compile to Go
slug: /compiler/go
---

Generate a complete, inspectable Go project from one FScript program:

```bash
fscript compile hello.fss --target go --output ./out/hello-go
```

## Example

```fsharp title="hello.fss"
let greet name = $"Hello, {name}!"

match Env.Arguments with
| name :: _ -> greet name
| [] -> greet "world"
```

The generated project contains:

```text
go.mod
main.go
internal/fscriptprogram/program.go
internal/fsruntime/closures.go
internal/fsruntime/environment.go
internal/fsruntime/patterns.go
internal/fsruntime/runtime.go
```

It declares Go 1.25, uses no Go generics, and is already `gofmt`-compatible.

## Build and run

```bash
cd out/hello-go
gofmt -d .
go test ./...
go run . Ada
```

Output:

```text
"Hello, Ada!"
```

Source generation itself does not require or invoke Go. The caller decides whether to inspect, test, run, or build the project.

## Supported semantics

The backend preserves closures, currying, recursion, concrete declared records and unions, structural conversions, patterns and guards, ordered maps, structural equality, specialized polymorphic containers, portable builtins, `Console.readLine`, `Console.writeLine`, read-only `Fs.readText`, and `Env`.

Generated code preserves FScript evaluation order and formatting rather than inheriting accidental Go behavior. Whole-program HM specialization emits concrete types without Go generics, `any`, `interface{}` value carriers, or a universal closure ABI.

`Console.readLine ()` consumes generated-program stdin and returns `None` at EOF. `Console.writeLine` writes one stdout line. `Fs.readText path` returns `Some text` when a file can be read and `None` otherwise; the remaining filesystem externs are not yet portable.

## Import Go functions

Directly representable functions can be declared without a body:

```fsharp
[<Import("strings", "ToUpper")>]
let upper : string -> string

upper "hello"
```

Imported records must have exported, exactly named primitive fields. FScript does not inspect packages or invent module versions; configure non-standard dependencies with ordinary Go tooling.

Reachable Tasks, arbitrary host externs, unsupported imported shapes, and complex adapters produce `FSB4xxx` diagnostics. Unused unsupported declarations remain inert.

See the [CLI reference](../reference/cli-reference.md) and the repository's [complete Go guide](https://github.com/MagnusOpera/FScript/blob/main/docs/guides/compile-to-go.md).
