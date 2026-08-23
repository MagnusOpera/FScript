# Compile FScript to Go

The Go target turns one complete FScript program into an inspectable Go project. Source generation does not invoke the Go toolchain.

## Prerequisites

- the `fscript` CLI built from a compiler-enabled FScript distribution;
- Go 1.25 or newer to format, test, or run the generated project.

Install the CLI from the repository when working on this unreleased compiler surface:

```bash
dotnet tool restore
dotnet build FScript.sln -c Release
```

## Generate a project

Given `hello.fss`:

```fsharp
let greet name = $"Hello, {name}!"

match Env.Arguments with
| name :: _ -> greet name
| [] -> greet "world"
```

generate Go source with:

```bash
fscript compile hello.fss --target go --output ./out/hello-go
```

When running from the repository instead of an installed tool:

```bash
dotnet run --project src/FScript -- \
  compile hello.fss --target go --output ./out/hello-go
```

The output is a complete project:

```text
out/hello-go/
  go.mod
  main.go
  internal/
    fscriptprogram/
      program.go
    fsruntime/
      closures.go
      environment.go
      patterns.go
      runtime.go
```

The project declares `go 1.25`, contains no Go generic declarations, and includes the FScript-owned runtime helpers it needs.

## Inspect and run

```bash
cd out/hello-go
gofmt -d .
go test ./...
go run . Ada
```

Expected output:

```text
"Hello, Ada!"
```

Generated source is already `gofmt`-compatible, so `gofmt -d .` should print nothing. `Env.Arguments` receives executable arguments. In generated Go, `Env.ScriptName` is the generated executable name rather than the original `.fss` path.

## Semantics and representations

The backend consumes validated LIR; it does not type-check source or implement pattern semantics independently.

The initial backend preserves:

- closures, currying, partial application, and recursive groups;
- tuples and concrete structs for declared records;
- concrete tagged structures for declared unions;
- structural/named record conversions;
- whole-program specialization of polymorphic lists, options, maps, and closures;
- ordered maps and recursive structural equality;
- pattern decision plans and guards;
- scalar, string, list, option, map, and portable console intrinsics;
- line-oriented stdin/stdout through `Console.readLine` and `Console.writeLine`;
- read-only text input through `Fs.readText`;
- short-circuit Boolean operations and left-to-right evaluation;
- FScript-compatible result formatting, including IEEE floating results.

The backend deliberately does not use Go generics or dynamic value carriers. Reachable HM instantiations become concrete Go types and native typed functions.

## Console input and text files

Generated programs use their process stdin and stdout:

```fsharp
Console.writeLine "What is your name?"
match Console.readLine () with
| Some name -> $"Hello, {name}!"
| None -> "No input"
```

`Console.readLine ()` returns `None` at EOF. A final line without a trailing newline is returned as `Some`. `Fs.readText path` is also portable and returns `None` when the file cannot be read; other filesystem functions remain unsupported by the Go backend.

## Go imports

A directly representable Go function can be declared without a body:

```fsharp
[<Import("strings", "ToUpper")>]
let upper : string -> string

upper "hello"
```

The initial function boundary accepts unit and primitive arguments/results, plus supported imported record types. Imported records require exported, exactly named primitive fields:

```fsharp
[<Import("example.com/acme/model", "Customer")>]
type Customer = {
  Name: string
  Seats: int
}
```

FScript does not inspect Go packages or add module versions. For non-standard-library imports, the generated project owner must configure dependencies with ordinary Go tooling, for example `go get` or an edited `go.mod`.

Unsupported generic types, channels, interfaces, embedded fields, function fields, complex pointers/maps, imported unions, and non-trivial adapters produce `FSB4xxx` diagnostics.

## Unsupported runtime features

Reachable Tasks and arbitrary host externs are not part of the initial Go runtime. Generation reports an explicit backend diagnostic. An unused unsupported declaration does not prevent generation.

The CLI prints compiler diagnostics and exits non-zero. Common diagnostic families are:

- `FSH1xxx`: typed HIR/frontend compilation;
- `FSL2xxx`: LIR lowering or validation;
- `FSB4xxx`: Go representation or linkage;
- `FSA5xxx`: output/artifact filesystem failures.

## Repeated generation

Running the same command again deterministically replaces the files owned by the generator. The backend does not run `go build`, manage a cache, or produce a binary.
