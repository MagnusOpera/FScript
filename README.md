# FScript
[![Build (main)](https://github.com/MagnusOpera/FScript/actions/workflows/ci-main.yml/badge.svg?branch=main)](https://github.com/MagnusOpera/FScript/actions/workflows/ci-main.yml)
[![NuGet Language](https://img.shields.io/nuget/v/MagnusOpera.FScript.Language?label=NuGet%20Language)](https://www.nuget.org/packages/MagnusOpera.FScript.Language)
[![NuGet Runtime](https://img.shields.io/nuget/v/MagnusOpera.FScript.Runtime?label=NuGet%20Runtime)](https://www.nuget.org/packages/MagnusOpera.FScript.Runtime)

FScript is a lightweight, embeddable interpreter with an F#/ML-style language.

It is designed for host applications that need:
- a concise functional scripting language,
- strong static checks (Hindley-Milner type inference),
- controlled host extensibility,
- and a clear sandbox/security boundary.

## Why FScript

- **F#/ML lineage**: immutable data, expressions-first style, pattern matching, `let`/`fun`, algebraic modeling.
- **Interpreter pipeline**: lexer -> parser -> type inference -> evaluator.
- **Host-first extensibility**: add external functions with explicit type schemes and runtime implementations.
- **Security-aware embedding**: script capabilities are defined by what the host exposes.

## Language Snapshot

FScript currently includes:
- bindings and functions: `let`, `let rec`, `and` mutual recursion, lambdas,
- control flow: `if/elif/else`, `match`, `for ... in ... do`,
- data: list, option, tuple, map, record, discriminated unions,
- pattern matching: list, option, tuple, record, union cases,
- optional type annotations on parameters,
- type declarations: records and unions (including recursive forms),
- interpolation, pipeline operator, and `typeof` type tokens for host workflows.

## Quick Start

### Build
```bash
make build
```

### Test
```bash
make test
```

### Run a script
```bash
dotnet run --project src/FScript -- samples/types-showcase.fss
```

Optional sandbox root override:
```bash
dotnet run --project src/FScript -- --root /tmp/sandbox samples/types-showcase.fss
```

Useful samples:
- `samples/types-showcase.fss`
- `samples/patterns-and-collections.fss`
- `samples/tree.fss`
- `samples/mutual-recursion.fss`

## Installation

### CLI via Homebrew
```bash
brew install magnusopera/tap/fscript
```

### Embeddable language via NuGet
- `MagnusOpera.FScript.Language`
- `MagnusOpera.FScript.Runtime`

## Interpreter Architecture

The core engine lives in `src/FScript.Language` and runs in four stages:
1. **Lexing**: indentation-aware tokenization.
2. **Parsing**: AST construction with expression/layout rules.
3. **Type inference**: Hindley-Milner inference + unification + optional annotations.
4. **Evaluation**: typed AST evaluation with immutable values and pattern matching.

Host integration lives in `src/FScript.Runtime`.

## Extensibility Model

FScript is extended through host-provided externs.

Each extern declares:
- a public name (for script calls),
- a type scheme,
- arity,
- implementation.

Built-in extern families include `List.*`, `Map.*`, `Option.*`, `Fs.*`, `Json.*`, `Xml.*`, `Regex.*`, hashing, GUIDs, and `print`.

For details and extension workflow, see [`docs/external-functions.md`](docs/external-functions.md).

## Sandbox and Security

FScript runs in-process. Security is capability-based:
- scripts can only do what exposed externs allow,
- core language evaluation is deterministic over in-memory values,
- side effects and external I/O are controlled at host extern boundaries.

Operational controls (timeouts, cancellation, resource limits, process/container isolation) are host responsibilities.

See [`docs/sandbox-and-security.md`](docs/sandbox-and-security.md) for the full model and checklist.

## Documentation

- [`docs/syntax-and-indentation.md`](docs/syntax-and-indentation.md)
- [`docs/supported-types.md`](docs/supported-types.md)
- [`docs/function-annotations.md`](docs/function-annotations.md)
- [`docs/external-functions.md`](docs/external-functions.md)
- [`docs/embedding-fscript-language.md`](docs/embedding-fscript-language.md)
- [`docs/sandbox-and-security.md`](docs/sandbox-and-security.md)
- [`docs/fsharp-ocaml-differences.md`](docs/fsharp-ocaml-differences.md)
- [`docs/assemblies-and-roles.md`](docs/assemblies-and-roles.md)

## License

This project is licensed under the MIT License.  
See [`LICENSE`](LICENSE).
