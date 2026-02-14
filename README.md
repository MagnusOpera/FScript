![FScript logo](https://raw.githubusercontent.com/MagnusOpera/FScript/refs/heads/main/FScript.png)

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
- interpolation, pipeline operator, `typeof` type tokens, and `nameof` identifier tokens for host workflows.
- unified brace literals for records/maps (`{ Field = value }`, `{ [key] = value }`, `{}` for empty map), with map keys typed as `string` or `int`.

## Getting Started Tutorial

If you are new to FScript, start with the progressive tutorial:

- [`docs/guides/getting-started-tutorial.md`](docs/guides/getting-started-tutorial.md)

It covers installation, syntax basics, flow control, collections, pattern matching, stdlib usage, includes, and host/export concepts.

## Installation

### CLI via Homebrew
```bash
brew install magnusopera/tap/fscript
```

### Embeddable language via NuGet
- `MagnusOpera.FScript.Language`
- `MagnusOpera.FScript.Runtime`

## VS Code Extension

FScript has a first-party VS Code extension with syntax highlighting and Language Server features (diagnostics, completion, hover, symbols, go-to-definition, references, rename, semantic tokens, inlay hints).

- VS Code Marketplace: `https://marketplace.visualstudio.com/items?itemName=MagnusOpera.fscript`
- Open VSX: `https://open-vsx.org/extension/MagnusOpera/fscript`

The extension uses automatic .NET runtime acquisition via `.NET Install Tool` and falls back to `dotnet` from `PATH` when needed.
Source lives in [`vscode-fscript/`](vscode-fscript/).

## Repository Development

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
- [`samples/types-showcase.fss`](samples/types-showcase.fss)
- [`samples/patterns-and-collections.fss`](samples/patterns-and-collections.fss)
- [`samples/tree.fss`](samples/tree.fss)
- [`samples/mutual-recursion.fss`](samples/mutual-recursion.fss)
- [`samples/includes-and-exports.fss`](samples/includes-and-exports.fss)

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

Built-in host extern families include `Fs.*`, `Json.*`, `Xml.*`, `Regex.*`, hashing, GUIDs, and `print`.
`List.*`, `Map.*`, and `Option.*` are provided by the embedded stdlib prelude.

For details and extension workflow, see [`docs/specs/external-functions.md`](docs/specs/external-functions.md).

## Sandbox and Security

FScript runs in-process. Security is capability-based:
- scripts can only do what exposed externs allow,
- core language evaluation is deterministic over in-memory values,
- side effects and external I/O are controlled at host extern boundaries.

Operational controls (timeouts, cancellation, resource limits, process/container isolation) are host responsibilities.

See [`docs/specs/sandbox-and-security.md`](docs/specs/sandbox-and-security.md) for the full model and checklist.

## Documentation

- Documentation portal: [`docs/README.md`](docs/README.md)
- Tutorial: [`docs/guides/getting-started-tutorial.md`](docs/guides/getting-started-tutorial.md)
- Specifications index: [`docs/specs/README.md`](docs/specs/README.md)
- Architecture index: [`docs/architecture/README.md`](docs/architecture/README.md)
- FScript vs F# / OCaml: [`docs/guides/fsharp-ocaml-differences.md`](docs/guides/fsharp-ocaml-differences.md)

## Changelog

- [`CHANGELOG.md`](CHANGELOG.md)

## License

This project is licensed under the MIT License.  
See [`LICENSE`](LICENSE).
