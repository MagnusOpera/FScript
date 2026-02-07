# FScript Assemblies, Roles, and Usage

## Purpose
This document describes how FScript is split into assemblies and how they are used together in applications.

## Assembly overview

### `FScript`
Role:
- Command-line tool (`fscript`) to execute `.fss` files.

Responsibilities:
- Parse CLI arguments.
- Resolve script path and sandbox root.
- Build runtime context and extern registry.
- Execute parse -> type inference -> evaluation pipeline.
- Print final result and report parse/type/eval errors.

Use this when:
- You want to run scripts directly from terminal/CI.
- You want a ready-to-use executable without writing host integration code.

### `FScript.Language`
Role:
- Embeddable language engine.

Responsibilities:
- Core language model and AST.
- Lexer and parser.
- Type system and Hindley-Milner inference.
- Evaluator and value model.
- Pretty-printing of evaluation results.

Use this when:
- You want to embed the language in another .NET application.
- You need language processing independently from default runtime extern catalog.

NuGet:
- `MagnusOpera.FScript.Language`

### `FScript.Runtime`
Role:
- Runtime integration layer and extern catalog.

Responsibilities:
- Host context model (`RootDirectory` and related host constraints).
- Built-in external function modules (`List.*`, `Map.*`, `Option.*`, filesystem, JSON/XML, regex, hash, GUID, print).
- Extern registry composition.
- Host-oriented decoding helpers and sandbox-aware path checks.

Use this when:
- You want the built-in extern ecosystem ready to use.
- You want a baseline runtime integration layer to extend with custom externs.

NuGet:
- `MagnusOpera.FScript.Runtime`

## Typical composition

### CLI execution path
1. `FScript` reads CLI args and script text.
2. `FScript` asks `FScript.Runtime` for externs (`Registry.all`).
3. `FScript` invokes `FScript.Language` parse/infer/eval pipeline.
4. `FScript` prints result.

### Embedded host path
1. Host app references `FScript.Language` and (optionally) `FScript.Runtime`.
2. Host builds `HostContext` and extern list.
3. Host runs parse/infer/eval programmatically.
4. Host decides output, logging, and error handling behavior.

## Dependency direction
- `FScript.Language` has no dependency on `FScript.Runtime`.
- `FScript.Runtime` depends on `FScript.Language` types.
- `FScript` depends on both `FScript.Language` and `FScript.Runtime`.

This keeps the language engine reusable while runtime capabilities remain host-configurable.
