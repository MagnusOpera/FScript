# FScript Assemblies, Roles, and Usage

## Purpose
This document describes how FScript is split into assemblies and how they are used together in applications.

## Assembly overview

### `FScript`
Role:
- Command-line tool (`fscript`) to execute `.fss` files.

Responsibilities:
- Parse CLI arguments.
- Resolve execution mode (file, stdin, `version`, REPL), sandbox root, and script arguments after `--`.
- Build runtime context and extern registry.
- Inject CLI environment value (`let Env`) into script execution (with stdlib `Environment` type).
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
- Built-in external function modules (filesystem, JSON/XML, regex, hash, GUID).
- Extern registry composition.
- Host-oriented decoding helpers and sandbox-aware path checks.

Use this when:
- You want the built-in extern ecosystem ready to use.
- You want a baseline runtime integration layer to extend with custom externs.

NuGet:
- `MagnusOpera.FScript.Runtime`

### `FScript.CSharpInterop`
Role:
- C#-friendly integration facade over language + runtime services.

Responsibilities:
- Resolve runtime extern catalog from source path/root context.
- Parse with include/import expansion through a stable interop entry point.
- Run inference APIs through a single host-facing surface.
- Expose stdlib virtual source loading for editor integrations.

Use this when:
- You integrate FScript from C# and want to avoid direct F# compiler/runtime internals.
- You build tooling services (for example LSP hosts) with a stable boundary.

### `FScript.LanguageServer`
Role:
- C# host executable for the Language Server process.

Responsibilities:
- Provide the production C# process host for LSP startup/dispatch.
- Execute the full LSP method surface used by the VS Code extension.
- Keep protocol behavior aligned with existing language/runtime analysis services.

Use this when:
- You want C# ownership of the server host process while reusing existing language services.

## Typical composition

### CLI execution path
1. `FScript` reads CLI args, splits script arguments after `--`, and picks mode: file path, piped stdin, `version`, or interactive REPL.
2. For execution modes, `FScript` builds `HostContext` root and resolves externs:
   - default runtime externs from `Registry.all` (unless `--no-default-externs`)
   - optional user extern assemblies via `--extern-assembly`
   - duplicate extern names are rejected as fatal CLI errors
3. `FScript` injects `Environment`/`Env` metadata:
   - file mode: `ScriptName = Some <file name>`, `Arguments = [..]`
   - stdin mode: `ScriptName = None`, `Arguments = [..]`
   - REPL mode: `ScriptName = None`, `Arguments = []`
4. `FScript` invokes `FScript.Language` parse/infer/eval pipeline.
5. `FScript` prints result (or version in `version` mode).

### Embedded host path
1. Host app references `FScript.Language` and (optionally) `FScript.Runtime`.
2. Host builds `HostContext` and extern list.
3. Host runs parse/infer/eval programmatically.
4. Host decides output, logging, and error handling behavior.

## Dependency direction
- `FScript.Language` has no dependency on `FScript.Runtime`.
- `FScript.Runtime` depends on `FScript.Language` types.
- `FScript.CSharpInterop` depends on both `FScript.Language` and `FScript.Runtime`.
- `FScript.LanguageServer` depends on `FScript.CSharpInterop`.
- `FScript` depends on both `FScript.Language` and `FScript.Runtime`.

This keeps the language engine reusable while runtime capabilities remain host-configurable.
