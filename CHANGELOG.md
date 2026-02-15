# Changelog

All notable changes to FScript are documented in this file.

## [Unreleased]

- Restricted `make release-prepare` to stable versions only (`major.minor.build`) and removed `-next` support from release docs and validation.
- Added REPL documentation under guides and architecture, and updated the getting-started tutorial to include a REPL check right after install.
- Added `make release-prepare` to automate changelog versioning, compare link generation, release commit, and local tag creation.
- Release tag workflow now populates draft release notes from the matching `CHANGELOG.md` version section and fails fast when it is missing or invalid.
- Initialize post-0.37.0 unreleased section.
- Added CLI stdin execution support so scripts can be piped to `fscript` (including `-r/--root` overrides).
- Added `fscript version` command to print the current CLI version.
- Added interactive CLI REPL mode when running `fscript` without arguments.
- Updated REPL multiline submission to require double-Enter for pending blocks and improved function display with typed signatures.
- Fixed block semantics to reject trailing `let`-only blocks without a final expression (for example `let a = let f x = ...`) and require an explicit return expression.

## [0.37.0]

- Added resolver-backed import loading APIs (`parseSourceWithIncludesResolver` and `ScriptHost.loadSourceWithIncludes`) for hosts that load scripts from non-file sources.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.36.0...0.37.0

## [0.36.0]

- Fixed LSP go-to-definition for alias-qualified function calls (for example `Helpers.append_part`).

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.35.0...0.36.0

## [0.35.0]

- Fixed LSP completion insertion for dotted prefixes so selecting `Option.map` after `Option.` no longer duplicates the qualifier.
- Switched import syntax to `import "path.fss" as Alias` and removed `from` import grammar.
- Updated LSP type display/navigation to hide internal import prefixes and use source aliases (for example `Common.ProjectInfo`).
- Renamed sample `includes-and-exports.fss` to `imports-and-exports.fss` and updated docs links.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.34.0...0.35.0

## [0.34.0]

- Removed F# sources from `src/FScript.LanguageServer*` by moving LSP semantic modules into `FScript.CSharpInterop` and keeping `FScript.LanguageServer` as C# host.
- Replaced `FScript.LanguageServer.Tests` project with a C# test project and C# LSP test harness to remove F# compile cost from LanguageServer test builds.
- Deleted obsolete F# LanguageServer test sources after C# test project migration.
- Renamed `FScript.CSharpInterop/LanguageServerLegacy` to `FScript.CSharpInterop/LanguageServer` to reflect the new primary architecture.
- CI now runs branch update builds on PR `synchronize` events while keeping `ci-main` scoped to `main` pushes to avoid duplicate runs.
- Enabled F# preview parallel compilation globally, disabled deterministic builds, and removed global RuntimeIdentifiers to reduce CI build latency.
- Added `FScript.CSharpInterop` as a stable bridge for parse/infer/runtime-extern/stdlib-source services and wired LanguageServer through it.
- Added `FScript.LanguageServer` host executable as the migration entrypoint for C#-owned LSP startup.
- Added a first native C# LSP server core (JSON-RPC transport, initialize/shutdown, text sync, and stdlib-source request) with dedicated integration tests.
- Extended the native C# LSP core with diagnostics publishing and `viewAst`/`viewInferredAst` command handling.
- Switched C# LSP host to full-method dispatch parity via shared handlers, made it the default test target, and updated extension/tag packaging to use `FScript.LanguageServer.dll`.
- Replaced the F# LSP server executable with `FScript.LanguageServer` (C#) and moved F# LSP logic into `FScript.LanguageServer.Core`.
- Fixed imported qualified type annotations (for example `common.ProjectInfo`) in parser/type inference to prevent false type mismatches.
- Fixed LSP inlay hints to ignore spans from other files so included symbols no longer leak labels into unrelated declarations.
- Enforced string-only map keys across type inference, evaluation, samples, and LSP type rendering (removed `int|string` key-domain displays).
- Fixed map-pattern inlay hints to infer key/value/tail bindings (`string`, `int`, `int map`) instead of `unknown`.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.33.0...0.34.0

## [0.33.0]

- Updated runtime/test project package versions after NuGet publish verification.
- Enforced strict changelog gates in CI (PR + main) and added `make verify-changelog` local preflight.
- Optimized `FScript.LanguageServer.Tests` startup by building the language server once per run instead of once per test.
- Reused a single LSP server process across `FScript.LanguageServer.Tests` and added per-test document cleanup for isolation, reducing suite runtime substantially.
- Removed `clean` from `publish-all` to avoid redundant CI cleanup, restored one LSP process per test for stronger isolation (while keeping build-once), and explicitly disabled AOT for LanguageServer publish.
- Split `FScript.LanguageServer.Tests` into feature-oriented files with shared wire/client/fixture helpers to reduce CI compile bottlenecks from the previous monolithic test file.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.32.0...0.33.0

## [0.32.0]

- Replaced `#include` with `import` and removed script-level `module` declarations.
- Imported files are now exposed through filename-derived modules (for example `shared.fss` -> `shared.*`).
- Updated parser/runtime/LSP/docs/tests for the new import/module semantics.
- Removed `unused` top-level binding diagnostics from LSP.
- Clarified `AGENTS.md` so every direct commit to `main` must include an `Unreleased` changelog entry, including docs/process/policy updates.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.31.0...0.32.0

## [0.31.0]

- Enforced warning-as-error globally via `Directory.Build.props` (`TreatWarningsAsErrors=true`).
- Fixed LSP compiler warnings (exhaustive `SInclude` matches and null-safe stdlib resource stream handling).

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.30.0...0.31.0

## [0.30.0]

- Added contributor policy in `AGENTS.md` for build/test/non-regression workflow.
- Added CI changelog gate requiring one-line `## [Unreleased]` entries for functional PR changes.
- Reorganized documentation into `docs/specs`, `docs/architecture`, and curated root `docs/` guides.
- Added docs indexes (`docs/README.md`, `docs/specs/README.md`, `docs/architecture/README.md`) and linked README/tutorial to the new structure.
- Added `docs/specs/lsp-inlay-hints.md` and mandated specification maintenance in `AGENTS.md`.
- LSP now injects runtime extern schemes for document typing, enabling host functions (`Fs.*`, `Regex.*`, etc.) and showing injected signatures in hover/completion/signature help.
- LSP definition/type-definition navigation now targets included declaration files (URI derived from symbol source span) instead of always the current file.
- LSP now resolves record field labels inside function return record literals to the function’s declared/inferred return type (including include-provided type definitions).
- Suppressed misleading top-level `unused` diagnostics for underscore helper/include files (for example `_helpers.fss`).
- LSP hover/signature for injected stdlib functions now shows named arguments (for example `Option.map` mapper/value labels).
- LSP definition on injected stdlib functions now opens readonly virtual stdlib sources (`fscript-stdlib:///Option.fss`, `List.fss`, `Map.fss`).

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.29.0...0.30.0

## [0.29.0]

- Improved LSP inlay/type rendering for map-related inference (`int|string` map-key domain and `unknown map` display).
- Added LSP inlay type hints for pattern-bound variables (for example `| Some x ->` now shows `x: int`).
- Added regression coverage for map-key/type inlay rendering and option-pattern variable inlay hints.

## [0.28.0]

- Added local variable type hover in LSP.
- Added local variable type capture in type inference.
- Added VS Code extension auto-compile for development startup.

## [0.27.0]

- Added structural update syntax support with `with` inside structural literals: `{| base with Field = value |}`.
- Structural updates can add new fields and work with multiline layout.
- Added parser/type/eval regression coverage for structural record update semantics.

## [0.26.0]

- Split record literals: `{ ... }` is nominal (must match one declared record type), while `{| ... |}` is structural.
- Improved type identity and LSP navigation for records (definition/type-definition from annotations, record literal fields, and parameter usages).
- Added sample and regression coverage for annotation/literal semantics and record navigation (`samples/annotations-and-matching.fss`, language + LSP tests).

## [0.25.0]

### Extension
- Fix nullness warnings.
- Implemented analysis state.

## [0.24.0]

### Tooling
- Added first-party VS Code extension (`vscode-fscript`) with:
  - FScript syntax highlighting.
  - Language Server features: diagnostics, completion, hover, symbols, go-to-definition, type-definition, references, rename, inlay hints, semantic tokens, quick fixes, and include-path navigation.
  - Extension status bar + output channel integration.
- Added automatic .NET runtime acquisition support for the extension via `.NET Install Tool` integration (with PATH fallback).
- Added tag-only publish workflow to distribute the extension to VS Code Marketplace and Open VSX.

### Documentation
- Added dedicated map matching reference: `docs/map-matching-reference.md`.
- Added include-resolution details (path normalization, root confinement, include deduplication, file-aware errors) in `docs/syntax-and-indentation.md`.
- Added embedding cookbook section to `docs/embedding-fscript-language.md`.
- Added and expanded onboarding tutorial and README navigation polish.

## [0.23.1]

_Published: 2026-02-12_

  - Added comprehensive Getting Started tutorial (docs/getting-started-tutorial.md) with progressive onboarding: install, basic/flow types, pattern matching, partial matching,
    recursion, stdlib, includes/modules, exports, and hosting/security.
  - Reworked README to prioritize onboarding:
      - dedicated Getting Started Tutorial section
      - repository development instructions moved after install/tutorial content
      - clarified stdlib vs host externs (List.*, Map.*, Option.* are prelude stdlib).
  - Minor docs polish and navigation improvements:
      - better cross-links between tutorial, samples, and reference docs.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.23.0...0.23.1

## [0.23.0]

_Draft_

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.22.0...0.23.0

## [0.22.0]

_Published: 2026-02-11_

  - Added generic map key support for string and int in FScript.
  - Kept {} polymorphic and inference-driven, so it resolves by usage.
  - Updated map indexer, map pattern matching, and map spread typing to work with typed keys.
  - Added validation errors for unsupported map key types.
  - Updated tests and samples behavior accordingly; full test suite and smoke tests pass.
  - Updated documentation.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.21.0...0.22.0

## [0.20.0]

_Published: 2026-02-11_

  - Improved map match semantics to support intuitive key-based lookup patterns.
  - Added support for multi-key map patterns, with optional ..tail.
  - Preserved dynamic extraction patterns ({ [k] = v; ..tail }).
  - Added map-pattern sample (samples/map-matching.fss) and updated syntax/tests accordingly.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.19.0...0.20.0

## [0.19.0]

_Published: 2026-02-11_

  - Added when guards to match cases (| pattern when condition -> expr).
  - Guards are type-checked as bool and evaluated after pattern binding.
  - Works uniformly across list, record, tuple, union, and map patterns.
  - Added tests and docs updates, plus a sample demonstrating guarded map matching.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.18.0...0.19.0

## [0.18.0]

_Published: 2026-02-11_

  - Added native map spread/update syntax: { [k] = v; ..tail } and { ..tail }.
  - Removed map support from @; append is now list-only.
  - Updated Map stdlib internals to use spread syntax.
  - Added parser/type/eval tests for map spread and map-append rejection.
  - Updated docs and types-showcase sample to reflect the new map update style.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.17.0...0.18.0

## [0.17.0]

_Published: 2026-02-11_

  - Added #include "file.fss" support with cycle detection, sandbox-aware path checks, and file-aware error reporting.
  - Added module support for included scripts (module Foo) with scoped symbol resolution.
  - Introduced a protected built-in stdlib prelude and migrated List/Option core functions from runtime externs to stdlib scripts.
  - Added map pattern matching ({} and { [k] = v; ..tail }) and migrated map helpers to stdlib, removing collection extern files.
  - Updated samples/docs to cover includes, modules, and map matching.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.16.0...0.17.0

## [0.16.0]

_Published: 2026-02-10_

  - Added attribute-based exports with [<export>] (case-sensitive), replacing export let.
  - Added qualified discriminated union support:
      - constructor usage with Type.Case
      - match patterns with Type.Case
  - Preserved backward compatibility for unqualified DU cases (Case still works).
  - Extended parser/type-inference/eval test coverage for export attributes and qualified DU behavior.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.15.2...0.16.0

## [0.15.2]

_Published: 2026-02-10_

- Update README.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.15.1...0.15.2

## [0.15.0]

_Published: 2026-02-10_

  - Added NuGet package icon metadata for MagnusOpera.FScript.Language and MagnusOpera.FScript.Runtime.
  - Updated README.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.14.0...0.15.0

## [0.14.0]

_Published: 2026-02-10_

  - Added support for multiline lambda bodies inside parenthesized call chains, including correct pipeline continuation after ).
  - Added support for aligned multiline let parameter declarations (strict column alignment).
  - Tightened layout rules for multiline literals and inline record type annotations.
  - Improved parse diagnostics for layout issues: misindented expressions now report explicit indentation errors instead of generic unexpected-token errors.
  - Updated syntax/indentation documentation and parser regression tests accordingly.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.13.0...0.14.0

## [0.13.0]

_Published: 2026-02-09_

  - Added support for multiline lambda bodies inside parenthesized call chains, including pipeline continuation after ).
  - Added parser tests for multiline lambda/pipeline behavior.
  - Added support for aligned multiline let parameter declarations with strict column alignment.
  - Tightened indentation/layout rules for multiline literals and inline record annotation shapes.
  - Updated syntax-and-indentation.md with the new accepted layouts and multiline lambda examples.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.12.0...0.13.0

## [0.12.0]

_Published: 2026-02-09_

  - Added newline-separated multiline layouts for map, record, list literals, record updates, and inline record type annotations.
  - Kept semicolon-separated syntax fully compatible.
  - Added parser tests and a new samples/layout-styles.fss.
  - Updated syntax docs and showcase examples.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.10.0...0.12.0

## [0.10.0]

_Published: 2026-02-08_

  - Added List.empty (arity-0 value, like Map.empty).
  - Added Map.map (maps values only) and Map.iter.
  - Registered new externs in runtime registry.
  - Added tests for new extern behavior and registry exposure.
  - Updated docs for extern catalog and map/list value semantics.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.9.0...0.10.0

## [0.11.0]

_Published: 2026-02-08_

- Fix fscript macOS binary identifier

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.9.0...0.11.0

## [0.9.0]

_Published: 2026-02-08_

- Unified map literals with brace syntax.
- Added key-expression support in map literals.
- Completed parser/tests/docs updates for the new map syntax.
 
**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.8.0...0.9.0

## [0.8.0]

_Published: 2026-02-08_

- Fixed inference for unannotated record field access in Option.map lambdas.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.7.0...0.8.0

## [0.7.0]

_Published: 2026-02-08_

- Targeted FScript.Language and FScript.Runtime to netstandard2.1.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.6.0...0.7.0

## [0.6.0]

_Draft_

- Added non-empty list pattern support.
- Improved multiline lambda application robustness.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.5.0...0.6.0

## [0.5.0]

_Published: 2026-02-08_

  - Enforced explicit tuple syntax in match cases.
  - Added multi-parameter lambda syntax support.
  - Improved host integration:
      - exposed exported function signatures,
      - accepted host record maps in map externs.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.4.0...0.5.0

## [0.4.0]

_Published: 2026-02-08_

 - Added native map literal syntax #{ ... }.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.3.0...0.4.0

## [0.3.0]

_Draft_

- Treated arity-0 externs and exports as values (not callable functions).

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.2.0...0.3.0

## [0.2.0]

_Published: 2026-02-08_

  - Added script host invocation API.
  - Added top-level export let metadata and restricted host visibility to exported bindings.
  - Refactored higher-order externs to callback-based runtime handlers.
  - Added nameof.
  - Added Map.ofList, multiline argument support, and extended map APIs (count/filter/fold/choose + try-get rename work).
  - Added root-confined filesystem write externs.
  - Added smoke-tests and updated export/host documentation.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.1.4...0.2.0

## [0.1.4]

_Published: 2026-02-07_

  - Updated entitlements.
  - Added embedding documentation.
  - Added distribution setup for NuGet and Homebrew.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.1.3...0.1.4

## [0.1.3]

_Published: 2026-02-07_

 - Fixed token issue affecting tap publishing.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.1.2-next...0.1.3

## [0.1.3-next]

_Pre-release Published: 2026-02-07_

- No code changes vs 0.1.3 (tag points to same commit).

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.1.2-next...0.1.3-next

## [0.1.2-next]

_Pre-release Published: 2026-02-07_

- Fixed Homebrew tap publishing.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.1.1-next...0.1.2-next

## [0.1.1-next]

_Pre-release Published: 2026-02-07_

  - Fixed missing entitlement in macOS signing/publish flow.
  - Ensured signed macOS artifacts can be produced correctly in CI.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.1.0-next...0.1.1-next

## [0.1.0-next]

_Pre-release Published: 2026-02-07_

  - Added publishing/distribution groundwork.
  - Introduced CLI parsing updates and root-directory behavior.
  - Renamed projects and improved docs.
  - Added macOS signing/versioning alignment and fixed build warnings.
  - Switched license to MIT.

**Full Changelog**: https://github.com/MagnusOpera/FScript/commits/0.1.0-next
