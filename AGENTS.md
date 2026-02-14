# AGENTS

This file defines contributor expectations for building, testing, regression safety, and release-note hygiene.

## Build, Test, and Non-Regression

Use these commands before opening or updating a PR:

- Build: `make build`
- Full test suite: `make test`
- Non-regression smoke tests (all samples): `make smoke-tests`

Equivalent direct commands:

- `dotnet build FScript.sln -c Release`
- `dotnet test FScript.sln -c Release`

Targeted test suites (when working on specific areas):

- Language: `dotnet test tests/FScript.Language.Tests/FScript.Language.Tests.fsproj -c Release`
- Runtime: `dotnet test tests/FScript.Runtime.Tests/FScript.Runtime.Tests.fsproj -c Release`
- Language Server / VS Code features: `dotnet test tests/FScript.LanguageServer.Tests/FScript.LanguageServer.Tests.fsproj -c Release`

## Test Quality Policy

- Every new feature must include automated test coverage.
- Every bug fix must include a regression test reproducing the prior failure mode.
- Add tests in the suite matching the change surface:
  - Parser/type/eval semantics -> `tests/FScript.Language.Tests`
  - Runtime and host behavior -> `tests/FScript.Runtime.Tests`
  - LSP/editor behavior -> `tests/FScript.LanguageServer.Tests`
- If language behavior, includes, or sample contracts change, run `make smoke-tests`.

## Release Notes (Unreleased)

- `CHANGELOG.md` must keep a top `## [Unreleased]` section.
- Each new feature/fix entry must be a short, single-line bullet.
- Write entries in user-facing terms (what changed), not implementation detail.
- At release time, move unreleased entries to the versioned section and reset `Unreleased`.

## Specification Maintenance (Mandatory)

- Any behavioral change in language, runtime, hosting, sandbox, or LSP must update the corresponding spec in `docs/specs/`.
- Any architectural change must update `docs/architecture/`.
- If docs are reorganized or files are moved, all internal links must be updated in the same PR.
- Spec updates are required for feature PRs; they are not optional.

## PR Checklist

- Build passes.
- Relevant test suite(s) pass.
- New behavior is test covered.
- Regression risk is covered by tests (including smoke tests when relevant).
- `CHANGELOG.md` `## [Unreleased]` has concise one-line entries for the change.
- Relevant specification/architecture documentation has been updated.
