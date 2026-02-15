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
- Build must be warning-free; warnings are treated as errors (`TreatWarningsAsErrors=true`).
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
- Each released version section should end with a compare link:
  - `**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/<previous-tag>...<new-tag>`
- When publishing the GitHub release, include that same compare link in the release notes body.

## Commit Gate (Hard Requirement)

- Every commit that targets `main` must update `CHANGELOG.md`.
- Required format for regular commits:
  - add at least one short, single-line bullet under `## [Unreleased]`.
- Scope is strict (no exceptions for docs/process/policy/chore/dependency-only commits).
- Local preflight command:
  - `make verify-changelog`
- CI enforces this on both PRs and direct pushes to `main`.

## Release Process (Tags and GitHub Draft)

Follow this exact sequence for every release:

1. Run `make release-prepare version=X.Y.Z`.
   - Optional preview mode: `make release-prepare version=X.Y.Z dryrun=true`
2. Push commit and tag together: `git push origin main --follow-tags`.
3. Wait for CI to create the GitHub Release **as draft** from the tag workflow.
4. Confirm the draft notes are sourced from `CHANGELOG.md` `## [X.Y.Z]` (including compare link).
5. Publish that existing draft release (do not create/publish a release manually before CI draft creation).

Rules:
- Tag-triggered CI is the source of truth for release artifacts and draft release creation.
- Do not bypass the draft step.
- Tag workflow must fail if `CHANGELOG.md` has no non-empty `## [X.Y.Z]` section with bullets and compare link.
- Release notes must match the `CHANGELOG.md` version section and keep the compare link.
- `make release-prepare` supports `X.Y.Z` only.

## Specification Maintenance (Mandatory)

- Any behavioral change in language, runtime, hosting, sandbox, or LSP must update the corresponding spec in `docs/specs/`.
- Any architectural change must update `docs/architecture/`.
- If docs are reorganized or files are moved, all internal links must be updated in the same PR.
- Spec updates are required for feature PRs; they are not optional.

## PR Checklist

- Build passes.
- Build passes with zero warnings.
- Relevant test suite(s) pass.
- New behavior is test covered.
- Regression risk is covered by tests (including smoke tests when relevant).
- `CHANGELOG.md` `## [Unreleased]` has concise one-line entries for the change.
- Relevant specification/architecture documentation has been updated.

## Direct To Main Policy

- Committing directly to `main` follows the same quality bar as a PR.
- All checklist items above still apply (build/test/spec/docs/changelog).
- Documentation and release notes must be updated in the same change set.
- Direct-to-main commits are blocked by the changelog gate if `CHANGELOG.md` is not updated.
