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
- Keep one-line bullet entries adjacent with no blank lines between them; use exactly one blank line around headings and compare links.
- After editing `CHANGELOG.md`, run `make format-changelog`; `make check-changelog-format` checks spacing without changing files.
- At release time, move unreleased entries to a `### [vX.Y.Z]` section under `## [X.Y]` and reset `Unreleased`.
- Each released version section should end with a compare link:
  - `**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/<previous-tag>...<new-tag>`
- When publishing the GitHub release, include that same compare link in the release notes body.

## Commit Gate (Hard Requirement)

- Every commit that targets `main` must update `CHANGELOG.md`.
- Required format for regular commits:
  - add at least one short, single-line bullet under `## [Unreleased]`.
- Scope is strict (no exceptions for docs/process/policy/chore/dependency-only commits).
- Exception: release commits (`chore(release): vX.Y.Z`) may leave `## [Unreleased]` empty.
- Local preflight command:
  - `make verify-changelog`
- CI enforces this on both PRs and direct pushes to `main`.

## Release Process (Automatic Tag Publication)

Follow this exact sequence for every release:

1. Commit the intended changes and Unreleased notes on `main` for a new release line, or on `release/vX.Y` for a maintenance release.
2. Run `make release-prepare version=vX.Y.Z`. Use `dryrun=true` to preview without changing files or Git state.
3. For a new release line prepared on `main`, create `release/vX.Y` at the prepared commit. For maintenance releases, remain on that release branch.
4. Run `make release-check version=vX.Y.Z`, then push the branch and tag together: `git push origin release/vX.Y --follow-tags`. Push the preparation commit on `main` too when it originated there.
5. The tag workflow validates and tests the source, packages binaries, signs and notarizes macOS, and publishes the GitHub Release automatically.
6. The same run explicitly publishes NuGet, npm, editor extensions, and the Homebrew tap. Confirm all publication jobs pass; no manual draft publication is required.

Rules:
- Tag-triggered CI is the source of truth for release artifacts and publication.
- Publish only after CI and macOS verification/notarization pass. Unsigned staging artifacts must not be included in the public release.
- Only canonical `vX.Y.Z` release tags are accepted, with no leading zeroes, suffixes, or unprefixed compatibility. Package versions and archive filenames use numeric `X.Y.Z`. Existing Git tags, GitHub releases, and published tap formulas are not migrated.
- Future release tags must be reachable from `release/vX.Y` matching their major/minor version.
- Tag workflow must fail if the tagged `CHANGELOG.md` has no non-empty `### [vX.Y.Z]` section with bullets and compare link.
- Release notes must match the `CHANGELOG.md` version section and keep the compare link.
- `make release-prepare` requires a clean checkout, moves Unreleased into the release section, adds its compare link, commits only `CHANGELOG.md`, and creates an annotated prefixed tag. New changelog patch headings use `[vX.Y.Z]`; archived historical entries are left intact. If no earlier prefixed tag exists, the first compare link uses the initial repository commit. It never builds or commits website snapshots.
- `make release-check` validates an existing `vX.Y.Z` tag without modifying files, making commits, or creating tags.
- Failed distribution can be retried with **Publish Release Artifacts**, supplying the existing published release tag. Do not move published tags or change package versions to retry distribution.
- Keep `on-release-published.yml` as the NuGet publisher workflow identity used by the trusted publishing policy, even though it is now called directly by the tag workflow.
- Website content and deployment belong to [FScriptLang/FScriptLang.github.io](https://github.com/FScriptLang/FScriptLang.github.io), independently of language releases. Do not reintroduce website sources or deployment into this repository.
- Keep language release notes in `CHANGELOG.md`; the website fetches them from main for Stable history and Next Unreleased. See [website publication](docs/architecture/website-publication.md).

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
