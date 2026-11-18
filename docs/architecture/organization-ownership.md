# FScriptLang organization

FScript repositories are hosted by FScriptLang under the Magnus Opera umbrella:

| Repository | Ownership |
| --- | --- |
| [FScript](https://github.com/FScriptLang/FScript) | Language, runtime, CLI, server, debugger, and language changelog |
| [FScript.Extensibility](https://github.com/FScriptLang/FScript.Extensibility) | Independent extensibility SDK and NuGet releases |
| [FScriptLang.github.io](https://github.com/FScriptLang/FScriptLang.github.io) | Website, Stable/Next documentation, and Pages deployment |
| [vscode-extension](https://github.com/FScriptLang/vscode-extension) | VS Code client, grammar, VSIX, and marketplace releases |

Repository transfer does not rename registry packages or change publisher
identity. NuGet IDs remain `MagnusOpera.FScript.*`, npm remains
`@magnusopera/fscript`, and the editor extension remains `MagnusOpera.fscript`.
The Homebrew tap stays in `magnusopera/homebrew-tap`. Existing releases, tags,
historical comparison links, and documentation-copy provenance remain intact.

New package repository metadata and npm provenance point to the transferred
repository. New Homebrew formulas download from FScriptLang releases. Language
release preparation derives future comparison links from the checkout's origin;
the independent SDK's release helpers use its new repository URL.

CI toolchain images are published to `ghcr.io/fscriptlang/fscript/<project>`.
Their source label points to FScriptLang/FScript. The trusted build job retains
package write permission and test shards retain package read permission, using
their built-in GitHub token to authenticate. Old MagnusOpera images are not
migrated or deleted; new cache identities rebuild under the new namespace.
Terrabuild workspace identity and cache credentials stay unchanged.

The website reads the public `FScriptLang/FScript/main/CHANGELOG.md`. The editor
extension fetches its exact existing server commit from FScriptLang/FScript;
changing repository ownership does not advance that commit or its language
version. Its ignored source cache updates its origin only when tracked source
files are clean, preserving local edits.

NuGet Trusted Publishing must trust each new GitHub owner/repository pair with
the existing `on-release-published.yml` filename. The NuGet policy creator's
username and package owner remain unchanged. VS Code Marketplace's existing
Trusted Publishing policy already names FScriptLang/vscode-extension and
`release.yml`. All three publisher policies must also require the `release`
environment. Registry policy setup belongs to the owner.

## Credentials and pull requests

PR validation and feature-branch CI use `ci.yml`, read-only GitHub token
permissions, and checkouts with credential persistence disabled. They receive no
stored credentials and build with Terrabuild's local-only mode. Their test
results remain available as artifacts; only trusted CI publishes combined check
reports. No workflow inherits secrets.

Trusted reusable-workflow calls name only their required secrets in explicit
allowlists. [GitHub requires this declaration even for environment-only secrets](https://docs.github.com/en/actions/how-tos/reuse-automations/reuse-workflows#using-inputs-and-secrets-in-a-reusable-workflow);
the job's environment supplies the values. Trusted CI fails immediately if its
cache credentials are missing, instead of silently rebuilding without the cache.
Only the trusted build job enables toolchain image publication. Test runners
and PRs can build that same image locally without attempting a registry push;
the publication flag does not change its hash.

Main, release branches, and release tags use `ci-trusted.yml`. Its jobs obtain
the Terrabuild token and cache key from the `trusted-ci` environment. Apple
signing/notarization, npm, and tap credentials live in the separate `release`
environment. Each environment permits only `main`, `release/v*` branches, and
`v*` tags. A PR's `refs/pull/*/merge` ref cannot use either environment, even if
the PR modifies a workflow to name it. The environment rules are configured in
GitHub, not enforced by these YAML files alone.

The extension's `release` environment permits `main` and `v*` tags and holds
only the Open VSX token. The SDK's `release` environment permits its numeric
release tags and holds no stored credential. Both use environment-bound Trusted
Publishing for their respective registries. The website has no stored secrets;
its `github-pages` environment permits only `main`.

Repository secrets and organization-secret grants must not duplicate these
credentials. Otherwise a modified same-repository PR workflow could read those
copies without entering an environment. Keep organization originals inaccessible
to repositories, or delete them after verifying the environment copies. Do not
allow pull-request refs in the environment deployment rules. Changes merged or
pushed to an allowed branch are trusted code and can use its credentials.

## Maintenance branch

`release/v0.94` uses the same environment-aware CI and publishing workflows as
main. Future maintenance releases require canonical `v0.94.Z` tags. Historical
Git tags, GitHub releases, and changelog entries remain unchanged. Legacy website
and editor source directories are retained on this branch as historical source,
but their Terrabuild project files and publishing workflows are removed. Website
and editor releases remain owned by their standalone repositories. Language and
runtime source, including its existing sample set, are not advanced by this
workflow backport.
