# Website publication

The documentation website is maintained and deployed independently in
[FScriptLang/FScriptLang.github.io](https://github.com/FScriptLang/FScriptLang.github.io),
under the Magnus Opera umbrella. Its production URL remains
[fscript.magnusopera.io](https://fscript.magnusopera.io/).

## Ownership

The website repository owns the landing page, tutorials, Stable and Next
documentation, browser playground, and GitHub Pages deployment. Its initial
copies preserved the shipped 0.94.3 manual and the unreleased documentation
from main. Further edits and promotions happen in that repository without
requiring a language release. FScript no longer contains website sources,
build targets, CI selections, or Pages publication permissions.

The playground consumes the latest public `@magnusopera/fscript` npm package.
Both documentation editions use that published interpreter. The website does
not compile an interpreter from FScript main or require this repository's
.NET toolchain.

## Changelog and release boundary

Language release notes remain in this repository's `CHANGELOG.md`. Keep the
top Unreleased section, version sections, and their original comparison links.
The website fetches this public file from main: Stable displays history through
its configured documentation release, and Next displays only Unreleased.
Archived notes are not moved or rewritten as part of the website migration.

`make release-prepare version=vX.Y.Z` continues to move Unreleased notes into
a version section, commit only the changelog, and create the annotated tag.
Release validation and artifact publication are unchanged. Only canonical
`vX.Y.Z` tags are accepted; package versions and archive names remain numeric.
No existing Git tags, GitHub releases, or Homebrew formulas are migrated.

The tag workflow still waits for CI, macOS verification, signing, and
notarization before publishing the release. It then distributes NuGet, npm,
and the Homebrew tap. The VS Code extension is packaged and published separately in [FScriptLang/vscode-extension](https://github.com/FScriptLang/vscode-extension). It does not deploy or dispatch the
website. Website pushes, manual runs, and scheduled builds refresh the site
independently, including new npm packages and upstream changelog changes.

See the [website development and publication instructions](https://github.com/FScriptLang/FScriptLang.github.io#readme)
for site maintenance. GitHub Pages and Cloudflare configuration belong to that
site's maintainers.
