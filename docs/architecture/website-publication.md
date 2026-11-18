# Website publication

The website uses Docusaurus versioning to expose exactly two editions. Canonical Next documentation remains in `website/docs` beside source code. Stable documentation comes from that same path at a published release tag. Historical manual snapshots are not stored or published.

## Source selection

Production checks out `main` with full history and tags. The GitHub release API supplies the inventory of published releases. `website/scripts/assemble-docs.mjs` excludes drafts and prereleases, then selects the highest numeric `X.Y.Z` version. Publication date does not determine version precedence, so publishing a patch for an older line does not roll Stable back.

Assembly resolves the selected tag and the checked-out `main` revision to immutable commit IDs before exporting content. Each edition uses its own documentation and sidebar. Stable's complete released changelog comes from the tagged source; Next renders only the Unreleased section of its own source. Both old top-level patch headings and grouped release-line headings are supported.

Generated Docusaurus inputs live in ignored `website/.generated`, `website/versioned_docs`, `website/versioned_sidebars`, and `website/versions.json`. Canonical pages, changelogs, branches, tags, and commits remain unchanged. All source inputs are validated before generated directories are replaced. `/website-sources.json` publishes the selected source identities.

## Build modes

A deployment always supplies the published release inventory and an explicit Next revision. A missing tag or an inventory without a stable release fails the build. A development or Terrabuild preview without an inventory renders only the working-tree Next edition at `/manual`, without Git or network access. This mode preserves live editing and makes normal repository builds independent of GitHub availability.

The website shell and JavaScript playground use the deployment checkout. Production therefore runs the Next interpreter and labels it accordingly; it does not claim to run the stable interpreter.

## Release boundary

Future tags must be reachable from `release/vX.Y`, matching the tag's major and minor version. The tag workflow validates ancestry and the tagged changelog before building artifacts. The validator is read-only. Maintainers prepare release sections through normal source commits, create tags themselves, and publish the CI-created draft.

Website deployment runs on main pushes, published or removed releases, and manual dispatch. It resolves sources when the serialized deployment job runs, builds both editions, verifies their rendered changelog headings and provenance, and checks repository cleanliness. It never promotes an unpublished tag. Legacy published tags do not require retroactive release branches.

See [website commands and release procedure](../../website/README.md).
