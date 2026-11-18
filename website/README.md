# FScript website

The Docusaurus website publishes one Stable manual and one Next manual. Documentation sources stay beside the implementation in `website/docs`; builds assemble editions in ignored directories. Do not commit generated manuals.

Keep all `@docusaurus/*` packages on the same version, including `@docusaurus/faster`. TypeScript stays on 5.9 because Docusaurus 3.10.2 uses `baseUrl`, which TypeScript 7 removes.

## Local development

```bash
npm ci
npm start
npm run build
npm run typecheck
npm test
```

Without release inputs these commands preview the current working tree as Next at `/manual`. This works offline and includes uncommitted documentation changes. It also works in Terrabuild without Git metadata. `npm start` watches canonical pages, the sidebar, and the changelog and refreshes the generated Next edition as you edit. No preview is deployed by CI.

To build both editions locally, fetch the published release inventory and tags first:

```bash
git fetch origin --tags
mkdir -p ../.out
gh api --paginate --slurp repos/MagnusOpera/FScript/releases > ../.out/release-pages.json
node -e 'const fs = require("node:fs"); fs.writeFileSync("../.out/website-releases.json", JSON.stringify(JSON.parse(fs.readFileSync("../.out/release-pages.json")).flat()))'
FSCRIPT_WEBSITE_RELEASES="$PWD/../.out/website-releases.json" FSCRIPT_WEBSITE_NEXT_REF=main npm run build
```

`FSCRIPT_WEBSITE_RELEASES` must contain the GitHub release objects, including `tag_name`, `published_at`, `draft`, and `prerelease`. Assembly selects the greatest published `X.Y.Z` stable version, excluding drafts and prereleases. An explicitly supplied inventory with no stable release fails rather than publishing a preview. The tag must exist locally. Production sets `FSCRIPT_WEBSITE_NEXT_REF=HEAD` after checking out `main`; local builds can omit it to include working-tree edits.

## Publishing

The **Publish Website** workflow runs on pushes to `main`, release publication/removal, and manual dispatch. It always checks out `main`, fetches full Git history and tags, and reads the published GitHub releases through the API.

- `/manual` is Stable. Its pages and sidebar come from `website/docs` and `website/sidebars.ts` at the latest published stable tag.
- `/manual/next` is Next. Its pages and sidebar come from the selected `main` commit.
- Stable's **What's New** page contains the complete released history from the tagged `CHANGELOG.md`, excluding Unreleased. Next's page contains only `main`'s Unreleased section.
- The selector contains only Next and Stable. Historical manuals are no longer published.
- The homepage, site design, and playground come from `main`. The playground is explicitly labelled FScript Next.
- `/website-sources.json` records the source commit for Next and the tag and commit for Stable.

Assembly creates `.generated/`, `versioned_docs/`, `versioned_sidebars/`, and `versions.json`. These are build inputs, not maintained source files. The deployment job checks that the build leaves tracked and untracked source files unchanged before deploying.

Documentation changes on `main` immediately update Next. Publishing a release promotes the documentation captured by its tag to Stable. Stable changes only when the selected published tag changes; website rebuilds never copy current Next content into Stable.

## Releases

Prepare the desired source and changelog on `release/vX.Y` through normal commits. The changelog must contain a nonempty `### [X.Y.Z]` section with bullets and its compare link. Create and push an annotated tag from that branch:

```bash
git switch release/vX.Y
git tag -a X.Y.Z -m "Release X.Y.Z"
make release-check version=X.Y.Z
git push origin release/vX.Y --follow-tags
```

The tag workflow validates branch ancestry and tagged notes, builds artifacts, and creates a draft. Verify and publish that draft. The website promotes only published stable releases. Existing releases created before the release-branch policy remain eligible as documentation sources.

Release checking and website generation make no commits or tags and never rewrite `CHANGELOG.md`. `release-prepare` and `website-version` have been removed. Changelog sections remain author-maintained source content.
