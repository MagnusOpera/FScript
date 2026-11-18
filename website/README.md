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

`FSCRIPT_WEBSITE_RELEASES` must contain the GitHub release objects, including `tag_name`, `published_at`, `draft`, and `prerelease`. Assembly selects the greatest published `X.Y.Z` stable version, excluding drafts and prereleases. An explicitly supplied inventory with no stable release fails rather than publishing a preview. The tag must exist locally. Production sets `FSCRIPT_WEBSITE_NEXT_REF=refs/remotes/origin/main` after checking out the invoking ref; local builds can omit it to include working-tree edits.

## Publishing

The **Publish Website** workflow runs on pushes to `main`, manual dispatch, and explicit calls from release distribution. It checks out the invoking ref for website build code, fetches full Git history and tags, and reads the published GitHub releases through the API. A tag release therefore uses the updated website code from that tag even if `main` still has an older website implementation.

- `/manual` is Stable. Its pages and sidebar come from `website/docs` and `website/sidebars.ts` at the latest published stable tag.
- `/manual/next` is Next. Its pages and sidebar come from the selected `main` commit.
- Stable's **What's New** page contains the complete released history from the tagged `CHANGELOG.md`, excluding Unreleased. Next's page contains only `main`'s Unreleased section.
- The selector contains only Next and Stable. Historical manuals are no longer published.
- The homepage and site design come from the invoking ref. The playground interpreter is compiled with main's .NET SDK from the same pinned `main` commit as the Next manual and is explicitly labelled FScript Next.
- `/website-sources.json` records the source commit for Next and the tag and commit for Stable.

Assembly creates `.generated/`, `versioned_docs/`, `versioned_sidebars/`, and `versions.json`. These are build inputs, not maintained source files. The deployment job checks that the build leaves tracked and untracked source files unchanged before deploying.

Documentation changes on `main` immediately update Next. Publishing a release promotes the documentation captured by its tag to Stable. Stable changes only when the selected published tag changes; website rebuilds never copy current Next content into Stable.

## Releases

Prepare a new release line locally from a clean `main` checkout. Commit the intended changes and Unreleased notes first:

```bash
git switch main
make release-prepare version=X.Y.Z dryrun=true
make release-prepare version=X.Y.Z
git branch release/vX.Y
make release-check version=X.Y.Z
git push origin main release/vX.Y --follow-tags
```

For a maintenance release, switch to the existing `release/vX.Y`, commit its fixes and Unreleased notes, then run the same preparation and validation commands. Push that release branch and tag together.

Preparation moves Unreleased notes into `### [X.Y.Z]`, adds the compare link, commits only `CHANGELOG.md`, and creates an annotated tag. The dry run prints the proposed changelog without modifying files, commits, or tags. No npm installation or website build is needed. Versions remain unprefixed `X.Y.Z` for now.

The tag workflow validates branch ancestry and tagged notes, runs CI, builds artifacts, and signs/notarizes macOS before automatically publishing the GitHub Release. It then explicitly calls package, editor-extension, tap, and website publication. There is no manual draft-publish step and no dependency on a second GitHub release event. The website promotes only published stable releases. Existing releases created before the release-branch policy remain eligible as documentation sources.

`release-check` and website generation remain read-only with respect to repository sources. `website-version` has been removed; website snapshots remain generated build inputs.

## Recovering an existing release

For a failed distribution, run **Publish Release Artifacts** from an updated workflow ref and supply the existing published release tag. Registry publication skips already published versions; the tap updates only when its formula changes. Website deployment runs after package and tap publication succeed.

To repair only an outdated website, manually dispatch **Publish Website** from the branch containing the current website build code. It still takes Stable content from the published tag and Next content/interpreter from `main`. This does not alter an existing release or republish packages.
