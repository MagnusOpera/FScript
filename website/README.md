# FScript Website

This folder contains the Docusaurus-powered FScript documentation website.

Keep all `@docusaurus/*` packages on the same version, including `@docusaurus/faster` for the enabled build optimizations. TypeScript stays on 5.9 because Docusaurus 3.10.2 uses the `baseUrl` compiler option, which TypeScript 7 removes.

## Commands

```bash
npm ci
npm run start
npm run build
npm run serve
npm run typecheck
```

Update the docs snapshot for the latest patch in a release line:

```bash
npm run version-docs -- X.Y.Z
```

## Publishing

- The **Publish Website** GitHub Actions workflow deploys the site from `main`.
- `make release-prepare version=X.Y.Z` snapshots the release's docs under `X.Y`. A patch replaces its release line's snapshot. A new release line retires the oldest snapshot once there are five.
- The latest stable manual is the default at `/manual`. The version selector lists `Next` first, followed by the five latest stable lines. Unreleased docs live at `/manual/next`.
- Each stable manual's **What's New** page shows the patch notes for its release line. The root changelog preserves the full release history.
