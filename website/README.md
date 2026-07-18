# FScript Website

This folder contains the Docusaurus-powered FScript documentation website.

## Commands

```bash
npm ci
npm run start
npm run build
npm run serve
npm run typecheck
```

Create a new versioned docs snapshot:

```bash
npm run version-docs -- X.Y.Z
```

## Publishing

- GitHub Actions deploys this site from the primary release workflow when a GitHub release is published.
- Docs versions are created during `make release-prepare version=X.Y.Z`.
- Regular website builds keep `Next` as the default and treat the first entry in `versions.json` as the maintained release; release preparation can make that release the default with `FSCRIPT_DOCS_LAST_VERSION`.
