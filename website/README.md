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

- GitHub Actions deploys this site to GitHub Pages on release tag pushes.
- Docs versions are created during `make release-prepare version=X.Y.Z`.
