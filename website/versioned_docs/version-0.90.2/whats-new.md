---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## 0.90.2

- Install Terrabuild in the release workflow so tagged builds can produce release artifacts.
- Speed up `List.init` in compiled .NET programs and generated C#.
- Install Node.js before reading workspace tool versions in CI.
- Restore detailed cached build logs in CI with the Terrabuild 0.207.2 cache fix.
- Avoid cached-log rendering failures in CI while retaining build traces and test-report checks.
- Keep build-tooling and shard-configuration regressions on demand instead of delaying every CI build.
- Batch compatible .NET builds into dependency-connected solutions while keeping per-project test reports separate.
- Let .NET restore and build project references when compiling against partially cached outputs.
- Restore the CLI executable before running CLI tests from a cached test build.
- Publish build toolchain images under the `ghcr.io/magnusopera/fscript/` namespace.
- Run .NET tests from cached builds on fresh runners and fail CI when expected test reports are missing or empty.
- Restore cached CI test reports directly with Terrabuild and retain diagnostics for each shard selection.
- Document the Terrabuild and Docker requirements for building FScript from source.
- Keep VS Code builds cached when release notes change by adding the changelog only during extension packaging.
- Cancel superseded branch and pull-request CI runs for the same source branch while keeping test shards parallel.
- Label GitHub test shards as test instead of ci in Insights.
- Keep GitHub workflow reruns in separate Insights build groups while correlating each attempt's build and test shards.
- Publish the CI toolchain as a single image manifest so test runners can pull it from GHCR.
- Use Terrabuild's native Docker publishing and hash-based toolchain tags.
- Reuse GHCR toolchain images across CI runners.
- Restore the CLI and website test dependencies when CI shards start with cached builds on fresh runners.
- Report the GitHub build and all test shards as one Insights build group per workflow attempt.
- Split GitHub CI into a cached build, parallel test shards, and combined test reports, including separate Native AOT variants.
- Allow the native teapot debugger test to finish mesh generation on busy CI runners and report clearer event timeouts.
- Define shared build-tool versions in WORKSPACE for container builds and GitHub workflows.
- Update the .NET build toolchain to SDK 10.0.403 so Native AOT publishing uses the patched compiler.
- Publish .NET, JavaScript, and debugger test summaries and downloadable Terrabuild logs in CI.
- Install the latest stable Terrabuild release in CI.
- Run Terrabuild CI on pushes to every branch.
- Add container-backed Terrabuild builds and cached test suites, with separate FFI provider, renderer, and test projects.
- Fix NuGet trusted publishing authentication and allow retrying existing release packages without rebuilding.

**Full Changelog**: https://github.com/MagnusOpera/FScript/compare/0.90.1...0.90.2
