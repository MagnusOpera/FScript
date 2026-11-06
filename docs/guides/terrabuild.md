# Container-backed builds with Terrabuild

Install Terrabuild 0.205.1 and a working Docker engine. The graph builds its toolchain image automatically. .NET, Node, and Clang are supplied by that image; they need not be installed on the host for these targets.

```sh
# Build every .NET project, the JavaScript package, extension bundle, and website.
make build TB_FLAGS=--local-only

# Run all .NET, debugger-adapter, JavaScript, extension, and website type checks.
make tb-test TB_FLAGS=--local-only

# Run everything, including sample smoke tests and the independent SDK check.
make tb-check TB_FLAGS=--local-only
```

`make build` always uses `terrabuild` from `PATH` through the same wrapper as `tb-build`; it fails if Terrabuild is unavailable. There is no host `dotnet build` fallback. For an explicit host build, run `dotnet build FScript.sln -c Release`.

`tb-test` builds its prerequisites automatically. `tb-smoke` and `tb-sdk` select just those checks and their prerequisites. `config` defaults to Release; Terrabuild defaults to one concurrent task per processor; set `TB_PARALLEL` to override that limit. Existing `test` and release recipes remain available, and targets that depend on `build` now require Terrabuild and Docker. The full test graph currently expects Release because existing CLI integration tests launch Release binaries.

The underlying command is:

```sh
terrabuild run build test smoke sdk --local-only
```

The Make targets check Terrabuild's JSON completion status as well as its process exit code. Use them in automation with Terrabuild 0.200.1, which can return zero after reporting failed tasks. A missing or unsuccessful completion report fails the Make target.

## Containers and project layout

The toolchain phase builds `.build/toolchain/Dockerfile` with .NET SDK 10.0.401, Node 22, Clang, and zlib headers. Only this Docker image bootstrap runs on the host. `@dotnet`, `@npm`, and the custom `workflow` extension all specify the toolchain image and an explicit Linux architecture. No registry push or deployment occurs. Persistent MSBuild nodes and shared compiler servers are disabled in the image because CLI tests capture subprocess output; retained server processes can keep those pipes open after a command exits.

Terrabuild stops discovery at a `PROJECT` file. The FFI sample therefore keeps scripts and `fscript.toml` at `samples/ffi`, with sibling `Provider`, `Renderer`, and `Tests` projects below it. Build the provider directly with `dotnet build samples/ffi/Provider/FfiSample.fsproj -c Release` when using the existing host workflow.

Project-reference dependencies build before their consumers. Tests run already-built binaries with `--no-build --no-restore`. Conditional sibling-SDK project references are declared explicitly in the graph; ordinary builds use the published SDK package. Non-.NET checks declare their inputs and upstream builds too. The SDK compatibility check runs in a temporary source copy because it deliberately publishes different assembly versions.

Builds use the normal `bin`, `obj`, `node_modules`, and generated website directories on the mounted checkout. They contain Linux outputs after a container run. Rebuild through the host toolchain before relying on native host apphosts or platform-specific Node dependencies. Avoid running host and container builds concurrently in the same checkout.

## Local and shared caching

The workspace ID is configured in `WORKSPACE`. Local caching works without an Insights account; `--local-only` disables shared caching. Omit `TB_FLAGS=--local-only` after connecting the workspace to allow shared reuse. Do not put tokens or master keys in repository files.

The PR and branch-push GitHub workflows share `.github/actions/setup-tools` to install Terrabuild 0.205.1 and configure authentication. They run `make tb-check`, covering builds, all test targets, sample smoke tests, and independent SDK compatibility. The graph supplies .NET, Node, and the Native AOT toolchain through Docker. Configure repository secrets `TERRABUILD_TOKEN` and `TERRABUILD_MASTER_KEY` to enable shared caching. Without both secrets, including on fork PRs, CI uses local caching. Test reports are collected from each project's `TestResults` directory.

Managed build outputs and successful test results are cacheable separately. npm dependencies are kept in the local workspace cache. When application work must execute, the toolchain bootstrap checks the image using Docker layer caching. Fully cached requests skip execution; the phase barrier does not force application cache hits to rebuild. The configured Linux architecture separates ARM64 and AMD64 artifacts.

Use `terrabuild explain build test --local-only` to inspect cache decisions. Repeating `make tb-check TB_FLAGS=--local-only` should reuse successful unchanged work. Use `TB_FLAGS='--local-only --force'` to execute deliberately, or `--retry` to rerun a failed cached task after fixing its environment.

Cached test success is not a fresh execution. Force a run when investigating a flaky test or an external dependency. Deleting generated output and repeating a fully cached request is not a restoration test: Terrabuild restores prerequisite files when an executing consumer needs them.

`ci-main.yml` runs on pushes to every branch, including names containing `/`, and supports manual dispatch. Tag pushes remain handled by the release workflows. `ci-pr.yml` separately validates pull requests targeting `main`, so a push to an open PR triggers both branch and PR CI.
