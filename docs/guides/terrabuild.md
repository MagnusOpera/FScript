# Container-backed builds with Terrabuild

Install the latest stable Terrabuild release and a working Docker engine. The graph prepares its toolchain image automatically. Trusted GitHub builds publish missing images to GHCR, and test shards pull them. .NET, Node, and Clang are supplied by that image; they need not be installed on the host for these targets.

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
terrabuild run build test smoke sdk reports --local-only
```

The Make targets check Terrabuild's JSON completion status as well as its process exit code. Use them in automation with Terrabuild 0.200.1, which can return zero after reporting failed tasks. A missing or unsuccessful completion report fails the Make target. The wrapper retains the report as `.terrabuild/result.json` and console output as `.terrabuild/run.log`, including when a task fails.

## Shared tool versions

`locals.versions` in `WORKSPACE` defines the .NET SDK, Node, Terrabuild release selector, VS Code packaging tool, and Open VSX publishing tool. Change these quoted literal values in one place. Terrabuild defaults to `latest`.

The toolchain project passes the SDK and Node versions as Docker build arguments. Terrabuild generates the image hash tag. The toolchain includes `WORKSPACE` as an input so SDK and Node updates invalidate the image; this currently also invalidates it for other workspace edits. The shared `workspace-versions` GitHub action reads the same values before tool installation; release jobs read them from the checked-out release tag. The reader fails on missing, duplicate, or computed version values rather than choosing a fallback.

## Containers and project layout

The toolchain phase builds `.build/toolchain/Dockerfile` with the .NET SDK and Node versions from `WORKSPACE`, plus Clang and zlib headers. Only this Docker image preparation runs on the host. `@dotnet`, `@npm`, and the custom `workflow` extension all specify the GHCR toolchain image and an explicit Linux architecture. Images use `ghcr.io/magnusopera/fscript-toolchain` and carry the source repository label. No application deployment occurs. Persistent MSBuild nodes and shared compiler servers are disabled in the image because CLI tests capture subprocess output; retained server processes can keep those pipes open after a command exits.

Terrabuild stops discovery at a `PROJECT` file. The FFI sample therefore keeps scripts and `fscript.toml` at `samples/ffi`, with sibling `Provider`, `Renderer`, and `Tests` projects below it. Build the provider directly with `dotnet build samples/ffi/Provider/FfiSample.fsproj -c Release` when using the existing host workflow.

Project-reference dependencies build before their consumers. Tests run already-built binaries with `--no-build --no-restore`. Tests that launch another project's executable must directly depend on that project's build target, not just their own cached build. FFI test targets consume `target.^build` to restore the CLI. The GitHub-only website `test_ci` target directly consumes `target.^build` for generated JavaScript and runs `npm ci` before typechecking to recreate package launcher symlinks on a fresh runner. Its normal local `test` target is unchanged. Conditional sibling-SDK project references are declared explicitly in the graph; ordinary builds use the published SDK package. Non-.NET checks declare their inputs and upstream builds too. The SDK compatibility check runs in a temporary source copy because it deliberately publishes different assembly versions.

Builds use the normal `bin`, `obj`, `node_modules`, and generated website directories on the mounted checkout. They contain Linux outputs after a container run. Rebuild through the host toolchain before relying on native host apphosts or platform-specific Node dependencies. Avoid running host and container builds concurrently in the same checkout.

## Local and shared caching

The workspace ID is configured in `WORKSPACE`. Local caching works without an Insights account; `--local-only` disables shared caching. Omit `TB_FLAGS=--local-only` after connecting the workspace to allow shared reuse. Do not put tokens or master keys in repository files.

The PR and branch-push GitHub workflows share `.github/actions/setup-tools` to install the latest stable Terrabuild release and configure authentication. Both call the shared `ci.yml` workflow, which builds once, scatters tests across seven runners, and joins the results. The build job runs `make build`; each test runner requests only its shard's `test` target and lets Terrabuild restore prerequisite outputs from the shared cache. The graph supplies .NET, Node, and the Native AOT toolchain through Docker. Configure repository secrets `TERRABUILD_TOKEN` and `TERRABUILD_MASTER_KEY` to enable shared caching. Without both secrets, including on fork PRs, CI uses local caching and rebuilds prerequisites on each test runner; cache sharing across jobs requires both credentials. The shared `collect-build-results` action collects the latest TRX result from each test output directory and all Node JUnit files into `.out/test-results`, uploads them and publishes both report formats in the GitHub build summary, including after a failed build. Fable, VS Code, and debugger tests retain console output while also writing JUnit XML. A reporting target consumes test outputs so cached reports are restored before collection. Terrabuild diagnostics, its JSON completion report, and captured console output are uploaded as a separate `terrabuild-logs-*` artifact. Its `terrabuild-debug.json` includes performance timings, slowest tasks, the critical chain, and FScript performance data for build optimization.

Managed build outputs and successful test results are cacheable separately. npm dependencies are kept in the local workspace cache. The toolchain mirrors `matis-next`: a project with the `@docker` initializer and a `dist` target uses `@docker build`, with `~external` artifact policy. Terrabuild generates the hash tag, builds and publishes the image in CI, and caches the external artifact. Test jobs reuse the successful target and Docker pulls the image when a container needs it. The shared `setup-tools` action authenticates Docker with `GITHUB_TOKEN`. Only the build job has package write permission; test jobs have package read permission. Publishing a missing image requires registry write access. Fully cached requests skip execution; the phase barrier does not force application cache hits to rebuild. The configured Linux architecture separates ARM64 and AMD64 artifacts.

Use `terrabuild explain build test --local-only` to inspect cache decisions. Repeating `make tb-check TB_FLAGS=--local-only` should reuse successful unchanged work. Use `TB_FLAGS='--local-only --force'` to execute deliberately, or `--retry` to rerun a failed cached task after fixing its environment.

Cached test success is not a fresh execution. Force a run when investigating a flaky test or an external dependency. Deleting generated output and repeating a fully cached request is not a restoration test: Terrabuild restores prerequisite files when an executing consumer needs them.

`ci-main.yml` runs on pushes to every branch, including names containing `/`, and supports manual dispatch. Tag pushes remain handled by the release workflows. `ci-pr.yml` separately validates pull requests targeting `main`, so a push to an open PR triggers both branch and PR CI. The shared workflow holds one concurrency lock per source repository and branch, with `cancel-in-progress: false`, following the lock policy in `matis-next`. Branch, PR, and manual runs for the same source branch execute sequentially; separate branches can run concurrently. Test shards remain parallel inside the active workflow. GitHub retains one pending run per lock and replaces an older pending run if another arrives.

## GitHub test matrix

The GitHub-only `.build/ci-shards.json` lists a static matrix. `.build/ci/*/PROJECT` defines each shard's dependency selection. The build job validates that the union covers every test project without selecting unrelated tests. Local `make build`, `make test`, and `make tb-check` retain their existing behavior; local full FFI tests still run both Native AOT variants.

| Shard | Checks |
| --- | --- |
| core | Language, runtime, compiler, conformance, interpreter, language server, and managed FFI tests |
| backend | .NET backend tests |
| cli | CLI tests |
| ffi-default | Native AOT teapot without size optimization |
| ffi-size | Native AOT teapot with size optimization |
| web | Fable, VS Code, and website tests |
| integration | Debugger adapter, all sample smoke tests, and SDK compatibility |

The FFI categories partition its cases into managed tests and the two Native AOT variants; their results and cache entries are separate. Each shard's executing `test` target consumes just its test outputs, restoring cached reports before artifact upload. Every runner retains its own diagnostics and test artifact. The join downloads the artifacts into separate directories, publishes combined TRX and JUnit summaries, and requires the build and every shard to succeed. A failed shard does not cancel the others. The shared workflow sets `TERRABUILD_GROUP` to `<repository>:<github.run_id>:<github.run_attempt>`; the wrapper passes it explicitly with `--group` for the build and every shard. Each rerun gets a separate group, so Insights shows one build group per workflow attempt. Local commands do not set a group by default.

The baseline [successful run](https://github.com/MagnusOpera/FScript/actions/runs/42246670574) took 7m34s in Terrabuild. Its trace identified FFI tests at 5m32s, SDK compatibility at 1m59s, and the website build at 1m36s. The matrix separates those workloads and splits the sequential AOT pair. Runner setup, cache transfers, and the build barrier add overhead; compare subsequent GitHub timings and `terrabuild-logs-*` artifacts before claiming a speedup.

The toolchain image is `ghcr.io/magnusopera/fscript-toolchain:<Terrabuild hash>`. `WORKSPACE` configures the Docker image repository and platform defaults, and consumers reference `project.toolchain.version`. There is no custom image preparation script or manually constructed version suffix. The native Docker build disables provenance attestations with `--provenance=false` to publish a single platform manifest. The previous attested index was readable in GHCR, but its referenced image manifest returned `manifest unknown` on fresh runners after successful registry login.
