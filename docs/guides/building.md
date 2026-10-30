# Building FScript from source

For the Terrabuild workflow, install Git, GNU Make, Docker with a running daemon, and the native Terrabuild CLI. No host .NET SDK or Node.js installation is needed. Application compilation and testing execute inside a container.

## Terrabuild

Install the native release matching `workspace.version` in `WORKSPACE` from [Terrabuild releases](https://github.com/MagnusOpera/terrabuild/releases), or install it through Homebrew on macOS/Linux:

```sh
brew install magnusopera/tap/terrabuild
terrabuild version
terrabuild run toolchain build test check
```

`WORKSPACE` is the shared tool inventory: `workspace.version` pins the GitHub native Terrabuild installer, and `locals.versions` lists .NET SDK, Node.js, pnpm, VS Code packaging (`vsce`), and Open VSX (`ovsx`) versions. The toolchain passes these versions as Docker build arguments. Changing `WORKSPACE` also changes the toolchain image hash, invalidating dependent build caches.

`WORKSPACE` uses Docker containers and Insights workspace `6ae115ba87040d459b6ccf40`. The `toolchain` target first builds a local image from `toolchains/build-sdk/Dockerfile`, following the toolchain/application phase pattern. It contains the .NET SDK, Node.js, and pnpm versions declared in `WORKSPACE`, plus GNU Make, Git, Python, clang, and zlib headers. Every application extension uses this image; only image bootstrapping calls the host Docker daemon. The image is rebuilt through Docker layer caching on each invocation and is never pushed to a registry. Existing npm lockfiles remain authoritative for the Node checks.

The solution builds once; nine .NET test suites then run independently with `--no-build --no-restore`. Test results go to each suite's `TestResults` directory. The debugger adapter tests also run under `test`. The `check` target runs the VS Code extension, Fable JavaScript tests, and website build. Terrabuild uses the available cores by default; set `--parallel` to impose a limit when needed.

```sh
terrabuild run toolchain build test --local-only
terrabuild explain toolchain build test --local-only
terrabuild run toolchain test --force --local-only
```

Local caching needs no Insights account. `--local-only` prevents remote cache access; `--force` reruns tasks despite cached success. The solution build caches `build/artifacts/solution.tar`; the `materialize` task extracts it to the shared output directories and restores NuGet packages before tests or smoke checks. `make build` requests both `build` and `materialize`. For a standalone Terrabuild build, use `terrabuild run toolchain build materialize`. TRX reports are also declared artifacts. Node checks run each time and install their own dependencies.

To connect a developer machine, supply your credentials through environment variables:

```sh
terrabuild login --workspace 6ae115ba87040d459b6ccf40 \
  --token "$TERRABUILD_TOKEN" --masterkey "$TERRABUILD_MASTER_KEY"
```

The shared `.github/actions/setup-tools` action installs native Terrabuild without installing .NET on the host. Main/PR CI and website publishing use this default. CI uses the runner’s available cores and connects to Insights when both repository secrets `TERRABUILD_TOKEN` and `TERRABUILD_MASTER_KEY` are available. Fork PRs and repositories without those secrets use local caching. CI uploads per-suite TRX files even when a task fails. The independently versioned SDK provider check runs afterward in the same container via `terrabuild run toolchain sdk` using temporary build artifacts so version checks cannot overwrite cached solution outputs. Tag builds retain the existing release preparation and draft release workflow. Direct release packaging, NuGet publishing, and native macOS CLI verification explicitly opt into host .NET through the same action; those commands are not containerized. Host Node.js, vsce, and ovsx installation is also opt-in for release jobs, with versions read from the shared inventory.

See the [Terrabuild documentation](https://terrabuild.io/docs/) for caching, task selection, and Insights.

## Make shortcuts

The common Make targets delegate to the containerized Terrabuild graph:

| Make target | Terrabuild task |
| --- | --- |
| `build` | Solution build |
| `test` | All .NET suites, debugger adapter, and build workflow regression tests |
| `smoke-tests` | Sample smoke tests, after building the solution |
| `verify-sdk-provider` | Independently versioned SDK compatibility check |
| `fable-build` / `fable-test` | JavaScript build / build and tests |
| `website-install` | Install website dependencies |
| `website-build` | Install dependencies and build the website |
| `website-typecheck` | Install dependencies and typecheck the website |

```sh
make build
make test parallel=4 TERRABUILD_ARGS=--local-only
make smoke-tests
make fable-test
make website-build
```

`TERRABUILD` selects the executable (default `terrabuild`), `parallel` optionally limits concurrency (unset by default, so Terrabuild uses the available cores), and `TERRABUILD_ARGS` adds run options such as `--local-only` or `--force`. Terrabuild tasks invoke the underlying tools directly, so they never recurse into these Make shortcuts.

Benchmarks, `clean`, the interactive website server, website versioning, VS Code UI smoke tests, and release packaging retain direct commands. Install the .NET SDK and Node.js/npm versions from `WORKSPACE` when using those direct commands, along with GNU Make, Git, and any platform-specific publishing or editor tools. For host website development, run `npm ci --prefix website` before `make website`. Changelog verification and release preparation also remain direct. The tag workflow installs Terrabuild and connects to Insights when credentials are available before `make publish-all` delegates its build and test prerequisites.

For compiling an FScript program rather than building this repository, see [Compilation](compilation.md).
