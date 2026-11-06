.PHONY: build test benchmark-interpreters benchmark-front-end benchmark-inference benchmark-evaluation benchmark-dotnet-comparison vscode-debug-smoke fable-build fable-test smoke-tests verify-changelog release-prepare clean publish publish-darwin publish-linux publish-windows pack-nuget publish-all website website-install website-build website-version website-typecheck

build: tb-build

test: check-changelog-format test-release-changelog test-terrabuild
	dotnet test FScript.sln -c Release
	node --test tests/FScript.DebugAdapter.Tests/*Tests.js

benchmark-interpreters:
	dotnet run --project benchmarks/FScript.Interpreter.Benchmarks -c Release -- $(or $(iterations),5) $(or $(size),2000)

benchmark-front-end:
	dotnet run --project benchmarks/FScript.FrontEnd.Benchmarks -c Release -- $(or $(iterations),5) $(or $(size),200)

benchmark-inference:
	dotnet run --project benchmarks/FScript.Inference.Benchmarks/FScript.Inference.Benchmarks.fsproj -c Release -- $(iterations) $(bindings)

benchmark-evaluation:
	dotnet run --project benchmarks/FScript.Evaluation.Benchmarks/FScript.Evaluation.Benchmarks.fsproj -c Release -- $(or $(iterations),20) $(or $(size),10000)

# Compile once, then compare exported native functions against optimized F#.
benchmark-dotnet-comparison:
	dotnet build benchmarks/FScript.DotNet.Comparison/FScript.DotNet.Comparison.fsproj -c Release
	DOTNET_TieredCompilation=0 DOTNET_ReadyToRun=0 dotnet benchmarks/FScript.DotNet.Comparison/bin/Release/net10.0/FScript.DotNet.Comparison.dll $(or $(iterations),500) $(or $(rounds),9) "$(or $(output),out/benchmarks/dotnet-vs-fsharp)" $(if $(filter true,$(verify)),--verify-only)

fable-build:
	dotnet tool restore
	cd src/FScript.JavaScript && npm run build

fable-test: fable-build
	cd src/FScript.JavaScript && npm test

smoke-tests:
	@set -e; \
	for script in samples/*.fss; do \
		echo "Running $$script"; \
		dotnet run --project src/FScript -- "$$script" > /dev/null; \
	done; \
	echo "Running samples/basic/basic.fss -- helloworld.bas"; \
	dotnet run --project src/FScript -- samples/basic/basic.fss -- helloworld.bas > /dev/null; \
	echo "Running samples/debugging/main.fss"; \
	dotnet run --project src/FScript -- samples/debugging/main.fss > /dev/null; \
	echo "Running samples/ffi (headless)"; \
	dotnet build samples/ffi/Provider/FfiSample.fsproj -c Release > /dev/null; \
	dotnet run --project src/FScript -- samples/ffi/main.fss -- --headless; \
	echo "Analyzing samples/task-sharing"; \
	dotnet build samples/task-sharing/TaskSharingSample.fsproj -c Release > /dev/null; \
	for script in samples/task-sharing/*.fss; do \
		dotnet run --project src/FScript -- analyze "$$script"; \
	done; \
	dotnet run --project src/FScript -- samples/task-sharing/ordered.fss > /dev/null; \
	dotnet run --project src/FScript -- --no-result samples/task-sharing/thread-safe.fss > /dev/null; \
	echo "All smoke tests passed."


verify-changelog: check-changelog-format
	REQUIRE_CHANGELOG_ALWAYS=true .github/scripts/check-unreleased-changelog.sh

release-prepare:
	./.github/scripts/release.sh "$(version)" "$(dryrun)"

clean:
	dotnet clean FScript.sln -c Release
	find . -type d -name bin -o -name obj | xargs rm -rf

config ?= Release
version ?= 0.0.0

publish:
	dotnet publish -c $(config) --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/dotnet src/FScript

publish-darwin:
	dotnet publish -c $(config) -r osx-arm64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/darwin/arm64 src/FScript

publish-linux:
	dotnet publish -c $(config) -r linux-x64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/linux/x64 src/FScript
	dotnet publish -c $(config) -r linux-arm64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/linux/arm64 src/FScript

publish-windows:
	dotnet publish -c $(config) -r win-x64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/windows/x64 src/FScript
	dotnet publish -c $(config) -r win-arm64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/windows/arm64 src/FScript

pack-nuget:
	dotnet pack -c $(config) -p:Version=$(version) -o $(PWD)/.out src/FScript
	dotnet pack -c $(config) -p:Version=$(version) -o $(PWD)/.out src/FScript.Language
	dotnet pack -c $(config) -p:Version=$(version) -o $(PWD)/.out src/FScript.Runtime

publish-all: build test publish publish-darwin publish-linux publish-windows pack-nuget

website-install:
	cd website && npm ci

website-build:
	cd website && npm run build

website:
	cd website && npm run start

website-version:
	cd website && npm run version-docs -- $(version)

website-typecheck:
	cd website && npm run typecheck

verify-sdk-provider:
	.github/scripts/verify-sdk-provider.sh

vscode-debug-smoke: build
	npm run compile --prefix vscode-fscript
	.github/scripts/test-vscode-debugger.sh

.PHONY: format-changelog check-changelog-format test-release-changelog

format-changelog:
	./.github/scripts/format-changelog.sh

check-changelog-format:
	./.github/scripts/format-changelog.sh --check

test-release-changelog:
	./.github/scripts/test-release-changelog.sh

# Container-backed whole-repository checks use Terrabuild from PATH.
.PHONY: tb-build tb-test tb-smoke tb-sdk tb-check test-terrabuild
TB_FLAGS ?=
TB_PARALLEL ?=

test-terrabuild:
	bash .github/scripts/test-terrabuild.sh
	bash .github/scripts/test-node-reports.sh
	node --test .github/scripts/test-collect-results.mjs
	node --test .github/scripts/test-workspace-versions.mjs
	node --test .github/scripts/test-toolchain-image.mjs

tb-build:
	FSCRIPT_BUILD_CONFIG=$(config) TB_PARALLEL=$(TB_PARALLEL) .build/run-terrabuild.sh build $(TB_FLAGS)

tb-test:
	FSCRIPT_BUILD_CONFIG=$(config) TB_PARALLEL=$(TB_PARALLEL) .build/run-terrabuild.sh test $(TB_FLAGS)

tb-smoke:
	FSCRIPT_BUILD_CONFIG=$(config) TB_PARALLEL=$(TB_PARALLEL) .build/run-terrabuild.sh smoke $(TB_FLAGS)

tb-sdk:
	FSCRIPT_BUILD_CONFIG=$(config) TB_PARALLEL=$(TB_PARALLEL) .build/run-terrabuild.sh sdk $(TB_FLAGS)

tb-check:
	FSCRIPT_BUILD_CONFIG=$(config) TB_PARALLEL=$(TB_PARALLEL) .build/run-terrabuild.sh build test smoke sdk reports $(TB_FLAGS)
