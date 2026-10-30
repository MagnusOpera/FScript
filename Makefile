.PHONY: build test benchmark-interpreters benchmark-front-end benchmark-inference benchmark-evaluation benchmark-dotnet-comparison vscode-debug-smoke fable-build fable-test smoke-tests verify-changelog release-prepare clean publish publish-darwin publish-linux publish-windows pack-nuget publish-all website website-install website-build website-version website-typecheck verify-sdk-provider

TERRABUILD ?= terrabuild
TERRABUILD_ARGS ?=
TERRABUILD_RUN = $(TERRABUILD) run toolchain $(1) $(if $(strip $(parallel)),--parallel $(parallel)) $(TERRABUILD_ARGS)

build:
	$(call TERRABUILD_RUN,build materialize)

test:
	$(call TERRABUILD_RUN,test)

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
	$(call TERRABUILD_RUN,javascript_build) --project src/FScript.JavaScript

fable-test:
	$(call TERRABUILD_RUN,check) --project src/FScript.JavaScript

smoke-tests:
	$(call TERRABUILD_RUN,smoke)


verify-changelog:
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
	$(call TERRABUILD_RUN,install) --project website

website-build:
	$(call TERRABUILD_RUN,check) --project website

website:
	cd website && npm run start

website-version:
	cd website && npm run version-docs -- $(version)

website-typecheck:
	$(call TERRABUILD_RUN,typecheck) --project website

verify-sdk-provider:
	$(call TERRABUILD_RUN,sdk)

vscode-debug-smoke: build
	npm run compile --prefix vscode-fscript
	.github/scripts/test-vscode-debugger.sh
