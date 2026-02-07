.PHONY: build test clean publish publish-darwin publish-linux publish-windows pack-nuget publish-all

build:
	dotnet build FScript.sln -c Release

test:
	dotnet test FScript.sln -c Release

clean:
	dotnet clean FScript.sln -c Release
	find . -type d -name bin -o -name obj | xargs rm -rf

config ?= Release
version ?= 0.0.0

publish:
	dotnet publish -c $(config) --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/dotnet src/FScript

publish-darwin:
	dotnet publish -c $(config) -r osx-x64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/darwin/x64 src/FScript
	dotnet publish -c $(config) -r osx-arm64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/darwin/arm64 src/FScript

publish-linux:
	dotnet publish -c $(config) -r linux-x64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/linux/x64 src/FScript
	dotnet publish -c $(config) -r linux-arm64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/linux/arm64 src/FScript

publish-windows:
	dotnet publish -c $(config) -r win-x64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/windows/x64 src/FScript
	dotnet publish -c $(config) -r win-arm64 --self-contained -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:Version=$(version) -o $(PWD)/.out/windows/arm64 src/FScript

pack-nuget:
	dotnet pack -c $(config) -p:Version=$(version) -o $(PWD)/.out src/FScript.Language
	dotnet pack -c $(config) -p:Version=$(version) -o $(PWD)/.out src/FScript.Runtime

publish-all: clean build test publish publish-darwin publish-linux publish-windows pack-nuget
