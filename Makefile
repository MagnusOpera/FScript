.PHONY: build test clean

build:
	dotnet build FScript.sln -c Release

test:
	dotnet test FScript.sln -c Release

clean:
	dotnet clean FScript.sln -c Release
	find . -type d -name bin -o -name obj | xargs rm -rf
