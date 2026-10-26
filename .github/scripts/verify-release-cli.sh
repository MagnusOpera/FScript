#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <released-cli> <version>" >&2
  exit 2
fi

cli="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
version="$2"
repo_root="$(git rev-parse --show-toplevel)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT
export DOTNET_BUNDLE_EXTRACT_BASE_DIR="$test_root/extracted"

reported_version="$("$cli" version)"
case "$reported_version" in
  "$version"|"$version"+*) ;;
  *) echo "Expected CLI $version, got $reported_version" >&2; exit 1 ;;
esac
printf 'Testing released CLI %s\n' "$reported_version"

dotnet build "$repo_root/samples/ffi/FfiSample.fsproj" -c Release
"$cli" analyze "$repo_root/samples/ffi/main.fss"

mkdir -p "$test_root/source"
printf "[runtime]\nextern-assemblies = ['%s']\n" \
  "$repo_root/samples/ffi/bin/Release/net10.0/FScript.Ffi.Sample.dll" > "$test_root/source/fscript.toml"
printf 'Math.sqrt 81.0\n' > "$test_root/source/check.fss"
for target in csharp dotnet; do
  "$cli" compile "$test_root/source/check.fss" --target "$target" --output "$test_root/$target"
  if [[ "$target" == dotnet ]]; then
    result="$(dotnet "$test_root/dotnet/check.dll")"
  else
    result="$(dotnet run --project "$test_root/csharp/Program.csproj" -c Release)"
  fi
  case "$result" in
    9|9.0) ;;
    *) echo "Unexpected compiled $target result: $result" >&2; exit 1 ;;
  esac
done

"$cli" compile "$repo_root/samples/ffi/main.fss" --target dotnet --aot --output "$test_root/teapot"
"$test_root/teapot/main" --headless
printf 'Released CLI passed configuration, C#, .NET, and Native AOT smoke tests.\n'
