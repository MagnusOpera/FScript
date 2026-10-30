#!/usr/bin/env bash
# Build the SDK-only fixture once, then load that binary into independently versioned hosts.
set -euo pipefail
cd "$(dirname "$0")/../.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
provider=tests/FScript.Cli.Tests.Fixtures.Valid/FScript.Cli.Tests.Fixtures.Valid.fsproj
dotnet build "$provider" -c Release --artifacts-path "$work/provider-artifacts" -o "$work/provider" -nologo
# No default catalog is needed by the provider; use an expression without stdlib dependencies.
cat > "$work/main.fss" <<'SCRIPT'
Ext.acceptNode (Ext.node (Ext.acceptPoint { X = 42 }))
SCRIPT
for version in 0.82.0 0.82.1; do
    dotnet publish src/FScript -c Release --no-self-contained --artifacts-path "$work/artifacts-$version" -p:Version="$version" -o "$work/$version" -nologo
    result=$(dotnet "$work/$version/fscript.dll" --no-default-externs --extern-assembly "$work/provider/FScript.Cli.Tests.Fixtures.Valid.dll" "$work/main.fss")
    test "$result" = '42'
done
echo 'One SDK-only provider loaded into both product assembly versions.'
