#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
case "$1" in
  sdk) bash .github/scripts/verify-sdk-provider.sh ;;
  build)
    dotnet build FScript.sln -c Release
    mkdir -p build/artifacts
    find src tests samples benchmarks -type d \( -name bin -o -name obj \) -prune -print0 \
      | tar --null -cf build/artifacts/solution.tar -T -
    ;;
  materialize)
    tar --no-same-owner -xf build/artifacts/solution.tar
    # Cached obj files import package SDK targets; restore packages on fresh runners.
    dotnet restore FScript.sln
    ;;
  debug-adapter) node --test tests/FScript.DebugAdapter.Tests/*Tests.js tests/Build.Tests/*Tests.js ;;
  website)
    # SWC requires a private cache below a trusted (sticky) parent directory.
    # Terrabuild's shared HOME/TMP mounts can be group-writable.
    swc_cache=$(mktemp -d /tmp/fscript-swc.XXXXXX)
    trap 'rm -rf "$swc_cache"' EXIT
    export SWC_NATIVE_BINDING_CACHE="$swc_cache"
    cd website
    npm ci
    npm run build
    ;;
  extension) cd vscode-fscript; npm ci; npm test ;;
  fable-build|fable)
    dotnet tool restore
    cd src/FScript.JavaScript
    npm run build
    if [[ "$1" == fable ]]; then npm test; fi
    ;;
  smoke) bash .github/scripts/smoke-tests.sh ;;
  website-install) cd website; npm ci ;;
  website-typecheck) cd website; npm ci; npm run typecheck ;;
  *) echo "Unknown check: $1" >&2; exit 1 ;;
esac
