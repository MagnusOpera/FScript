#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
# Terrabuild supplies the Release solution build before invoking this script.
cli=src/FScript/bin/Release/net10.0/fscript.dll
for script in samples/*.fss; do
  echo "Running $script"
  dotnet "$cli" "$script" > /dev/null
done
echo "Running samples/basic/basic.fss -- helloworld.bas"
dotnet "$cli" samples/basic/basic.fss -- helloworld.bas > /dev/null
echo "Running samples/debugging/main.fss"
dotnet "$cli" samples/debugging/main.fss > /dev/null
echo "Running samples/ffi (headless)"
dotnet "$cli" samples/ffi/main.fss -- --headless
echo "Analyzing samples/task-sharing"
for script in samples/task-sharing/*.fss; do
  dotnet "$cli" analyze "$script"
done
dotnet "$cli" samples/task-sharing/ordered.fss > /dev/null
dotnet "$cli" --no-result samples/task-sharing/thread-safe.fss > /dev/null
echo "All smoke tests passed."
