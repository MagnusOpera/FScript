#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
configuration="${FSCRIPT_BUILD_CONFIG:-Release}"
cli=(dotnet "src/FScript/bin/$configuration/net10.0/fscript.dll")
case "${1:?Expected debugger or smoke}" in
    debugger)
        mkdir -p tests/FScript.DebugAdapter.Tests/TestResults
        node --test --test-reporter=junit --test-reporter-destination=tests/FScript.DebugAdapter.Tests/TestResults/debugger.xml tests/FScript.DebugAdapter.Tests/*Tests.js
        ;;
    smoke)
        for script in samples/*.fss; do
            echo "Running $script"
            "${cli[@]}" "$script" > /dev/null
        done
        "${cli[@]}" samples/basic/basic.fss -- helloworld.bas > /dev/null
        "${cli[@]}" samples/debugging/main.fss > /dev/null
        "${cli[@]}" samples/ffi/main.fss -- --headless
        for script in samples/task-sharing/*.fss; do "${cli[@]}" analyze "$script"; done
        "${cli[@]}" samples/task-sharing/ordered.fss > /dev/null
        "${cli[@]}" --no-result samples/task-sharing/thread-safe.fss > /dev/null
        echo "All smoke tests passed."
        ;;
    *) echo "Unknown check: $1" >&2; exit 2 ;;
esac
