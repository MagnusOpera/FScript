#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
configuration="${FSCRIPT_BUILD_CONFIG:-Release}"
cli=(dotnet "src/FScript/bin/$configuration/net10.0/fscript.dll")
run_node_tests() {
    local report=$1
    shift
    mkdir -p "$(dirname "$report")"
    node --test --test-reporter=spec --test-reporter-destination=stdout \
        --test-reporter=junit --test-reporter-destination="$report" "$@"
}
case "${1:?Expected debugger, javascript, vscode, reports, or smoke}" in
    debugger)
        run_node_tests tests/FScript.DebugAdapter.Tests/TestResults/debugger.xml tests/FScript.DebugAdapter.Tests/*Tests.js
        ;;
    javascript)
        cd src/FScript.JavaScript
        run_node_tests TestResults/javascript.xml test/*.test.mjs
        ;;
    vscode)
        cd vscode-fscript
        run_node_tests TestResults/vscode.xml ../tests/FScript.LanguageServer.Tests/*Tests.js
        ;;
    reports)
        find src tests samples vscode-fscript \
            -type d \( -name bin -o -name obj -o -name node_modules \) -prune -o \
            -type f -path '*/TestResults/*' \( -name '*.trx' -o -name '*.xml' \) -print
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
