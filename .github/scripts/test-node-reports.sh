#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/.build" "$fixture/vscode-fscript" "$fixture/src/FScript.JavaScript/test" \
    "$fixture/tests/FScript.DebugAdapter.Tests" "$fixture/tests/FScript.LanguageServer.Tests"
cp "$root/.build/checks.sh" "$fixture/.build/"

for suite in debugger javascript vscode; do
    case "$suite" in
        debugger)
            test_file=tests/FScript.DebugAdapter.Tests/FixtureTests.js
            report=tests/FScript.DebugAdapter.Tests/TestResults/debugger.xml
            import="const test = require('node:test');" ;;
        javascript)
            test_file=src/FScript.JavaScript/test/fixture.test.mjs
            report=src/FScript.JavaScript/TestResults/javascript.xml
            import="import test from 'node:test';" ;;
        vscode)
            test_file=tests/FScript.LanguageServer.Tests/FixtureTests.js
            report=vscode-fscript/TestResults/vscode.xml
            import="const test = require('node:test');" ;;
    esac
    printf '%s\ntest("passing report fixture", () => {});\n' "$import" > "$fixture/$test_file"
    bash "$fixture/.build/checks.sh" "$suite" > "$fixture/console.log" 2>&1
    grep -q '<testcase' "$fixture/$report"
    grep -q 'passing report fixture' "$fixture/console.log"
    if grep -q '<failure' "$fixture/$report"; then
        echo "ERROR: $suite reported a failure for a passing test." >&2
        exit 1
    fi

    printf '%s\ntest("failing report fixture", () => { throw new Error("expected failure"); });\n' "$import" > "$fixture/$test_file"
    if bash "$fixture/.build/checks.sh" "$suite" > "$fixture/console.log" 2>&1; then
        echo "ERROR: $suite accepted a failing test." >&2
        exit 1
    fi
    grep -q '<failure' "$fixture/$report"
    grep -q 'failing report fixture' "$fixture/console.log"
done
echo 'Node console and JUnit report checks passed.'
