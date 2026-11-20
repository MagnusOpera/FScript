#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
make_command=$(command -v make)
mkdir -p "$fixture/repo/.build" "$fixture/bin"
cp "$root/Makefile" "$fixture/repo/Makefile"
cp "$root/.build/run-terrabuild.sh" "$fixture/repo/.build/"
# Stub changelog validation so make test needs no host Node.js/npm or .NET SDK.
mkdir -p "$fixture/repo/.github/scripts"
printf '#!/usr/bin/env bash\nexit 0\n' > "$fixture/repo/.github/scripts/format-changelog.sh"
chmod +x "$fixture/repo/.github/scripts/format-changelog.sh"
# Isolate PATH so an installed Terrabuild cannot hide the missing-tool case.
for tool in bash dirname mkdir rm grep tee; do
    ln -s "$(command -v "$tool")" "$fixture/bin/$tool"
done
cat > "$fixture/bin/terrabuild" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$TB_TEST_ARGS"
if [[ -p /dev/fd/1 ]]; then
    echo 'Terrabuild stdout was piped' >&2
    exit 1
fi
echo 'Terrabuild test output'
report=
while [[ $# -gt 0 ]]; do
    if [[ "$1" == --out ]]; then report=$2; shift; fi
    shift
done
case "$TB_TEST_RESULT" in
    success) printf '{"status":"success"}\n' > "$report" ;;
    cached-failure)
        if grep -qx -- '--retry' "$TB_TEST_ARGS"; then
            printf '{"status":"success"}\n' > "$report"
        else
            printf '{"status":"failure"}\n' > "$report"
        fi ;;
    failed-graph) printf '{"status":"failure"}\n' > "$report" ;;
    failed-process) printf '{"status":"success"}\n' > "$report"; exit 7 ;;
    missing-report) ;;
esac
STUB
cat > "$fixture/bin/dotnet" <<'STUB'
#!/usr/bin/env bash
printf 'called\n' > "$TB_TEST_DOTNET"
exit 1
STUB
chmod +x "$fixture/bin/terrabuild" "$fixture/bin/dotnet"
export TB_TEST_ARGS="$fixture/args" TB_TEST_DOTNET="$fixture/dotnet-called"
# The ungrouped cases model normal local builds, even when tested inside CI.
unset TERRABUILD_GROUP GITHUB_RUN_ATTEMPT

run_build() {
    PATH="$fixture/bin" TB_TEST_RESULT="$1" "$make_command" --no-print-directory -C "$fixture/repo" \
        build config=Debug TB_PARALLEL="${2-2}" TB_FLAGS='--local-only --force' > "$fixture/log" 2>&1
}

if ! run_build success; then cat "$fixture/log"; exit 1; fi
grep -qx 'Terrabuild test output' "$fixture/log"
grep -qx '{"status":"success"}' "$fixture/repo/.terrabuild/result.json"
cat > "$fixture/expected-args" <<'ARGS'
run
build
--local-only
--force
--variable
config=Debug
--parallel
2
--out
ARGS
head -n 9 "$fixture/args" > "$fixture/actual-args"
diff -u "$fixture/expected-args" "$fixture/actual-args"
test "$(wc -l < "$fixture/args" | tr -d ' ')" = 10

if ! run_build success ''; then cat "$fixture/log"; exit 1; fi
if grep -q -- '--parallel' "$fixture/args"; then
    echo "ERROR: make build overrides Terrabuild's default parallelism." >&2
    exit 1
fi
test "$(wc -l < "$fixture/args" | tr -d ' ')" = 8

export TERRABUILD_GROUP='FScriptLang/FScript:12345:2'
if ! run_build success ''; then cat "$fixture/log"; exit 1; fi
cat > "$fixture/expected-group-args" <<'ARGS'
run
build
--local-only
--force
--variable
config=Debug
--group
FScriptLang/FScript:12345:2
--out
.terrabuild/result.json
ARGS
diff -u "$fixture/expected-group-args" "$fixture/args"
unset TERRABUILD_GROUP

for result in failed-graph failed-process missing-report; do
    if run_build "$result"; then
        echo "ERROR: make build accepted $result." >&2
        exit 1
    fi
    grep -qx 'Terrabuild test output' "$fixture/log"
done
# The public test entry point must select the cached graph, forward settings,
# and fail for graph/process/report errors just like the build entry point.
run_test() {
    PATH="$fixture/bin" TB_TEST_RESULT="$1" "$make_command" --no-print-directory -C "$fixture/repo" \
        test config=Release TB_PARALLEL=3 TB_FLAGS='--local-only --retry' > "$fixture/log" 2>&1
}
if ! run_test success; then cat "$fixture/log"; exit 1; fi
cat > "$fixture/expected-test-args" <<'ARGS'
run
test
--local-only
--retry
--variable
config=Release
--parallel
3
--out
.terrabuild/result.json
ARGS
diff -u "$fixture/expected-test-args" "$fixture/args"
for result in failed-graph failed-process missing-report; do
    if run_test "$result"; then
        echo "ERROR: make test accepted $result." >&2
        exit 1
    fi
done
# Match CI's TB_FLAGS override, which removes Make's default --retry.
run_cached_build() {
    PATH="$fixture/bin" TB_TEST_RESULT=cached-failure "$make_command" --no-print-directory -C "$fixture/repo" \
        build TB_FLAGS='--debug --log' > "$fixture/log" 2>&1
}
if run_cached_build; then
    echo 'ERROR: a local invocation unexpectedly retried the failed cache entry.' >&2
    exit 1
fi
export GITHUB_RUN_ATTEMPT=1
if run_cached_build; then
    echo 'ERROR: the first GitHub attempt unexpectedly retried the failed cache entry.' >&2
    exit 1
fi
for attempt in 2 10; do
    export GITHUB_RUN_ATTEMPT=$attempt
    if ! run_cached_build; then cat "$fixture/log"; exit 1; fi
    test "$(grep -cx -- '--retry' "$fixture/args")" = 1
    grep -qx -- '--debug' "$fixture/args"
    grep -qx -- '--log' "$fixture/args"
done
# Shards call the wrapper directly, bypassing Make's default flags.
export GITHUB_RUN_ATTEMPT=2
PATH="$fixture/bin" TB_TEST_RESULT=cached-failure bash "$fixture/repo/.build/run-terrabuild.sh" \
    test --project tests/example --restore --local-only > "$fixture/log" 2>&1
test "$(grep -cx -- '--retry' "$fixture/args")" = 1
grep -qx -- '--restore' "$fixture/args"
grep -qx -- '--local-only' "$fixture/args"
# Explicit local/CI retry flags are preserved without duplication.
if ! run_test success; then cat "$fixture/log"; exit 1; fi
test "$(grep -cx -- '--retry' "$fixture/args")" = 1
unset GITHUB_RUN_ATTEMPT
rm "$fixture/bin/terrabuild"
if run_build success; then
    echo "ERROR: make build succeeded without Terrabuild on PATH." >&2
    exit 1
fi
if run_test success; then
    echo "ERROR: make test succeeded without Terrabuild on PATH." >&2
    exit 1
fi
test ! -e "$TB_TEST_DOTNET"
echo "Terrabuild Makefile checks passed."
