#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
make_command=$(command -v make)
mkdir -p "$fixture/repo/.build" "$fixture/bin"
cp "$root/Makefile" "$fixture/repo/Makefile"
cp "$root/.build/run-terrabuild.sh" "$fixture/repo/.build/"
# Isolate PATH so an installed Terrabuild cannot hide the missing-tool case.
for tool in bash dirname mktemp rm grep; do
    ln -s "$(command -v "$tool")" "$fixture/bin/$tool"
done
cat > "$fixture/bin/terrabuild" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$TB_TEST_ARGS"
report=
while [[ $# -gt 0 ]]; do
    if [[ "$1" == --out ]]; then report=$2; shift; fi
    shift
done
case "$TB_TEST_RESULT" in
    success) printf '{"status":"success"}\n' > "$report" ;;
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

run_build() {
    PATH="$fixture/bin" TB_TEST_RESULT="$1" "$make_command" --no-print-directory -C "$fixture/repo" \
        build config=Debug TB_PARALLEL="${2-2}" TB_FLAGS='--local-only --force' > "$fixture/log" 2>&1
}

if ! run_build success; then cat "$fixture/log"; exit 1; fi
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

for result in failed-graph failed-process missing-report; do
    if run_build "$result"; then
        echo "ERROR: make build accepted $result." >&2
        exit 1
    fi
done
rm "$fixture/bin/terrabuild"
if run_build success; then
    echo "ERROR: make build succeeded without Terrabuild on PATH." >&2
    exit 1
fi
test ! -e "$TB_TEST_DOTNET"
echo "Terrabuild Makefile checks passed."
