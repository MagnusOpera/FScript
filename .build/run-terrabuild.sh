#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p .terrabuild
report=.terrabuild/result.json
rm -f "$report"
parallel_args=()
if [[ -n "${TB_PARALLEL:-}" ]]; then
    parallel_args=(--parallel "$TB_PARALLEL")
fi
group_args=()
if [[ -n "${TERRABUILD_GROUP:-}" ]]; then
    group_args=(--group "$TERRABUILD_GROUP")
fi
terrabuild run "$@" --variable "config=${FSCRIPT_BUILD_CONFIG:-Release}" ${parallel_args[@]+"${parallel_args[@]}"} ${group_args[@]+"${group_args[@]}"} --out "$report" 2>&1 | tee .terrabuild/run.log
# Terrabuild 0.200.1 can exit zero after reporting a failed graph. Require the
# machine-readable completion status too, including for a fully cached run.
if ! grep -Eq '"status"[[:space:]]*:[[:space:]]*"success"' "$report"; then
    echo "Terrabuild did not report a successful build; inspect the task logs above." >&2
    exit 1
fi
