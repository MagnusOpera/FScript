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
# GitHub reruns must retry failed cache entries even when CI overrides TB_FLAGS.
# Apply this here so build jobs and direct test-shard invocations behave alike.
retry_args=()
if [[ "${GITHUB_RUN_ATTEMPT:-1}" =~ ^[0-9]+$ ]] && (( 10#${GITHUB_RUN_ATTEMPT:-1} > 1 )); then
    retry_args=(--retry)
    for arg in "$@"; do
        if [[ "$arg" == --retry ]]; then
            retry_args=()
            break
        fi
    done
fi
terrabuild run "$@" ${retry_args[@]+"${retry_args[@]}"} --variable "config=${FSCRIPT_BUILD_CONFIG:-Release}" ${parallel_args[@]+"${parallel_args[@]}"} ${group_args[@]+"${group_args[@]}"} --out "$report"
# Terrabuild 0.200.1 can exit zero after reporting a failed graph. Require the
# machine-readable completion status too, including for a fully cached run.
if ! grep -Eq '"status"[[:space:]]*:[[:space:]]*"success"' "$report"; then
    echo "Terrabuild did not report a successful build; inspect the task logs above." >&2
    exit 1
fi
