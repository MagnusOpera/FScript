#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT_DIR"

BASE_REF="${GITHUB_BASE_REF:-}"
HEAD_REF="${GITHUB_HEAD_REF:-}"

if [[ -n "$BASE_REF" ]]; then
  git fetch --no-tags --depth=1 origin "$BASE_REF" >/dev/null 2>&1 || true
  DIFF_RANGE="origin/${BASE_REF}...HEAD"
else
  if git rev-parse --verify HEAD~1 >/dev/null 2>&1; then
    DIFF_RANGE="HEAD~1...HEAD"
  else
    echo "No comparable git range available; skipping changelog check."
    exit 0
  fi
fi

mapfile -t CHANGED_FILES < <(git diff --name-only "$DIFF_RANGE")

if [[ ${#CHANGED_FILES[@]} -eq 0 ]]; then
  echo "No changed files detected; skipping changelog check."
  exit 0
fi

TRIGGERS=(
  '^src/'
  '^tests/'
  '^vscode-fscript/'
  '^docs/'
  '^samples/'
)

EXEMPTS=(
  '^\.github/'
  '^LICENSE$'
  '^README\.md$'
  '^AGENTS\.md$'
  '^CHANGELOG\.md$'
  '^\.gitignore$'
)

needs_changelog=false
for file in "${CHANGED_FILES[@]}"; do
  is_trigger=false
  for pat in "${TRIGGERS[@]}"; do
    if [[ "$file" =~ $pat ]]; then
      is_trigger=true
      break
    fi
  done

  if [[ "$is_trigger" == false ]]; then
    continue
  fi

  is_exempt=false
  for pat in "${EXEMPTS[@]}"; do
    if [[ "$file" =~ $pat ]]; then
      is_exempt=true
      break
    fi
  done

  if [[ "$is_exempt" == false ]]; then
    needs_changelog=true
    break
  fi
done

if [[ "$needs_changelog" == false ]]; then
  echo "No changelog-required files changed."
  exit 0
fi

if ! printf '%s\n' "${CHANGED_FILES[@]}" | grep -qx 'CHANGELOG.md'; then
  echo "ERROR: Functional changes detected but CHANGELOG.md was not updated."
  echo "Add a short one-line bullet under ## [Unreleased]."
  exit 1
fi

if ! grep -q '^## \[Unreleased\]' CHANGELOG.md; then
  echo "ERROR: CHANGELOG.md must contain a top-level '## [Unreleased]' section."
  exit 1
fi

UNRELEASED_BLOCK=$(awk '
  /^## \[Unreleased\]/{in_block=1; next}
  /^## \[/{if(in_block){exit}}
  in_block{print}
' CHANGELOG.md)

if [[ -z "${UNRELEASED_BLOCK//$'\n'/}" ]]; then
  echo "ERROR: ## [Unreleased] section is empty."
  exit 1
fi

if ! printf '%s\n' "$UNRELEASED_BLOCK" | grep -qE '^- '; then
  echo "ERROR: ## [Unreleased] must contain at least one bullet line starting with '- '."
  exit 1
fi

echo "Changelog gate passed."
