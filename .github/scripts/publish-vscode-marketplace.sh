#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${VSCE_TOKEN:-}" ]]; then
  echo "Missing VSCE_TOKEN."
  exit 1
fi

mapfile -t vsix_files < <(find . -maxdepth 1 -type f -name '*.vsix' -print | sort)

if [[ "${#vsix_files[@]}" -eq 0 ]]; then
  echo "No VSIX artifact found in release assets."
  exit 1
fi

if [[ "${#vsix_files[@]}" -gt 1 ]]; then
  echo "Expected exactly one VSIX artifact but found ${#vsix_files[@]}:"
  printf '%s\n' "${vsix_files[@]}"
  exit 1
fi

vsix_file="${vsix_files[0]#./}"
echo "Publishing VSIX to VS Code Marketplace: ${vsix_file}"

is_duplicate_publish_error() {
  local output="$1"
  grep -Eiq "already exists|version already exists|already published|version .* exists|conflict.*(version|publish|extension)" <<< "$output"
}

echo "VS Code Marketplace publish attempt 1/1"
set +e
output="$(vsce publish --packagePath "$vsix_file" --pat "$VSCE_TOKEN" 2>&1)"
status=$?
set -e

echo "$output"

if [[ "$status" -eq 0 ]]; then
  echo "VS Code Marketplace publish succeeded."
  exit 0
fi

if is_duplicate_publish_error "$output"; then
  echo "Extension version is already published on VS Code Marketplace; treating as success."
  exit 0
fi

echo "VS Code Marketplace publish failed."
exit 1
