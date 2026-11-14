#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "Usage: $0 <version> [changelog]"
  exit 2
fi

version="$1"
changelog="${2:-CHANGELOG.md}"

if [[ ! -f "$changelog" ]]; then
  echo "ERROR: Changelog not found: $changelog" >&2
  exit 1
fi

section_body="$(
  awk -v version="[$version]" '
    $0 == "### " version || $0 == "## " version { in_section = 1; next }
    in_section && (/^## / || /^### \[[0-9]+\.[0-9]+\.[0-9]+(-next)?\]$/) { exit }
    in_section && NF {
      if (seen) printf "%s", blank
      print
      blank = ""
      seen = 1
      next
    }
    in_section && seen { blank = blank "\n" }
  ' "$changelog"
)"

if [[ -z "${section_body//[[:space:]]/}" ]]; then
  echo "ERROR: Missing or empty changelog section '${version}'." >&2
  echo "Add a patch section for ${version} to CHANGELOG.md before tagging." >&2
  exit 1
fi

if ! grep -Eq '^[[:space:]]*-[[:space:]]+' <<<"$section_body"; then
  echo "ERROR: Section '${version}' must include at least one bullet line." >&2
  exit 1
fi

if ! grep -q '\*\*Full Changelog\*\*:' <<<"$section_body"; then
  echo "ERROR: Section '${version}' must include a '**Full Changelog**' compare link." >&2
  exit 1
fi

printf '%s\n' "$section_body"
