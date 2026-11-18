#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "Usage: $0 <version> [dryrun]"
  echo "  version: vX.Y.Z"
  echo "  dryrun : true|false (default: false)"
  exit 2
fi

tag="$1"
scripts="$(cd "$(dirname "$0")" && pwd)"
version="$(node "$scripts/release-tag.mjs" "$tag")"
release_line="${version%.*}"
dryrun="${2:-false}"

if [[ "$dryrun" != "true" && "$dryrun" != "false" ]]; then
  echo "ERROR: dryrun must be true or false." >&2
  exit 1
fi

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

branch="$(git branch --show-current)"
if [[ "$branch" != "main" && "$branch" != "release/v${release_line}" ]]; then
  echo "ERROR: Prepare ${version} on main or release/v${release_line}, not '${branch:-detached HEAD}'." >&2
  exit 1
fi

if [[ ! -f CHANGELOG.md ]]; then
  echo "ERROR: CHANGELOG.md not found."
  exit 1
fi

if [[ "$dryrun" != "true" && -n "$(git status --porcelain)" ]]; then
  echo "ERROR: Working tree is not clean. Commit or stash changes before running release."
  exit 1
fi

if git rev-parse -q --verify "refs/tags/${tag}" >/dev/null; then
  echo "ERROR: Tag '${tag}' already exists."
  exit 1
fi

if grep -Fxq "### [${tag}]" CHANGELOG.md || grep -Fxq "## [${tag}]" CHANGELOG.md ||
   grep -Fxq "### [${version}]" CHANGELOG.md || grep -Fxq "## [${version}]" CHANGELOG.md; then
  echo "ERROR: Version ${version} is already recorded; choose a new package version."
  exit 1
fi

section_header="## [Unreleased]"

unreleased_body="$(
  awk -v header="$section_header" '
    BEGIN { in_section = 0 }
    $0 == header { in_section = 1; next }
    /^## / && in_section { exit }
    in_section { print }
  ' CHANGELOG.md
)"

if [[ -z "${unreleased_body//[[:space:]]/}" ]]; then
  echo "ERROR: Unreleased section is empty."
  exit 1
fi

if ! grep -Eq '^[[:space:]]*-[[:space:]]+' <<<"$unreleased_body"; then
  echo "ERROR: Unreleased section must include at least one bullet."
  exit 1
fi

candidate_tags="$(git tag --merged HEAD --list | grep -E '^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$' | sort -Vu || true)"

previous_tag="$(
  {
    printf "%s\n" "$candidate_tags"
    printf "%s\n" "$tag"
  } \
  | sort -Vu \
  | awk -v target="$tag" '
      $0 == target { print prev; exit }
      { prev = $0 }
    '
)"

if [[ -z "$previous_tag" ]]; then
  # The first prefixed release starts a new tag convention. Never inspect or
  # accept legacy tags; compare against the repository's initial commit.
  previous_tag="$(git rev-list --max-parents=0 HEAD | tail -n 1)"
fi

remote_url="$(git remote get-url origin 2>/dev/null || true)"
if [[ -z "$remote_url" ]]; then
  echo "ERROR: Could not determine origin remote URL."
  exit 1
fi

repo_slug=""
if [[ "$remote_url" =~ github.com[:/]([^/]+/[^/.]+)(\.git)?$ ]]; then
  repo_slug="${BASH_REMATCH[1]}"
fi

if [[ -z "$repo_slug" ]]; then
  echo "ERROR: Could not parse GitHub owner/repo from origin remote '$remote_url'."
  exit 1
fi

compare_link="**Full Changelog**: https://github.com/${repo_slug}/compare/${previous_tag}...${tag}"

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

stripped_changelog="${tmp_dir}/changelog-stripped.md"
new_section_file="${tmp_dir}/new-section.md"
updated_changelog="${tmp_dir}/CHANGELOG.md"

awk '
  BEGIN { skip = 0 }
  $0 == "## [Unreleased]" {
    print
    print ""
    skip = 1
    next
  }
  skip && /^## / { skip = 0 }
  !skip { print }
' CHANGELOG.md > "$stripped_changelog"

{
  echo "### [${tag}]"
  echo ""
  printf "%s\n" "$unreleased_body"
  echo ""
  echo "$compare_link"
} > "$new_section_file"

awk -v version="$version" -v release_line="$release_line" -v section_file="$new_section_file" '
  function insert_release(new_group, line) {
    if (new_group) print "## [" release_line "]"
    while ((getline line < section_file) > 0) print line
    close(section_file)
    inserted = 1
  }
  BEGIN { split(version, target, ".") }
  !inserted && /^## / {
    if (in_group) insert_release(0)
    in_group = $0 == "## [" release_line "]"
  }
  !inserted && /^## \[[0-9]+\.[0-9]+\]$/ {
    split(substr($0, 5, length($0) - 5), current, ".")
    if (current[1]+0 < target[1]+0 ||
        (current[1]+0 == target[1]+0 && current[2]+0 < target[2]+0)) {
      insert_release(1)
    }
  }
  !inserted && in_group && /^### \[/ {
    recorded = substr($0, 6, length($0) - 6)
    sub(/^v/, "", recorded)
    split(recorded, current, ".")
    if (current[3]+0 < target[3]+0 || /-next\]$/) insert_release(0)
  }
  { print }
  END { if (!inserted) insert_release(!in_group) }
' "$stripped_changelog" > "$updated_changelog"

# Normalize both the extracted body and existing history before publication.
"${repo_root}/.github/scripts/format-changelog.sh" "$updated_changelog"

"$repo_root/.github/scripts/extract-release-notes.sh" "$tag" "$updated_changelog" >/dev/null

if [[ "$dryrun" == "true" ]]; then
  echo "[DRY RUN] Would update CHANGELOG.md, commit and create annotated tag '${tag}'."
  echo "[DRY RUN] Previous tag: ${previous_tag}"
  echo "[DRY RUN] Compare link: ${compare_link}"
  cat "$updated_changelog"
  exit 0
fi

cp "$updated_changelog" CHANGELOG.md

# The website assembles its manuals dynamically from main and published tags.
# Release preparation owns only the changelog, its commit, and the tag.
git add CHANGELOG.md
git commit -m "chore(release): ${tag}"
git tag -a "${tag}" -m "Release ${tag}"

echo "Release prepared successfully."
echo "Next steps:"
if [[ "$branch" == "main" ]]; then
  echo "  Create release/v${release_line} at this commit before pushing the tag."
fi
echo "  make release-check version=${tag}"
echo "  git push origin release/v${release_line} --follow-tags"
