#!/usr/bin/env bash
set -euo pipefail

scripts="$(cd "$(dirname "$0")" && pwd)"
fixture="$(mktemp -d)"
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/repo"

cat > "$fixture/repo/CHANGELOG.md" <<'CHANGELOG'
# Changelog


Introductory text.

## [Unreleased]


- First change.

- Second change.


## [0.1]

### [0.1.0]


- Existing change.


CHANGELOG
cat > "$fixture/expected.md" <<'CHANGELOG'
# Changelog

Introductory text.

## [Unreleased]

- First change.
- Second change.

## [0.1]

### [0.1.0]

- Existing change.
CHANGELOG
if "$scripts/format-changelog.sh" --check "$fixture/repo/CHANGELOG.md" >/dev/null 2>&1; then
  echo "ERROR: Formatting check accepted extra blank lines." >&2
  exit 1
fi
cp "$fixture/repo/CHANGELOG.md" "$fixture/input.md"
"$scripts/format-changelog.sh" "$fixture/input.md"
diff -u "$fixture/expected.md" "$fixture/input.md"
"$scripts/format-changelog.sh" --check "$fixture/input.md"

printf '# Changelog\n## [Unreleased]\n- Change.\n**Full Changelog**: example\n' > "$fixture/compact.md"
printf '# Changelog\n\n## [Unreleased]\n\n- Change.\n\n**Full Changelog**: example\n' > "$fixture/compact-expected.md"
"$scripts/format-changelog.sh" "$fixture/compact.md"
diff -u "$fixture/compact-expected.md" "$fixture/compact.md"

printf '* First item.\n\n* Second item.\n' > "$fixture/stars.md"
printf '* First item.\n* Second item.\n' > "$fixture/stars-expected.md"
"$scripts/format-changelog.sh" "$fixture/stars.md"
diff -u "$fixture/stars-expected.md" "$fixture/stars.md"

cat > "$fixture/releases.md" <<'CHANGELOG'
# Changelog

## [Unreleased]

- Future feature.

## [0.2]

### [0.2.1]

- Patch fix.

**Full Changelog**: https://github.com/example/repo/compare/0.2.0...0.2.1

### [0.2.0]

- First release.

**Full Changelog**: https://github.com/example/repo/compare/0.1.0...0.2.0
CHANGELOG
printf '%s\n\n%s\n' '- Patch fix.' '**Full Changelog**: https://github.com/example/repo/compare/0.2.0...0.2.1' > "$fixture/expected-notes.md"
"$scripts/extract-release-notes.sh" 0.2.1 "$fixture/releases.md" > "$fixture/notes.md"
diff -u "$fixture/expected-notes.md" "$fixture/notes.md"
cat "$fixture/releases.md" | "$scripts/extract-release-notes.sh" 0.2.1 - > "$fixture/notes.md"
diff -u "$fixture/expected-notes.md" "$fixture/notes.md"
sed '/^## \[0.2\]$/d; s/^### \[0.2/## [0.2/' "$fixture/releases.md" > "$fixture/legacy.md"
"$scripts/extract-release-notes.sh" 0.2.1 "$fixture/legacy.md" > "$fixture/notes.md"
diff -u "$fixture/expected-notes.md" "$fixture/notes.md"
for invalid in missing empty no-bullets no-compare; do
    case "$invalid" in
        missing) printf '# Changelog\n' > "$fixture/invalid.md" ;;
        empty) printf '### [0.2.1]\n' > "$fixture/invalid.md" ;;
        no-bullets) printf '### [0.2.1]\n\nText only.\n' > "$fixture/invalid.md" ;;
        no-compare) printf '### [0.2.1]\n\n- Change.\n' > "$fixture/invalid.md" ;;
    esac
    if "$scripts/extract-release-notes.sh" 0.2.1 "$fixture/invalid.md" >/dev/null 2>&1; then
        echo "ERROR: Accepted $invalid release notes." >&2
        exit 1
    fi
done
printf 'Changelog formatting and release-note extraction tests passed.\n'
