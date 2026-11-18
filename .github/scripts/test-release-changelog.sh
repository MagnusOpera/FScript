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

# Release preparation runs locally on main and maintenance branches. Website
# dependencies and generated snapshots must never participate in preparation.
repo="$fixture/prepare"
mkdir -p "$repo/.github/scripts" "$repo/website" "$fixture/bin"
cp "$scripts/release.sh" "$scripts/format-changelog.sh" "$scripts/extract-release-notes.sh" "$repo/.github/scripts/"
cat > "$fixture/bin/npm" <<'STUB'
#!/usr/bin/env bash
echo 'Release preparation must not invoke npm' >&2
exit 99
STUB
chmod +x "$fixture/bin/npm"
export PATH="$fixture/bin:$PATH"
printf 'Keep website sources unchanged.\n' > "$repo/website/content.md"
cp "$fixture/releases.md" "$repo/CHANGELOG.md"
cd "$repo"
git init -qb main
git config user.name 'Release test'
git config user.email 'release@example.invalid'
git config commit.gpgSign false
git config tag.gpgSign false
git config core.hooksPath /dev/null
git remote add origin git@github.com:example/repo.git
git add .
git commit -qm fixture
git tag 0.2.1
head_before="$(git rev-parse HEAD)"
.github/scripts/release.sh 0.3.0 true > "$fixture/preview.log"
grep -Fq '### [0.3.0]' "$fixture/preview.log"
[[ "$(git rev-parse HEAD)" == "$head_before" ]]
[[ -z "$(git status --porcelain)" ]]
[[ "$(git tag --list | wc -l | tr -d ' ')" == 1 ]]
.github/scripts/release.sh 0.3.0 > "$fixture/prepare-main.log"
[[ "$(git cat-file -t 0.3.0)" == tag ]]
[[ "$(git log -1 --pretty=%s)" == 'chore(release): 0.3.0' ]]
[[ "$(git diff-tree --no-commit-id --name-only -r HEAD)" == CHANGELOG.md ]]
[[ -z "$(git status --porcelain)" ]]
grep -Fq 'https://github.com/example/repo/compare/0.2.1...0.3.0' CHANGELOG.md
.github/scripts/extract-release-notes.sh 0.3.0 > "$fixture/main-notes.md"
grep -Fq -- '- Future feature.' "$fixture/main-notes.md"
[[ -z "$(awk '/^## \[Unreleased\]/{p=1;next} p && /^## /{exit} p && /^- /{print}' CHANGELOG.md)" ]]
git switch -qc release/v0.3
# A maintenance fix uses the same Unreleased -> version transition.
python3 - <<'PYTHON'
from pathlib import Path
p = Path('CHANGELOG.md')
p.write_text(p.read_text().replace('## [Unreleased]\n', '## [Unreleased]\n\n- Maintenance fix.\n', 1))
PYTHON
git add CHANGELOG.md
git commit -qm 'Fix a regression'
.github/scripts/release.sh 0.3.1 > "$fixture/prepare-patch.log"
[[ "$(git diff-tree --no-commit-id --name-only -r HEAD)" == CHANGELOG.md ]]
grep -Fq 'https://github.com/example/repo/compare/0.3.0...0.3.1' CHANGELOG.md
[[ "$(grep -Fc '## [0.3]' CHANGELOG.md)" == 1 ]]
.github/scripts/extract-release-notes.sh 0.3.1 > "$fixture/patch-notes.md"
grep -Fq -- '- Maintenance fix.' "$fixture/patch-notes.md"
if .github/scripts/release.sh 0.3.1 true >/dev/null 2>&1; then
  echo 'ERROR: Accepted an existing tag.' >&2; exit 1
fi
if .github/scripts/release.sh 0.4.0 true >/dev/null 2>&1; then
  echo 'ERROR: Accepted a mismatched release branch.' >&2; exit 1
fi
printf 'Uncommitted change.\n' >> website/content.md
if .github/scripts/release.sh 0.3.2 >/dev/null 2>&1; then
  echo 'ERROR: Accepted a dirty checkout.' >&2; exit 1
fi
printf 'Local release preparation tests passed.\n'
