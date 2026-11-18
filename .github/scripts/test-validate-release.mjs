import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {test} from 'node:test';
import {validateRelease} from './validate-release.mjs';
import {parseReleaseTag, isReleaseTag} from './release-tag.mjs';

const changelog = (tag) => `# Changelog\n\n## [Unreleased]\n\n## [${parseReleaseTag(tag).line}]\n\n### [${tag}]\n\n- Released ${tag}.\n\n**Full Changelog**: https://github.com/example/repo/compare/base...${tag}\n`;
function fixture(t) {
  const root = mkdtempSync(path.join(os.tmpdir(), 'release-validation-'));
  t.after(() => rmSync(root, {recursive: true, force: true}));
  const git = (...args) => execFileSync('git', ['-C', root, ...args], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']}).trim();
  git('init', '-q', '-b', 'main');
  git('config', 'user.name', 'Release test');
  git('config', 'user.email', 'test@example.invalid');
  git('config', 'commit.gpgSign', 'false');
  git('config', 'tag.gpgSign', 'false');
  git('config', 'core.hooksPath', '/dev/null');
  const write = (file, contents) => writeFileSync(path.join(root, file), contents);
  const content = (tag, text) => {
    write('CHANGELOG.md', changelog(tag));
    write('README.md', text);
    git('add', '.'); git('commit', '-qm', text);
  };
  return {root, git, write, content};
}

test('release validation checks the tag, matching branch and tagged notes without making commits', (t) => {
  const f = fixture(t);
  f.content('v1.0.0', 'release');
  f.git('branch', 'release/v1.0'); f.git('tag', 'v1.0.0');
  f.content('v1.1.0', 'main'); f.git('tag', 'v1.0.1');
  assert.throws(() => validateRelease(f.root, 'v1.0.1'), /must belong to release\/v1.0/);
  assert.throws(() => validateRelease(f.root, 'v1.0.0-beta'), /canonical release tag/);
  f.git('tag', 'v1.1.0');
  assert.throws(() => validateRelease(f.root, 'v1.1.0'), /Missing release\/v1.1/);
  const before = f.git('status', '--porcelain');
  const result = validateRelease(f.root, 'v1.0.0');
  assert.equal(result.tag, 'v1.0.0');
  assert.equal(result.version, '1.0.0');
  assert.equal(result.commit, f.git('rev-parse', 'v1.0.0'));
  assert.match(result.notes, /Released v1.0.0/);
  assert.doesNotMatch(result.notes, /1.1.0/);
  assert.equal(f.git('status', '--porcelain'), before);
  // CI has remote tracking branches; a local preflight can precede pushing its branch.
  f.git('update-ref', 'refs/remotes/origin/release/v1.0', f.git('rev-parse', 'v1.0.0'));
  f.git('branch', '-f', 'release/v1.0', 'v1.0.1');
  f.write('CHANGELOG.md', changelog('v1.0.1'));
  f.git('add', '.'); f.git('commit', '-qm', 'Patch notes');
  f.git('tag', '-f', 'v1.0.1'); f.git('branch', '-f', 'release/v1.0', 'HEAD');
  assert.equal(validateRelease(f.root, 'v1.0.1').branch, 'refs/heads/release/v1.0');
  f.git('update-ref', 'refs/remotes/origin/release/v1.0', f.git('rev-parse', 'HEAD'));
  f.git('branch', '-D', 'release/v1.0');
  assert.equal(validateRelease(f.root, 'v1.0.1').branch, 'refs/remotes/origin/release/v1.0');
  f.git('branch', 'release/v1.1');
  f.content('v1.2.0', 'wrong-notes'); f.git('branch', 'release/v1.2'); f.git('tag', 'v1.2.1');
  assert.throws(() => validateRelease(f.root, 'v1.2.1'));
  assert.throws(() => validateRelease(f.root, '1.0.0'), /canonical release tag/);
});


test('release entry points require canonical vX.Y.Z and produce numeric package versions', () => {
  for (const tag of ['v0.0.0', 'v1.2.3', 'v123.456.789']) {
    assert.equal(parseReleaseTag(tag).version, tag.slice(1));
    assert.ok(isReleaseTag(tag));
  }
  for (const invalid of ['1.2.3', 'v1.2', 'v1.2.3.4', 'V1.2.3', 'vv1.2.3', 'v01.2.3', 'v1.02.3', 'v1.2.03',
    'v1.2.3-beta', 'v1.2.3+build', ' v1.2.3', 'v1.2.3 ', 'v1.2.3\n', '', null, 123]) {
    assert.equal(isReleaseTag(invalid), false, String(invalid));
    assert.throws(() => parseReleaseTag(invalid), /canonical release tag/);
  }
});
