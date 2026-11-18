import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {test} from 'node:test';
import {assembleDocs, selectReleases, whatsNew} from './assemble-docs.mjs';
import {parseReleaseTag, isReleaseTag} from '../../.github/scripts/release-tag.mjs';
import {prepareRuntime} from './prepare-runtime.mjs';
import {watchDocumentation} from './start.mjs';
import {validateRelease} from '../../.github/scripts/validate-release.mjs';

const release = (tag, extra = {}) => ({tag_name: tag, published_at: '2026-01-01', draft: false, prerelease: false, ...extra});
const changelog = (tag, next = 'Upcoming feature.') => `# Changelog\n\n## [Unreleased]\n\n- ${next}\n\n## [${parseReleaseTag(tag).line}]\n\n### [${tag}]\n\n- Released ${tag}.\n\n**Full Changelog**: https://github.com/example/repo/compare/base...${tag}\n`;

function fixture(t) {
  const root = mkdtempSync(path.join(os.tmpdir(), 'website-test-'));
  t.after(() => rmSync(root, {recursive: true, force: true}));
  const git = (...args) => execFileSync('git', ['-C', root, ...args], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']}).trim();
  git('init', '-q', '-b', 'main');
  git('config', 'user.name', 'Website test');
  git('config', 'user.email', 'test@example.invalid');
  git('config', 'commit.gpgSign', 'false');
  git('config', 'tag.gpgSign', 'false');
  git('config', 'core.hooksPath', '/dev/null');
  const write = (file, contents) => {mkdirSync(path.dirname(path.join(root, file)), {recursive: true}); writeFileSync(path.join(root, file), contents);};
  write('.gitignore', 'website/.generated\nwebsite/versioned_docs\nwebsite/versioned_sidebars\nwebsite/versions.json\n');
  const content = (tag, text) => {
    write('CHANGELOG.md', changelog(tag));
    write('website/docs/intro.md', text);
    write('website/docs/whats-new.md', 'This placeholder must be generated.');
    write('website/sidebars.ts', `import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';\nconst sidebars: SidebarsConfig = {docs: ['${text}', 'whats-new']};\nexport default sidebars;`);
    git('add', '.'); git('commit', '-qm', text);
  };
  return {root, git, write, content, read: (file) => readFileSync(path.join(root, 'website', file), 'utf8')};
}

test('published releases select only the latest stable tag, independently of publication order', () => {
  const entries = ['v1.0.0', 'v1.1.0', 'v1.2.0', 'v1.3.0', 'v1.4.0', 'v1.5.0', 'v1.5.2', 'v1.5.10', 'v1.5.1'].map((v) => release(v));
  entries.push(release('v9.0.0', {draft: true}), release('v8.0.0', {prerelease: true}), release('v7.0.0', {published_at: null}), release('v6.0.0-beta.1'), release('../invalid'), release('99.0.0'), release('v01.0.0'), release('v1.0.0\n'));
  assert.deepEqual(selectReleases(entries), ['v1.5.10']);
});

test('Next includes only Unreleased; stable requires prefixed release headings', () => {
  const source = changelog('v1.2.3');
  assert.match(whatsNew(source), /Upcoming feature/);
  assert.doesNotMatch(whatsNew(source), /Released v1.2.3|Full Changelog/);
  assert.match(whatsNew(source, 'v1.2.3'), /### v1.2.3/);
  const history = source + '\n## [1.1]\n\n### [1.1.0]\n\n- Older release.\n';
  assert.match(whatsNew(history, 'v1.2.3'), /Older release/);
  const plainMarkdown = whatsNew(history + '- Map patterns { [k] = v; ..tail }.\n', 'v1.2.3');
  assert.match(plainMarkdown, /mdx:\n  format: md/);
  assert.match(plainMarkdown, /\{ \[k\] = v; \.\.tail \}/);
  assert.doesNotMatch(whatsNew(source, 'v1.2.3'), /Upcoming feature|Unreleased/);
  assert.throws(() => whatsNew(source.replace('### [v1.2.3]', '### [1.2.3]'), 'v1.2.3'), /Missing release notes/);
  assert.match(whatsNew(source.replace('- Upcoming feature.', '')), /No unreleased changes/);
  assert.throws(() => whatsNew(source, 'v1.2.4'), /Missing release notes/);
  assert.throws(() => whatsNew(source.replace('**Full Changelog**:', 'Missing:'), 'v1.2.3'), /compare link/);
});

test('assembly isolates tagged docs, sidebars and changelogs from Next and never changes Git state', async (t) => {
  const f = fixture(t);
  f.content('v1.0.0', 'old'); f.git('tag', 'v1.0.0');
  f.content('v1.1.0', 'stable'); f.git('tag', 'v1.1.0');
  f.content('v1.2.0', 'next');
  f.write('website/docs/stdlib/process.md', 'Process.exec preview documentation');
  f.write('website/docs/stdlib/fscript.md', 'FScript.exec preview documentation');
  f.git('add', '.'); f.git('commit', '-qm', 'Preview extension functions');
  const before = f.git('status', '--porcelain');
  const head = f.git('rev-parse', 'HEAD');
  const tags = f.git('tag');
  const mainChangelog = readFileSync(path.join(f.root, 'CHANGELOG.md'), 'utf8');
  const manifest = await assembleDocs({root: f.root, releases: [release('v1.0.0'), release('v1.1.0')], nextRef: 'main'});
  assert.equal(manifest.next.commit, head);
  assert.equal(f.read('.generated/next/intro.md'), 'next');
  for (const name of ['process', 'fscript']) {
    assert.ok(existsSync(path.join(f.root, `website/.generated/next/stdlib/${name}.md`)));
    assert.ok(!existsSync(path.join(f.root, `website/versioned_docs/version-stable/stdlib/${name}.md`)));
    assert.ok(existsSync(path.join(f.root, `website/docs/stdlib/${name}.md`)));
  }
  assert.match(f.read('.generated/next/whats-new.md'), /Upcoming feature/);
  assert.doesNotMatch(f.read('.generated/next/whats-new.md'), /Released v1.2.0/);
  assert.equal(f.read('versioned_docs/version-stable/intro.md'), 'stable');
  assert.match(f.read('versioned_docs/version-stable/whats-new.md'), /Released v1.1.0/);
  assert.doesNotMatch(f.read('versioned_docs/version-stable/whats-new.md'), /Upcoming feature|v1.2.0/);
  assert.match(f.read('versioned_sidebars/version-stable-sidebars.json'), /stable/);
  assert.doesNotMatch(f.read('versioned_sidebars/version-stable-sidebars.json'), /next/);
  assert.deepEqual(JSON.parse(f.read('versions.json')), ['stable']);
  assert.equal(readFileSync(path.join(f.root, 'CHANGELOG.md'), 'utf8'), mainChangelog);
  assert.equal(f.read('docs/intro.md'), 'next');
  assert.equal(f.read('docs/whats-new.md'), 'This placeholder must be generated.');
  assert.equal(f.git('rev-parse', 'HEAD'), head);
  assert.equal(f.git('tag'), tags);
  assert.equal(f.git('status', '--porcelain'), before);
  // A failed source lookup must not leave a half-replaced manual.
  await assert.rejects(assembleDocs({root: f.root, releases: [release('v3.0.0')], nextRef: 'main'}));
  assert.equal(f.read('versioned_docs/version-stable/intro.md'), 'stable');
  await assert.rejects(assembleDocs({root: f.root, releases: []}), /No published stable/);
  // Rebuilding replaces the single stable manual, without retaining old manuals.
  await assembleDocs({root: f.root, releases: [release('v1.1.0')], nextRef: 'main'});
  assert.ok(!existsSync(path.join(f.root, 'website/versioned_docs/version-1.0')));
  assert.equal(f.git('status', '--porcelain'), before);
});

test('offline previews need no Git metadata and reflect uncommitted documentation edits', async (t) => {
  const f = fixture(t);
  f.content('v1.0.0', 'preview');
  rmSync(path.join(f.root, '.git'), {recursive: true});
  f.write('website/docs/intro.md', 'edited');
  const manifest = await assembleDocs({root: f.root});
  assert.deepEqual(manifest.stable, []);
  assert.equal(manifest.next.ref, 'working-tree');
  assert.equal(f.read('.generated/next/intro.md'), 'edited');
});

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


test('development watches canonical pages and refreshes Next without editing those sources', async (t) => {
  const f = fixture(t);
  f.content('v1.0.0', 'original');
  await assembleDocs({root: f.root});
  let refreshed;
  const update = new Promise((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error('Documentation watch did not refresh')), 5000);
    refreshed = async () => {
      await assembleDocs({root: f.root});
      clearTimeout(timeout);
      resolve();
    };
  });
  // Drive native notifications explicitly: fs.watch has no readiness event,
  // and macOS can lose a write made immediately after subscribing.
  const notifications = [];
  let closed = 0;
  const stop = watchDocumentation(f.root, refreshed, (directory, options, listener) => {
    notifications.push(typeof options === 'function' ? options : listener);
    return {close: () => {closed += 1;}};
  });
  t.after(stop);
  f.write('website/docs/intro.md', 'live edit');
  notifications[0]('change', 'intro.md');
  await update;
  assert.equal(f.read('.generated/next/intro.md'), 'live edit');
  assert.equal(f.read('docs/intro.md'), 'live edit');
  assert.equal(closed, 0);
});


test('release website builds Next interpreter from the same main snapshot as its docs', async (t) => {
  const f = fixture(t);
  f.write('.gitignore', readFileSync(path.join(f.root, '.gitignore'), 'utf8') + 'website/src/generated\n');
  f.write('src/FScript.JavaScript/Library.fs', 'stable runtime');
  f.content('v1.0.0', 'stable docs'); f.git('tag', 'v1.0.0');
  f.write('src/FScript.JavaScript/Library.fs', 'next runtime');
  f.content('v1.1.0', 'next docs');
  const nextCommit = f.git('rev-parse', 'main');
  f.git('switch', '--detach', 'v1.0.0');
  const before = f.git('status', '--porcelain');
  const manifest = await assembleDocs({root: f.root, releases: [release('v1.0.0')], nextRef: 'main'});
  assert.equal(manifest.next.commit, nextCommit);
  assert.equal(f.read('.generated/next/intro.md'), 'next docs');
  assert.equal(f.read('versioned_docs/version-stable/intro.md'), 'stable docs');
  // Even if main moves after assembly, compile the recorded documentation revision.
  f.git('branch', '-f', 'main', 'v1.0.0');
  const calls = [];
  prepareRuntime({root: f.root, ref: 'main', run: (command, args, options) => {
    calls.push({command, args, cwd: options.cwd});
    assert.equal(readFileSync(path.join(options.cwd, 'src/FScript.JavaScript/Library.fs'), 'utf8'), 'next runtime');
    if (args[0] === 'fable') assert.equal(args[3], path.join(f.root, 'website/src/generated/fscript'));
  }});
  assert.deepEqual(calls.map((call) => call.args[0]), ['tool', 'fable']);
  assert.equal(f.git('status', '--porcelain'), before);
  assert.ok(!existsSync(calls[0].cwd), 'Temporary interpreter source must be removed');
});

test('local interpreter preparation works without Git and propagates compiler failures', (t) => {
  const f = fixture(t);
  f.content('v1.0.0', 'local');
  rmSync(path.join(f.root, '.git'), {recursive: true});
  const calls = [];
  prepareRuntime({root: f.root, run: (command, args, options) => calls.push(options.cwd)});
  assert.deepEqual(calls, [f.root, f.root]);
  assert.throws(() => prepareRuntime({root: f.root, run: () => {throw new Error('Compiler failed');}}), /Compiler failed/);
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
