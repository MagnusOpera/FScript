import assert from 'node:assert/strict';
import {readFileSync, readdirSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const versions = JSON.parse(readFileSync(path.join(root, 'versions.json'), 'utf8'));
assert.ok(versions.length > 0 && versions.length <= 5, 'Expected up to five stable manuals');
assert.ok(versions.every((version) => /^\d+\.\d+$/.test(version)), 'Expected major.minor docs versions');
assert.equal(new Set(versions).size, versions.length, 'Expected unique release lines');
assert.deepEqual(versions, [...versions].sort((left, right) => right.localeCompare(left, 'en', {numeric: true})));
assert.deepEqual(
  readdirSync(path.join(root, 'versioned_docs')).sort(),
  versions.map((version) => `version-${version}`).sort(),
);
assert.deepEqual(
  readdirSync(path.join(root, 'versioned_sidebars')).sort(),
  versions.map((version) => `version-${version}-sidebars.json`).sort(),
);

const manual = path.join(root, 'build', 'manual');
const maintainedHtml = readFileSync(path.join(manual, 'index.html'), 'utf8');
assert.ok(!maintainedHtml.includes('no longer actively maintained'), 'The default manual is marked unmaintained');
assert.ok(!maintainedHtml.includes('This is unreleased documentation'), 'The default manual is marked unreleased');

const nextHtml = readFileSync(path.join(manual, 'next', 'index.html'), 'utf8');
assert.ok(nextHtml.includes('This is unreleased documentation'), 'Next is missing the unreleased banner');

const changelog = readFileSync(path.join(root, '..', 'CHANGELOG.md'), 'utf8');
const sections = changelog.split(/^## /m);
for (const [index, version] of versions.entries()) {
  const releaseNotes = sections.find((section) => section.startsWith(`[${version}]\n`));
  assert.ok(releaseNotes, `Missing changelog section for ${version}`);
  const html = readFileSync(path.join(manual, index === 0 ? '' : version, 'whats-new', 'index.html'), 'utf8');
  const patches = [...releaseNotes.matchAll(/^### \[([\d.]+)\]$/gm)].map((match) => match[1]);
  assert.ok(patches.length > 0, `Missing patch notes for ${version}`);
  const headings = [...html.matchAll(/<h3 [^>]*>([\d.]+)</g)].map((match) => match[1]);
  assert.deepEqual(headings, patches, `Unexpected What's New headings for ${version}`);
}

console.log(`Verified Next and the ${versions.length} stable manuals with their patch notes.`);
