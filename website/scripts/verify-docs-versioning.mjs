import assert from 'node:assert/strict';
import {readFileSync, readdirSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const versions = JSON.parse(readFileSync(path.join(root, 'versions.json'), 'utf8'));
const sources = JSON.parse(readFileSync(path.join(root, '.generated/static/website-sources.json'), 'utf8'));
assert.ok(versions.length <= 1, 'Expected only one stable manual');
assert.deepEqual(versions, sources.stable.map((release) => release.version));
assert.equal(new Set(versions).size, versions.length);
assert.deepEqual(versions, [...versions].sort((a, b) => b.localeCompare(a, 'en', {numeric: true})));
const manual = path.join(root, 'build/manual');
const next = versions.length > 0 ? 'next' : '';
const nextHtml = readFileSync(path.join(manual, next, 'index.html'), 'utf8');
assert.ok(nextHtml.includes('This is unreleased documentation'), 'Next is missing its unreleased banner');
const nextNotes = readFileSync(path.join(manual, next, 'whats-new/index.html'), 'utf8');
assert.ok(nextNotes.includes('Unreleased'), 'Next is missing Unreleased notes');
assert.ok(!/<h3 [^>]*>[\d.]+</.test(nextNotes), 'Released patch headings leaked into Next');
if (versions.length > 0) {
  const stableHtml = readFileSync(path.join(manual, 'index.html'), 'utf8');
  assert.ok(!stableHtml.includes('no longer actively maintained'));
  assert.ok(!stableHtml.includes('This is unreleased documentation'));
  assert.deepEqual(readdirSync(path.join(root, 'versioned_docs')).sort(), versions.map((v) => `version-${v}`).sort());
  assert.deepEqual(readdirSync(path.join(root, 'versioned_sidebars')).sort(), versions.map((v) => `version-${v}-sidebars.json`).sort());
}
for (const [index, release] of sources.stable.entries()) {
  const markdown = readFileSync(path.join(root, `versioned_docs/version-${release.version}/whats-new.md`), 'utf8');
  assert.ok(!markdown.includes('## Unreleased'));
  const patches = [...markdown.matchAll(/^### ([\d.]+)$/gm)].map((match) => match[1]);
  assert.ok(patches.includes(release.tag));
  const html = readFileSync(path.join(manual, index === 0 ? '' : release.version, 'whats-new/index.html'), 'utf8');
  assert.deepEqual([...html.matchAll(/<h3 [^>]*>([\d.]+)</g)].map((match) => match[1]), patches);
}
assert.deepEqual(JSON.parse(readFileSync(path.join(root, 'build/website-sources.json'), 'utf8')), sources);
console.log(`Verified Next and ${versions.length} stable manuals against their recorded sources.`);
