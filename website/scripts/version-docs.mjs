import {existsSync, mkdtempSync, readFileSync, renameSync, rmSync, writeFileSync} from 'node:fs';
import path from 'node:path';
import {spawnSync} from 'node:child_process';

const version = process.argv[2];
if (!version || !/^\d+\.\d+\.\d+$/.test(version)) {
  console.error('Usage: npm run version-docs -- X.Y.Z');
  process.exit(2);
}

const releaseLine = version.slice(0, version.lastIndexOf('.'));
const retention = 5;
const versionsPath = 'versions.json';
const originalVersions = readFileSync(versionsPath, 'utf8');
const versions = JSON.parse(originalVersions);
const retained = [...new Set([...versions, releaseLine])]
  .sort((left, right) => right.localeCompare(left, 'en', {numeric: true}))
  .slice(0, retention);

if (!retained.includes(releaseLine)) {
  console.log(`Docs for ${releaseLine} are outside the five retained release lines.`);
  process.exit(0);
}

const changelog = readFileSync('../CHANGELOG.md', 'utf8');
const releaseNotes = changelog.split(/^## /m)
  .find((section) => section.startsWith(`[${releaseLine}]\n`))
  ?.split('\n').slice(1).join('\n').trim();

const patches = [...(releaseNotes ?? '').matchAll(/^### \[([\d.]+)\]$/gm)].map((match) => match[1]);
if (!patches.includes(version)) {
  throw new Error(`Missing ### [${version}] under ## [${releaseLine}] in CHANGELOG.md.`);
}

if (patches[0] !== version) {
  console.log(`Docs for ${releaseLine} track its latest patch, ${patches[0]}.`);
  process.exit(0);
}

const whatsNewContent = `---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## ${releaseLine}

${releaseNotes.replace(/^### \[([\d.]+)\]$/gm, '### $1')}
`;

const docsPath = (name) => path.join('versioned_docs', `version-${name}`);
const sidebarPath = (name) => path.join('versioned_sidebars', `version-${name}-sidebars.json`);

function removeSnapshot(name) {
  rmSync(docsPath(name), {recursive: true, force: true});
  rmSync(sidebarPath(name), {force: true});
}

// Generate the replacement under the full version so Docusaurus can snapshot
// the current docs while the previous snapshot for this release line exists.
const bin = path.resolve('node_modules', '.bin', process.platform === 'win32' ? 'docusaurus.cmd' : 'docusaurus');
const backup = mkdtempSync('.docs-snapshot-');
const moved = [];
const installed = [];
try {
  const result = spawnSync(bin, ['docs:version', version], {stdio: 'inherit'});
  if (result.error || result.status !== 0) {
    throw new Error(`Could not snapshot docs for ${version}.`, {cause: result.error});
  }
  writeFileSync(path.join(docsPath(version), 'whats-new.md'), whatsNewContent);

  const replaced = [releaseLine, ...versions.filter((name) => !retained.includes(name))];
  for (const name of replaced) {
    for (const original of [docsPath(name), sidebarPath(name)]) {
      if (!existsSync(original)) continue;
      const saved = path.join(backup, path.basename(original));
      renameSync(original, saved);
      moved.push({original, saved});
    }
  }

  for (const location of [docsPath, sidebarPath]) {
    const destination = location(releaseLine);
    renameSync(location(version), destination);
    installed.push(destination);
  }

  writeFileSync(versionsPath, `${JSON.stringify(retained, null, 2)}\n`);
} catch (error) {
  for (const destination of installed) {
    rmSync(destination, {recursive: true, force: true});
  }
  for (const {original, saved} of moved) {
    renameSync(saved, original);
  }
  removeSnapshot(version);
  writeFileSync(versionsPath, originalVersions);
  rmSync(backup, {recursive: true});
  throw error;
}

rmSync(backup, {recursive: true});

console.log(`Updated docs for ${releaseLine} from ${version}.`);
