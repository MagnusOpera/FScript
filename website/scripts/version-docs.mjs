import {existsSync, readFileSync, writeFileSync} from 'node:fs';
import path from 'node:path';
import {spawnSync} from 'node:child_process';

const version = process.argv[2];
if (!version || !/^\d+\.\d+\.\d+$/.test(version)) {
  console.error('Usage: npm run version-docs -- X.Y.Z');
  process.exit(2);
}

const root = process.cwd();
const versionsPath = path.join(root, 'versions.json');

function extractSection(markdown, header) {
  const start = markdown.indexOf(header);

  if (start === -1) {
    return null;
  }

  const afterHeader = markdown.slice(start + header.length);
  const nextHeaderOffset = afterHeader.search(/\n## \[/);

  return (nextHeaderOffset === -1 ? afterHeader : afterHeader.slice(0, nextHeaderOffset)).trim();
}

if (existsSync(versionsPath)) {
  const versions = JSON.parse(readFileSync(versionsPath, 'utf8'));
  if (Array.isArray(versions) && versions.includes(version)) {
    console.log(`Docs version ${version} already exists; skipping.`);
    process.exit(0);
  }
}

const bin = process.platform === 'win32'
  ? path.join(root, 'node_modules', '.bin', 'docusaurus.cmd')
  : path.join(root, 'node_modules', '.bin', 'docusaurus');

const result = spawnSync(bin, ['docs:version', version], {
  stdio: 'inherit',
});

if (result.status !== 0) {
  process.exit(result.status ?? 1);
}

const changelogPath = path.resolve(root, '..', 'CHANGELOG.md');
const changelog = readFileSync(changelogPath, 'utf8');
const releasedSectionBody = extractSection(changelog, `## [${version}]`);

if (releasedSectionBody === null) {
  console.error(`Missing changelog section for ${version}.`);
  process.exit(1);
}

const unreleasedSectionBody = extractSection(changelog, '## [Unreleased]');

if (unreleasedSectionBody === null) {
  console.error('Missing changelog section for Unreleased.');
  process.exit(1);
}

const whatsNewPath = path.join(root, 'versioned_docs', `version-${version}`, 'whats-new.md');
const whatsNewContent = `---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## ${version}

${releasedSectionBody}
`;

writeFileSync(whatsNewPath, whatsNewContent);

const currentWhatsNewPath = path.join(root, 'docs', 'whats-new.md');
const currentWhatsNewContent = `---
id: whats-new
title: What's New
slug: /whats-new
---

For the complete history, see the full [CHANGELOG.md](https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md) on GitHub.

## Unreleased

${unreleasedSectionBody}
`;

writeFileSync(currentWhatsNewPath, currentWhatsNewContent);
