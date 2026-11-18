import {execFileSync} from 'node:child_process';
import {cpSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import {stripTypeScriptTypes} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {isReleaseTag, parseReleaseTag} from '../../.github/scripts/release-tag.mjs';


export const releaseLine = (tag) => tag.slice(0, tag.lastIndexOf('.'));
export const compareVersions = (a, b) => a.localeCompare(b, 'en', {numeric: true});

// The caller supplies published GitHub releases, never an unfiltered tag list.
export function selectReleases(releases) {
  return [...releases].filter((r) => !r.draft && !r.prerelease && r.published_at && isReleaseTag(r.tag_name))
    .sort((a, b) => compareVersions(b.tag_name, a.tag_name)).slice(0, 1).map((r) => r.tag_name);
}

export function changelogSection(changelog, heading) {
  const lines = changelog.replaceAll('\r\n', '\n').split('\n');
  const start = lines.indexOf(heading);
  if (start < 0) throw new Error(`Missing changelog heading: ${heading}`);
  const level = heading.match(/^#+/)[0].length;
  const end = lines.findIndex((line, index) => index > start && new RegExp(`^#{1,${level}} `).test(line));
  return lines.slice(start + 1, end < 0 ? undefined : end).join('\n').trim();
}

export function whatsNew(changelog, tag) {
  let body;
  if (tag) {
    parseReleaseTag(tag);
    const entries = [...changelog.replaceAll('\r\n', '\n').matchAll(/^### \[(v[0-9]+\.[0-9]+\.[0-9]+)\]\n([\s\S]*?)(?=^#{2,3} |$(?![\s\S]))/gm)]
      .filter((entry) => releaseLine(entry[1]) === releaseLine(tag) && compareVersions(entry[1], tag) <= 0);
    const tagged = entries.find((entry) => entry[1] === tag);
    if (!tagged || !/^\s*-\s+/m.test(tagged[2])) throw new Error(`Missing release notes for ${tag}`);
    if (!tagged[2].includes('**Full Changelog**:')) throw new Error(`Missing compare link for ${tag}`);
    const normalized = changelog.replaceAll('\r\n', '\n');
    const released = normalized.search(/^## \[(?:[0-9]+\.[0-9]+|v[0-9]+\.[0-9]+\.[0-9]+)\]$/m);
    if (released < 0) throw new Error('Missing released changelog history.');
    body = normalized.slice(released).trim()
      .replace(/^## \[([0-9]+\.[0-9]+)\]$/gm, '## $1')
      .replace(/^#{2,3} \[(v?[0-9]+\.[0-9]+\.[0-9]+)\]$/gm, (_, version) => `### v${version.replace(/^v/, '')}`);
  } else {
    body = changelogSection(changelog, '## [Unreleased]') || 'No unreleased changes.';
  }
  return `---\nid: whats-new\ntitle: What's New\nslug: /whats-new\nmdx:\n  format: md\n---\n\n${tag ? 'Released versions' : '## Unreleased'}\n\n${body}\n`;
}

function git(root, args, options = {}) {
  return execFileSync('git', ['-C', root, ...args], {encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], ...options});
}

export function resolveCommit(root, ref) {
  return git(root, ['rev-parse', '--verify', `${ref}^{commit}`]).trim();
}

function exportDocs(root, sha, destination) {
  mkdirSync(destination, {recursive: true});
  const archive = git(root, ['archive', '--format=tar', sha, 'website/docs', 'website/sidebars.ts', 'CHANGELOG.md'],
    {encoding: 'buffer', maxBuffer: 32 * 1024 * 1024});
  execFileSync('tar', ['-xf', '-', '-C', destination], {input: archive});
}

async function sidebar(source) {
  // Sidebars must be self-contained data modules. Type-only imports are erased.
  const js = stripTypeScriptTypes(readFileSync(source, 'utf8'));
  return (await import(`data:text/javascript;base64,${Buffer.from(js).toString('base64')}`)).default;
}

export async function assembleDocs({root, releases, nextRef} = {}) {
  const website = path.join(root, 'website');
  const tags = releases === undefined ? [] : selectReleases(releases);
  if (releases !== undefined && tags.length === 0) throw new Error('No published stable releases found.');
  const temporary = mkdtempSync(path.join(os.tmpdir(), 'fscript-docs-'));
  const manifest = {next: {ref: nextRef ?? 'working-tree', commit: null}, stable: []};
  try {
    let nextRoot = root;
    if (nextRef) {
      manifest.next.commit = resolveCommit(root, nextRef);
      nextRoot = path.join(temporary, 'next');
      exportDocs(root, manifest.next.commit, nextRoot);
    }
    // Validate all source inputs before replacing generated output.
    const prepared = path.join(temporary, 'prepared');
    mkdirSync(prepared);
    cpSync(path.join(nextRoot, 'website/docs'), path.join(prepared, '.generated/next'), {recursive: true});
    writeFileSync(path.join(prepared, '.generated/next/whats-new.md'), whatsNew(readFileSync(path.join(nextRoot, 'CHANGELOG.md'), 'utf8')));
    writeFileSync(path.join(prepared, '.generated/sidebars.json'), JSON.stringify(await sidebar(path.join(nextRoot, 'website/sidebars.ts'))));
    for (const tag of tags) {
      const commit = resolveCommit(root, `refs/tags/${tag}`);
      const source = path.join(temporary, tag);
      exportDocs(root, commit, source);
      const line = 'stable';
      const docs = path.join(prepared, `versioned_docs/version-${line}`);
      cpSync(path.join(source, 'website/docs'), docs, {recursive: true});
      writeFileSync(path.join(docs, 'whats-new.md'), whatsNew(readFileSync(path.join(source, 'CHANGELOG.md'), 'utf8'), tag));
      mkdirSync(path.join(prepared, 'versioned_sidebars'), {recursive: true});
      writeFileSync(path.join(prepared, `versioned_sidebars/version-${line}-sidebars.json`), JSON.stringify(await sidebar(path.join(source, 'website/sidebars.ts'))));
      manifest.stable.push({version: line, tag, commit});
    }
    mkdirSync(path.join(prepared, '.generated/static'), {recursive: true});
    writeFileSync(path.join(prepared, '.generated/static/website-sources.json'), `${JSON.stringify(manifest, null, 2)}\n`);
    writeFileSync(path.join(prepared, 'versions.json'), `${JSON.stringify(manifest.stable.map((r) => r.version), null, 2)}\n`);
    for (const name of ['.generated', 'versioned_docs', 'versioned_sidebars', 'versions.json']) {
      rmSync(path.join(website, name), {recursive: true, force: true});
      if (existsSync(path.join(prepared, name))) cpSync(path.join(prepared, name), path.join(website, name), {recursive: true});
    }
    return manifest;
  } finally {
    rmSync(temporary, {recursive: true, force: true});
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
  const releasesFile = process.env.FSCRIPT_WEBSITE_RELEASES;
  const manifest = await assembleDocs({root, releases: releasesFile ? JSON.parse(readFileSync(releasesFile, 'utf8')) : undefined,
    nextRef: process.env.FSCRIPT_WEBSITE_NEXT_REF});
  console.log(`Prepared Next and ${manifest.stable.length} stable manuals. ${releasesFile ? '' : 'Local preview only; supply FSCRIPT_WEBSITE_RELEASES for stable manuals.'}`);
}
