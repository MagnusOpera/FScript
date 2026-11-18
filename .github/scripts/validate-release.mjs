import {execFileSync} from 'node:child_process';
import path from 'node:path';
import {parseReleaseTag} from './release-tag.mjs';
import {fileURLToPath} from 'node:url';

export function validateRelease(root, tag) {
  const {version, line} = parseReleaseTag(tag);
  const git = (args) => execFileSync('git', ['-C', root, ...args], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']}).trim();
  const sha = git(['rev-parse', '--verify', `refs/tags/${tag}^{commit}`]);
  const candidates = [`refs/heads/release/v${line}`, `refs/remotes/origin/release/v${line}`];
  const branch = candidates.find((ref) => {
    try { git(['rev-parse', '--verify', ref]); return true; } catch { return false; }
  });
  if (!branch) throw new Error(`Missing release/v${line} branch.`);
  try { git(['merge-base', '--is-ancestor', sha, branch]); }
  catch { throw new Error(`Tag ${tag} must belong to release/v${line}.`); }
  const changelog = git(['show', `${sha}:CHANGELOG.md`]);
  // Validate the tagged content, even when the caller's working tree differs.
  const extractor = fileURLToPath(new URL('./extract-release-notes.sh', import.meta.url));
  const notes = execFileSync('bash', [extractor, tag, '-'], {input: changelog, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe']});
  return {tag, version, commit: sha, branch, notes};
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  console.log(JSON.stringify(validateRelease(process.cwd(), process.argv[2]), null, 2));
}
