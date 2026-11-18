import {existsSync, realpathSync} from 'node:fs';
import {fileURLToPath} from 'node:url';

const pattern = /^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$/;
export function isReleaseTag(tag) {
  return typeof tag === 'string' && pattern.exec(tag)?.[0] === tag;
}
export function parseReleaseTag(tag) {
  if (!isReleaseTag(tag)) throw new Error('Expected a canonical release tag vX.Y.Z, without leading zeroes, suffixes, or whitespace.');
  const version = tag.slice(1);
  const line = version.slice(0, version.lastIndexOf('.'));
  return {tag, version, line, branch: `release/v${line}`};
}

if (process.argv[1] && existsSync(process.argv[1]) && realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    const release = parseReleaseTag(process.argv[2]);
    if (process.argv.length === 3) console.log(release.version);
    else if (process.argv.length === 4 && process.argv[3] === '--github-output') {
      console.log(`tag=${release.tag}\nversion=${release.version}\nline=${release.line}`);
    } else throw new Error('Usage: release-tag.mjs <vX.Y.Z> [--github-output]');
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
