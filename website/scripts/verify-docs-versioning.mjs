import {existsSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const versions = JSON.parse(readFileSync(path.join(root, 'versions.json'), 'utf8'));
const maintainedVersion = versions[0];

if (typeof maintainedVersion !== 'string' || maintainedVersion.length === 0) {
  throw new Error('Unable to determine the maintained documentation version.');
}

const maintainedPage = process.env.FSCRIPT_DOCS_LAST_VERSION === maintainedVersion
  ? path.join(root, 'build', 'manual', 'index.html')
  : path.join(root, 'build', 'manual', maintainedVersion, 'index.html');

if (!existsSync(maintainedPage)) {
  throw new Error(`Maintained documentation page was not built: ${maintainedPage}`);
}

const html = readFileSync(maintainedPage, 'utf8');
if (html.includes('no longer actively maintained')) {
  throw new Error(`Maintained documentation ${maintainedVersion} is marked as unmaintained.`);
}

console.log(`Verified maintained documentation version: ${maintainedVersion}`);
