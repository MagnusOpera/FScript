import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {chmodSync, mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {test} from 'node:test';
import {fileURLToPath} from 'node:url';

const script = fileURLToPath(new URL('./release-tag.mjs', import.meta.url));

test('tag normalization works through symlinks and returns numeric package versions', (t) => {
  const root = mkdtempSync(path.join(os.tmpdir(), 'release-tag-cli-'));
  t.after(() => rmSync(root, {recursive: true, force: true}));
  const linked = path.join(root, 'tag.mjs');
  symlinkSync(script, linked);
  assert.equal(execFileSync(process.execPath, [linked, 'v1.2.3'], {encoding: 'utf8'}), '1.2.3\n');
  assert.equal(execFileSync(process.execPath, [linked, 'v1.2.3', '--github-output'], {encoding: 'utf8'}), 'tag=v1.2.3\nversion=1.2.3\nline=1.2\n');
  assert.throws(() => execFileSync(process.execPath, [linked, '1.2.3'], {stdio: 'pipe'}));
});

test('tap generation uses the prefixed release URL and numeric archive and formula versions', (t) => {
  const root = mkdtempSync(path.join(os.tmpdir(), 'release-tag-tap-'));
  t.after(() => rmSync(root, {recursive: true, force: true}));
  const bin = path.join(root, 'bin');
  mkdirSync(bin);
  const stub = (name, content) => {
    writeFileSync(path.join(bin, name), `#!${process.execPath}\n${content}\n`);
    chmodSync(path.join(bin, name), 0o755);
  };
  stub('gh', `const fs = require('node:fs'); const args = process.argv.slice(2);
fs.appendFileSync(process.env.TAG_TEST_LOG, JSON.stringify(args) + '\\n');
for (let i = 0; i < args.length; i++) if (args[i] === '-p') fs.writeFileSync(args[i + 1], 'asset');`);
  stub('sha256sum', `console.log('0'.repeat(64) + ' asset');`);
  stub('envsubst', `const fs = require('node:fs');process.stdout.write(fs.readFileSync(0, 'utf8').replace(/\\$\\{([A-Z_0-9]+)\\}/g, (_, key) => process.env[key] ?? ''));`);
  const log = path.join(root, 'downloads.jsonl');
  const env = {...process.env, PATH: `${bin}${path.delimiter}${process.env.PATH}`, TAG_TEST_LOG: log, FSCRIPT_GH_REPO: 'example/repo'};
  const generator = fileURLToPath(new URL('./generate-homebrew-tap', import.meta.url));
  const template = fileURLToPath(new URL('./fscript-formula-template.rb', import.meta.url));
  const output = execFileSync('bash', [generator, 'v1.2.3', template], {env, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']});
  assert.match(output, /version "1\.2\.3"/);
  assert.match(output, /class Fscript < Formula/);
  for (const platform of ['darwin-arm64', 'linux-x64', 'linux-arm64']) {
    assert.ok(output.includes(`releases/download/v1.2.3/fscript-1.2.3-${platform}.zip`));
  }
  const downloads = JSON.parse(readFileSync(log, 'utf8').trim());
  assert.ok(downloads.includes('v1.2.3'));
  assert.ok(!downloads.includes('1.2.3'));
  assert.throws(() => execFileSync('bash', [generator, '1.2.3', template], {env, stdio: 'pipe'}));
  assert.equal(readFileSync(log, 'utf8').trim().split('\n').length, 1, 'Invalid tags must fail before downloading or publishing');
});
