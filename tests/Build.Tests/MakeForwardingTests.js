const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');

const root = path.resolve(__dirname, '../..');
const targets = {
  build: ['build', 'materialize'],
  test: ['test'],
  'smoke-tests': ['smoke'],
  'verify-sdk-provider': ['sdk'],
  'fable-build': ['javascript_build', '--project', 'src/FScript.JavaScript'],
  'fable-test': ['check', '--project', 'src/FScript.JavaScript'],
  'website-install': ['install', '--project', 'website'],
  'website-build': ['check', '--project', 'website'],
  'website-typecheck': ['typecheck', '--project', 'website'],
};

for (const parallel of [undefined, '7']) {
  for (const [target, args] of Object.entries(targets)) {
    test(`make ${target} delegates once with ${parallel ? `parallel=${parallel}` : 'default concurrency'}`, t => {
      const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-make-'));
      t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
      const executable = path.join(directory, 'terrabuild');
      const log = path.join(directory, 'arguments');
      fs.writeFileSync(executable, '#!/bin/sh\nprintf "%s\\n" "$@" >> "$FSCRIPT_MAKE_TEST_LOG"\n', { mode: 0o755 });
      const env = { ...process.env, FSCRIPT_MAKE_TEST_LOG: log };
      for (const key of ['parallel', 'MAKEFLAGS', 'MFLAGS', 'MAKEOVERRIDES']) delete env[key];
      const result = spawnSync('make', ['--no-print-directory', '-s', target,
        `TERRABUILD=${executable}`, ...(parallel ? [`parallel=${parallel}`] : []), 'TERRABUILD_ARGS=--local-only'], {
        cwd: root,
        encoding: 'utf8',
        env,
      });
      assert.equal(result.status, 0, result.stderr);
      const projectIndex = args.indexOf('--project');
      const targetArgs = projectIndex < 0 ? args : args.slice(0, projectIndex);
      const selectionArgs = projectIndex < 0 ? [] : args.slice(projectIndex);
      assert.deepEqual(fs.readFileSync(log, 'utf8').trim().split('\n'),
        ['run', 'toolchain', ...targetArgs, ...(parallel ? ['--parallel', parallel] : []), '--local-only', ...selectionArgs]);
    });
  }
}
