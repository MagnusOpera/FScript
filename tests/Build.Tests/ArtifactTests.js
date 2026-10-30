const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');

const root = path.resolve(__dirname, '../..');

test('materialization restores missing solution outputs from a cached artifact bundle', t => {
  const workspace = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-artifacts-'));
  t.after(() => fs.rmSync(workspace, { recursive: true, force: true }));
  const outputs = [
    'src/FScript/bin/Release/net10.0/fscript.dll',
    'tests/FScript.Language.Tests/bin/Release/net10.0/FScript.Language.Tests.dll',
    'samples/ffi/Tests/obj/project.assets.json',
  ];
  for (const output of outputs) {
    fs.mkdirSync(path.dirname(path.join(workspace, output)), { recursive: true });
    fs.writeFileSync(path.join(workspace, output), output);
  }
  fs.mkdirSync(path.join(workspace, 'build/artifacts'), { recursive: true });
  const pack = spawnSync('tar', ['-cf', 'build/artifacts/solution.tar', 'src', 'tests', 'samples'],
    { cwd: workspace, encoding: 'utf8' });
  assert.equal(pack.status, 0, pack.stderr);
  for (const directory of ['src', 'tests', 'samples']) {
    fs.rmSync(path.join(workspace, directory), { recursive: true });
  }
  fs.mkdirSync(path.join(workspace, '.github/scripts'), { recursive: true });
  const helper = '.github/scripts/terrabuild-check.sh';
  fs.copyFileSync(path.join(root, helper), path.join(workspace, helper));
  const tools = path.join(workspace, 'tools');
  const restoreLog = path.join(workspace, 'restore-arguments');
  fs.mkdirSync(tools);
  fs.writeFileSync(path.join(tools, 'dotnet'),
    '#!/bin/sh\nprintf "%s\\n" "$@" > "$FSCRIPT_RESTORE_LOG"\n', { mode: 0o755 });
  const restore = spawnSync('bash', [helper, 'materialize'], {
    cwd: workspace,
    encoding: 'utf8',
    env: { ...process.env, PATH: `${tools}${path.delimiter}${process.env.PATH}`, FSCRIPT_RESTORE_LOG: restoreLog },
  });
  assert.equal(restore.status, 0, restore.stderr);
  assert.deepEqual(fs.readFileSync(restoreLog, 'utf8').trim().split('\n'), ['restore', 'FScript.sln']);
  for (const output of outputs) {
    assert.equal(fs.readFileSync(path.join(workspace, output), 'utf8'), output);
  }
});
