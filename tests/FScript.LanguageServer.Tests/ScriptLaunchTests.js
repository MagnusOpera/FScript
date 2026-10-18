const assert = require('node:assert/strict');
const { test } = require('node:test');
const { scriptLaunch } = require('../../vscode-fscript/script-launch');

test('script paths and manifest paths remain literal process arguments', () => {
  const launch = scriptLaunch('/work/a $x.fss', '/work', '/work/config with spaces.toml');
  assert.equal(launch.command, 'fscript');
  assert.deepEqual(launch.args, ['--config', '/work/config with spaces.toml', '/work/a $x.fss']);
});
test('standalone and repository scripts both use the installed CLI', () => {
  assert.deepEqual(scriptLaunch('/tmp/a.fss').args, ['/tmp/a.fss']);
  assert.equal(scriptLaunch('/tmp/a.fss').cwd, '/tmp');
  const repository = scriptLaunch('/repo/main.fss', '/repo');
  assert.equal(repository.command, 'fscript');
  assert.deepEqual(repository.args, ['/repo/main.fss']);
});
