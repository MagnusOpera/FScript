const assert = require('node:assert/strict');
const { test } = require('node:test');
const { scriptLaunch } = require('../../vscode-fscript/script-launch');

test('script paths and manifest paths remain literal process arguments', () => {
  const launch = scriptLaunch('/work/a $x.fss', '/work', '/work/config with spaces.toml');
  assert.equal(launch.command, 'fscript');
  assert.deepEqual(launch.args, ['--config', '/work/config with spaces.toml', '/work/a $x.fss']);
});
test('standalone scripts use defaults and local repository scripts use dotnet CLI', () => {
  assert.deepEqual(scriptLaunch('/tmp/a.fss').args, ['/tmp/a.fss']);
  assert.equal(scriptLaunch('/tmp/a.fss').cwd, '/tmp');
  assert.deepEqual(scriptLaunch('/repo/main.fss', '/repo', undefined, '/repo/src/FScript/FScript.fsproj').args,
    ['run', '--project', '/repo/src/FScript/FScript.fsproj', '--', '/repo/main.fss']);
});
