const assert = require('node:assert/strict');
const { test } = require('node:test');
const { cliCommand } = require('../../vscode-fscript/cli-command');
const { scriptLaunch } = require('../../vscode-fscript/script-launch');

test('CLI defaults to PATH and preserves alternate command names', () => {
  assert.deepEqual(cliCommand('  '), { command: 'fscript', args: [] });
  assert.deepEqual(cliCommand('fscript-preview'), { command: 'fscript-preview', args: [] });
});
test('executable paths are literal and workspace relative', () => {
  assert.deepEqual(cliCommand('./bin/fscript preview', '/work'), { command: '/work/bin/fscript preview', args: [] });
  assert.deepEqual(cliCommand('/tools/fscript $x'), { command: '/tools/fscript $x', args: [] });
});
test('managed CLI runs through dotnet with a separate DLL argument', () => {
  assert.deepEqual(cliCommand('bin/fscript.DLL', '/work'), { command: 'dotnet', args: ['/work/bin/fscript.DLL'] });
  assert.deepEqual(cliCommand('./fscript.dll', undefined, '/standalone/main.fss'), { command: 'dotnet', args: ['/standalone/fscript.dll'] });
});
test('Run Current Script preserves CLI, manifest and script argument boundaries', () => {
  assert.deepEqual(scriptLaunch('/work/main.fss', '/work', '/work/ffi config.toml', 'bin/fscript.dll'), {
    command: 'dotnet', args: ['/work/bin/fscript.dll', '--config', '/work/ffi config.toml', '/work/main.fss'], cwd: '/work'
  });
});
