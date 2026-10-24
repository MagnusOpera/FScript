const { test } = require('node:test');
const assert = require('node:assert/strict');
const path = require('node:path');
const { debugLaunch } = require('../../vscode-fscript/debug-launch');

test('debug launch uses editor environment and preserves Unicode/space arguments', () => {
  const program = path.resolve('/tmp/FScript démo/main.fss');
  const result = debugLaunch({ args: ['hello world', '🫖'], stopOnEntry: true }, program, '/tmp/FScript démo', '/tmp/FScript démo/config/fscript.toml');
  assert.equal(result.program, program);
  assert.equal(result.configurationPath, '/tmp/FScript démo/config/fscript.toml');
  assert.deepEqual(result.args, ['hello world', '🫖']);
  assert.equal(result.stopOnEntry, true);
});
test('no selected editor manifest stays explicit and standalone launch uses script directory', () => {
  const result = debugLaunch({}, '/tmp/example/main.fss');
  assert.equal(result.configurationPath, '');
  assert.equal(result.cwd, '/tmp/example');
  assert.equal(result.stopOnEntry, false);
});
test('debugger requires a saved FScript program', () => {
  assert.throws(() => debugLaunch({}, undefined), /Save an FScript/);
  assert.throws(() => debugLaunch({}, '/tmp/main.txt'), /Save an FScript/);
});
