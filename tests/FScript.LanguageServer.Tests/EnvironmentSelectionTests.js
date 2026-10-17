const assert = require('node:assert/strict');
const { test } = require('node:test');
const { selectConfiguration } = require('../../vscode-fscript/config-selection');

test('automatic discovery handles zero, one and multiple candidates', () => {
  assert.deepEqual(selectConfiguration('', undefined, []), { source: 'default' });
  assert.deepEqual(selectConfiguration('', undefined, ['nested/fscript.toml']), { source: 'automatic', path: 'nested/fscript.toml' });
  assert.deepEqual(selectConfiguration('', undefined, ['a', 'b']), { source: 'selection needed' });
});
test('explicit none and missing selections persist independently of discovery', () => {
  assert.deepEqual(selectConfiguration('', { kind: 'none' }, ['a']), { source: 'explicit none' });
  assert.deepEqual(selectConfiguration('', { kind: 'manifest', path: 'missing' }, ['a']), { source: 'selection', path: 'missing' });
  assert.deepEqual(selectConfiguration('', { kind: 'automatic' }, ['a']), { source: 'automatic', path: 'a' });
});
test('setting is authoritative over both persisted modes', () => {
  for (const persisted of [{kind: 'none'}, {kind: 'manifest', path: 'old'}]) {
    assert.deepEqual(selectConfiguration('tools/fscript.toml', persisted, ['a']), { source: 'setting', path: 'tools/fscript.toml' });
  }
});
