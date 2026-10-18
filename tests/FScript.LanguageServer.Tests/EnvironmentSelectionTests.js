const assert = require('node:assert/strict');
const { test } = require('node:test');
const { selectConfiguration } = require('../../vscode-fscript/config-selection');

test('automatic discovery requires a script location and defaults outside configured subtrees', () => {
  assert.deepEqual(selectConfiguration('', undefined, [], '/work/main.fss'), { source: 'default' });
  assert.deepEqual(selectConfiguration('', undefined, ['/work/nested/fscript.toml']), { source: 'default' });
  assert.deepEqual(selectConfiguration('', undefined, ['/work/nested/fscript.toml'], '/work/nested/main.fss'), { source: 'automatic', path: '/work/nested/fscript.toml' });
});
test('explicit none and missing selections persist independently of discovery', () => {
  assert.deepEqual(selectConfiguration('', { kind: 'none' }, ['a']), { source: 'explicit none' });
  assert.deepEqual(selectConfiguration('', { kind: 'manifest', path: 'missing' }, ['a']), { source: 'selection', path: 'missing' });
  assert.deepEqual(selectConfiguration('', { kind: 'automatic' }, ['/work/fscript.toml'], '/work/main.fss'), { source: 'automatic', path: '/work/fscript.toml' });
});
test('setting is authoritative over both persisted modes', () => {
  for (const persisted of [{kind: 'none'}, {kind: 'manifest', path: 'old'}]) {
    assert.deepEqual(selectConfiguration('tools/fscript.toml', persisted, ['a']), { source: 'setting', path: 'tools/fscript.toml' });
  }
});

test('automatic discovery selects the nearest containing manifest, independent of unrelated candidates', () => {
  const candidates = ['/umbrella/product/samples/ffi/fscript.toml', '/umbrella/numerics/examples/fscript.toml'];
  assert.deepEqual(selectConfiguration('', {kind:'automatic'}, candidates, '/umbrella/numerics/examples/kmeans.fss'),
    {path:candidates[1],source:'automatic'});
  assert.deepEqual(selectConfiguration('', undefined, candidates, '/umbrella/product/samples/ffi/main.fss'),
    {path:candidates[0],source:'automatic'});
  assert.deepEqual(selectConfiguration('', undefined, candidates, '/umbrella/plain.fss'), {source:'default'});
  assert.deepEqual(selectConfiguration('', undefined, [candidates[1]], '/umbrella/plain.fss'), {source:'default'});
});
test('nested manifests override parent manifests only for their subtree', () => {
  const candidates = ['/work/fscript.toml','/work/examples/fscript.toml'];
  assert.equal(selectConfiguration('', undefined, candidates, '/work/examples/nested/main.fss').path, candidates[1]);
  assert.equal(selectConfiguration('', undefined, candidates, '/work/examples-other/main.fss').path, candidates[0]);
  assert.equal(selectConfiguration('', undefined, candidates, '/work-other/main.fss').path, undefined);
});
test('explicit selections and settings override even a manifest beside the script', () => {
  const candidates=['/work/examples/fscript.toml'],file='/work/examples/main.fss';
  assert.deepEqual(selectConfiguration('',{kind:'none'},candidates,file),{source:'explicit none'});
  assert.deepEqual(selectConfiguration('',{kind:'manifest',path:'/work/missing.toml'},candidates,file),{source:'selection',path:'/work/missing.toml'});
  assert.deepEqual(selectConfiguration('/work/forced.toml',{kind:'automatic'},candidates,file),{source:'setting',path:'/work/forced.toml'});
});
