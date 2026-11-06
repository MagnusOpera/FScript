import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';

const matrix = JSON.parse(fs.readFileSync('.build/ci-shards.json', 'utf8')).include;
const expected = {
  core: ['tests/fscript.language.tests:test', 'tests/fscript.runtime.tests:test', 'tests/fscript.compiler.tests:test',
    'tests/fscript.conformance.tests:test', 'tests/fscript.interpreter.lir.tests:test',
    'tests/fscript.languageserver.tests:test', 'samples/ffi/tests:test_ffi_managed'],
  backend: ['tests/fscript.backend.dotnet.tests:test'],
  cli: ['tests/fscript.cli.tests:test'],
  'ffi-default': ['samples/ffi/tests:test_ffi_default'],
  'ffi-size': ['samples/ffi/tests:test_ffi_size'],
  web: ['src/fscript.javascript:test', 'vscode-fscript:test', 'website:test_ci'],
  integration: ['tests/fscript.debugadapter.tests:test', 'tests/fscript.debugadapter.tests:smoke',
    'tests/fscript.debugadapter.tests:sdk']
};

test('CI runner forwards the exact project selection and propagates failures', () => {
  const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-ci-shard-'));
  try {
    fs.mkdirSync(path.join(fixture, '.build'));
    fs.copyFileSync('.build/ci-shards.json', path.join(fixture, '.build/ci-shards.json'));
    fs.writeFileSync(path.join(fixture, '.build/run-terrabuild.sh'),
      'printf "%s\\n" "$@"\nexit "${CI_FIXTURE_EXIT:-0}"\n');
    const script = fileURLToPath(new URL('./run-ci-shard.mjs', import.meta.url));
    const run = (name, extra = {}) => spawnSync(process.execPath, [script, name], {
      cwd: fixture, encoding: 'utf8', env: { ...process.env, TB_FLAGS: '--local-only', ...extra }
    });
    const core = run('core');
    assert.equal(core.status, 0, core.stderr);
    assert.deepEqual(core.stdout.trim().split('\n'),
      ['ci', '--project', 'ci_core', 'ci_ffi_managed', '--debug', '--log', '--local-only']);
    assert.equal(run('ffi-default', { CI_FIXTURE_EXIT: '7' }).status, 7);
    assert.notEqual(run('missing').status, 0);
    const shared = run('ffi-size', { TB_FLAGS: '' });
    assert.equal(shared.status, 0);
    assert.ok(!shared.stdout.includes('--local-only'));
  } finally {
    fs.rmSync(fixture, { recursive: true, force: true });
  }
});

test('static CI shards select exactly their checks, restore reports, and cover every test project', () => {
  assert.deepEqual(matrix.map(shard => shard.name).sort(), Object.keys(expected).sort());
  const covered = [];
  for (const shard of matrix) {
    execFileSync('terrabuild', ['run', 'ci', '--project', ...shard.projects,
      '--dry-run', '--local-only', '--debug'], { stdio: 'pipe', timeout: 30000 });
    const graph = JSON.parse(fs.readFileSync('terrabuild-debug.json', 'utf8'));
    const selected = graph.nodes.filter(node => node.selected);
    const checks = selected.filter(node => /^(test(?:_ffi_.*|_ci)?|smoke|sdk)$/.test(node.target));
    const names = checks.map(node => `${node.projectDirectory.toLowerCase()}:${node.target}`).sort();
    assert.deepEqual(names, expected[shard.name].toSorted(), shard.name);
    assert.ok(selected.some(node => node.target === 'ci' && node.build === 'always'), shard.name);
    for (const consumer of selected.filter(node => node.target === 'ci')) {
      assert.ok(consumer.resolvedOperations.length > 0, 'Cached reports need an executing consumer.');
    }
    for (const check of checks) {
      const direct = check.dependencies.map(id => graph.nodes.find(node => node.id === id));
      if (check.projectName === 'ffisample_tests') {
        assert.ok(direct.some(node => node?.projectName === 'fscript' && node.target === 'build'),
          `${check.target} must directly restore the CLI even when its own build is cached.`);
      }
      if (check.projectName === 'website') {
        assert.equal(check.target, 'test_ci');
        assert.deepEqual(check.resolvedOperations.map(operation => operation.metaCommand), ['@npm install', '@npm run'],
          'The CI website target must recreate npm launchers before typechecking.');
        assert.ok(direct.some(node => node?.projectName === 'javascript' && node.target === 'build'));
      }
    }
    covered.push(...checks.filter(node => node.target.startsWith('test')).map(node =>
      `${node.projectDirectory.toLowerCase()}:test`));
  }
  const allTests = [];
  function scan(directory) {
    for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
      if (['node_modules', 'bin', 'obj', '.git', '.terrabuild', 'TestResults', '.out'].includes(entry.name)) continue;
      const file = path.join(directory, entry.name);
      if (entry.isDirectory()) scan(file);
      else if (entry.name === 'PROJECT' && /target test\s*\{/.test(fs.readFileSync(file, 'utf8'))) {
        allTests.push(`${directory.replace(/^\.\//, '').toLowerCase()}:test`);
      }
    }
  }
  scan('.');
  assert.deepEqual([...new Set(covered)].sort(), allTests.sort());
  const duplicates = covered.filter((name, index) => covered.indexOf(name) !== index);
  assert.deepEqual(duplicates, ['samples/ffi/tests:test', 'samples/ffi/tests:test']);
});
