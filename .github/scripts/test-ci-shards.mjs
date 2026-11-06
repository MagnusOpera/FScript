import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';

const matrix = JSON.parse(fs.readFileSync('.build/ci-shards.json', 'utf8')).include;
const shardArgs = name => matrix.find(shard => shard.name === name).runs.flatMap(selection =>
  [...selection.targets, '--project', ...selection.projects, '--restore', '--debug', '--log', '--local-only']);
const trx = '<TestRun><Results><UnitTestResult outcome="Passed"/></Results><ResultSummary><Counters executed="1" failed="0" error="0"/></ResultSummary></TestRun>';
const junit = '<testsuites><testcase name="fixture"/></testsuites>';
const fakeReportsScript = `node --input-type=module -e 'import fs from "node:fs"; import path from "node:path";
for (const file of JSON.parse(process.env.CI_FIXTURE_REPORTS)) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, file.endsWith(".trx") ? ${JSON.stringify(trx)} : ${JSON.stringify(junit)});
}'`;
const allReports = matrix.flatMap(shard => shard.runs.flatMap(selection => selection.reports));
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
      'printf "%s\\n" "$@"\n' + fakeReportsScript + '\nexit "${CI_FIXTURE_EXIT:-0}"\n');
    const script = fileURLToPath(new URL('./run-ci-shard.mjs', import.meta.url));
    const run = (name, extra = {}) => spawnSync(process.execPath, [script, name], {
      cwd: fixture, encoding: 'utf8', env: { ...process.env, TB_FLAGS: '--local-only', CI_FIXTURE_REPORTS: JSON.stringify(allReports), ...extra }
    });
    const core = run('core');
    assert.equal(core.status, 0, core.stderr);
    assert.deepEqual(core.stdout.trim().split('\n').filter(line => !line.startsWith('Verified test report:')),
      shardArgs('core'));
    assert.equal(run('ffi-default', { CI_FIXTURE_EXIT: '7' }).status, 7);
    assert.notEqual(run('missing').status, 0);
    const shared = run('ffi-size', { TB_FLAGS: '' });
    assert.equal(shared.status, 0);
    assert.ok(!shared.stdout.includes('--local-only'));
  } finally {
    fs.rmSync(fixture, { recursive: true, force: true });
  }
});

test('multiple selections retain diagnostics and cannot mask an earlier failure', () => {
  const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-ci-diagnostics-'));
  try {
    fs.mkdirSync(path.join(fixture, '.build'));
    fs.copyFileSync('.build/ci-shards.json', path.join(fixture, '.build/ci-shards.json'));
    fs.writeFileSync(path.join(fixture, '.build/run-terrabuild.sh'), `
mkdir -p .terrabuild
printf '%s' "$1" > .terrabuild/run.log
if [ "$1" = test ]; then
  printf '{"selection":"test"}' > terrabuild-debug.json
  exit 7
fi
printf '{"status":"success"}' > .terrabuild/result.json
`);
    const script = fileURLToPath(new URL('./run-ci-shard.mjs', import.meta.url));
    const result = spawnSync(process.execPath, [script, 'core'], { cwd: fixture, encoding: 'utf8' });
    assert.equal(result.status, 7, result.stderr);
    const read = file => fs.readFileSync(path.join(fixture, '.terrabuild/shards', file), 'utf8');
    assert.equal(read('1/terrabuild-debug.json'), '{"selection":"test"}');
    assert.equal(read('2/result.json'), '{"status":"success"}');
    assert.equal(read('2/run.log'), 'test_ffi_managed');
    assert.ok(!fs.existsSync(path.join(fixture, '.terrabuild/shards/2/terrabuild-debug.json')),
      'A later startup failure must not inherit diagnostics from the previous selection.');
  } finally {
    fs.rmSync(fixture, { recursive: true, force: true });
  }
});

test('a cached shard restores reports without rerunning tests or restoring build outputs', () => {
  const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-ci-restoration-'));
  try {
    fs.mkdirSync(path.join(fixture, '.build'));
    fs.mkdirSync(path.join(fixture, 'app'));
    fs.copyFileSync('.build/run-terrabuild.sh', path.join(fixture, '.build/run-terrabuild.sh'));
    fs.writeFileSync(path.join(fixture, '.build/ci-shards.json'), JSON.stringify({ include: [
      { name: 'fixture', runs: [{ targets: ['test'], projects: ['fixture'], reports: ['app/TestResults/report.xml'] }] }
    ] }));
    fs.writeFileSync(path.join(fixture, '.gitignore'), 'app/bin/\napp/TestResults/\n.terrabuild/\nterrabuild-debug.*\napp/executions\n');
    fs.writeFileSync(path.join(fixture, 'WORKSPACE'), `workspace { engine = ~host }
variable config { default = "Release" }
target build { }
target test { }
`);
    fs.writeFileSync(path.join(fixture, 'app/PROJECT'), `project fixture { }
target build {
  artifacts = ~managed
  outputs = [ "bin/" ]
  @shell sh { args = "build.sh" }
}
target test {
  artifacts = ~managed
  depends_on = [ target.build ]
  outputs = [ "TestResults/" ]
  @shell sh { args = "test.sh" }
}
`);
    fs.writeFileSync(path.join(fixture, 'app/build.sh'), 'mkdir -p bin\nprintf binary > bin/output\n');
    fs.writeFileSync(path.join(fixture, 'app/test.sh'), 'test -f bin/output\nmkdir -p TestResults\nprintf \'<testsuites><testcase name="fixture"/></testsuites>\' > TestResults/report.xml\nprintf run >> executions\n');
    // Keep this fixture independent of entries left by earlier test invocations.
    fs.writeFileSync(path.join(fixture, 'app/nonce'), path.basename(fixture));
    execFileSync('git', ['init', '--quiet'], { cwd: fixture });
    execFileSync('git', ['add', '.'], { cwd: fixture });
    execFileSync('git', ['-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', 'commit', '--quiet', '-m', 'fixture'], { cwd: fixture });
    const script = fileURLToPath(new URL('./run-ci-shard.mjs', import.meta.url));
    const run = () => execFileSync(process.execPath, [script, 'fixture'], {
      cwd: fixture, env: { ...process.env, TB_FLAGS: '--local-only' }, timeout: 30000, stdio: 'pipe'
    });
    run();
    fs.rmSync(path.join(fixture, 'app/bin'), { recursive: true });
    fs.rmSync(path.join(fixture, 'app/TestResults'), { recursive: true });
    run();
    assert.equal(fs.readFileSync(path.join(fixture, 'app/TestResults/report.xml'), 'utf8'), junit);
    assert.equal(fs.readFileSync(path.join(fixture, 'app/executions'), 'utf8'), 'run');
    assert.ok(!fs.existsSync(path.join(fixture, 'app/bin')), 'Cached reports do not need build prerequisites.');
    const graph = JSON.parse(fs.readFileSync(path.join(fixture, '.terrabuild/shards/1/terrabuild-debug.json'), 'utf8'));
    assert.equal(graph.nodes.find(node => node.target === 'test').action, 'restore');
  } finally {
    fs.rmSync(fixture, { recursive: true, force: true });
  }
});

test('a zero-exit test process cannot reuse stale reports or succeed without execution evidence', () => {
  const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-ci-empty-tests-'));
  try {
    fs.mkdirSync(path.join(fixture, '.build'));
    fs.mkdirSync(path.join(fixture, 'app/TestResults'), { recursive: true });
    fs.writeFileSync(path.join(fixture, '.build/ci-shards.json'), JSON.stringify({ include: [
      { name: 'fixture', runs: [{ targets: ['test'], projects: ['fixture'], reports: ['app/TestResults/results.trx'] }] }
    ] }));
    const script = fileURLToPath(new URL('./run-ci-shard.mjs', import.meta.url));
    const stub = path.join(fixture, '.build/run-terrabuild.sh');
    fs.writeFileSync(path.join(fixture, 'app/TestResults/results.trx'), trx);
    fs.writeFileSync(stub, 'exit 0\n');
    const run = () => spawnSync(process.execPath, [script, 'fixture'], { cwd: fixture, encoding: 'utf8' });
    const missing = run();
    assert.notEqual(missing.status, 0);
    assert.match(missing.stderr, /Missing expected test report/);
    fs.writeFileSync(stub, `mkdir -p app/TestResults
printf '%s' '<TestRun><Results/><ResultSummary><Counters executed="0"/></ResultSummary></TestRun>' > app/TestResults/results.trx
exit 0
`);
    const empty = run();
    assert.notEqual(empty.status, 0);
    assert.match(empty.stderr, /no executed tests/);
  } finally {
    fs.rmSync(fixture, { recursive: true, force: true });
  }
});

test('static CI shards select exactly their checks, restore reports, and cover every test project', () => {
  assert.deepEqual(matrix.map(shard => shard.name).sort(), Object.keys(expected).sort());
  const covered = [];
  for (const shard of matrix) {
    const checks = [];
    const nodes = new Map();
    for (const selection of shard.runs) {
      execFileSync('terrabuild', ['run', ...selection.targets, '--project', ...selection.projects,
        '--restore', '--dry-run', '--local-only', '--debug'], { stdio: 'pipe', timeout: 30000 });
      const graph = JSON.parse(fs.readFileSync('terrabuild-debug.json', 'utf8'));
      for (const node of graph.nodes) nodes.set(node.id, node);
      assert.ok(!graph.nodes.some(node => node.projectDirectory.startsWith('.build/ci/')),
        'Shards must not use dummy report consumers.');
      checks.push(...graph.nodes.filter(node => node.selected && /^(test(?:_ffi_.*|_ci)?|smoke|sdk)$/.test(node.target)));
    }
    const names = checks.map(node => `${node.projectDirectory.toLowerCase()}:${node.target}`).sort();
    assert.deepEqual(names, expected[shard.name].toSorted(), shard.name);
    for (const check of checks) {
      const direct = check.dependencies.map(id => nodes.get(id));
      const commands = check.resolvedOperations.map(operation => operation.metaCommand);
      if (commands.includes('@dotnet test')) {
        assert.equal(check.artifacts, 'managed', 'Explicit package restore must preserve shared test-result caching.');
        assert.deepEqual(commands, ['@dotnet restore', '@dotnet test'],
          'A fresh runner must restore package-provided MSBuild imports before testing cached builds.');
        assert.ok(shard.runs.some(selection => selection.projects.includes(check.projectName) &&
          selection.reports.includes(`${check.projectDirectory}/${check.outputs[0]}results.trx`)),
          'Every .NET test target needs a checked report contract.');
      }
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
    if (directory === '.build/ci') return;
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
