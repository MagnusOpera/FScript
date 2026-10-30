const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');

const root = path.resolve(__dirname, '../..');
const inventory = `workspace {
    version = "1.2.3"
    id = "6ae115ba87040d459b6ccf40"
}
locals {
    versions = {
        dotnet_sdk: "10.0.401"
        node: "22"
        pnpm: "10.33.0"
        vsce: "4.0.0"
        ovsx: "0.9"
    }
}
`;

function fixture(t, source = inventory) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-tools-'));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  const workspace = path.join(directory, 'WORKSPACE');
  fs.writeFileSync(workspace, source);
  return { directory, workspace };
}

function read(workspace, ...args) {
  return spawnSync('python3', ['.github/scripts/tool-versions.py', '--workspace', workspace, ...args],
    { cwd: root, encoding: 'utf8' });
}

test('shared tool inventory supplies native CLI and container versions', t => {
  const { workspace } = fixture(t);
  const result = read(workspace);
  assert.equal(result.status, 0, result.stderr);
  const values = JSON.parse(result.stdout);
  assert.equal(values.terrabuild, '1.2.3');
  assert.equal(values.dotnet_sdk, '10.0.401');
  assert.equal(values.pnpm, '10.33.0');
  assert.equal(values.workspace_id, '6ae115ba87040d459b6ccf40');
});

test('changing the inventory updates GitHub outputs without editing workflow versions', t => {
  const { directory, workspace } = fixture(t, inventory.replace('10.0.401', '10.0.999'));
  const output = path.join(directory, 'outputs');
  fs.writeFileSync(output, 'existing=value\n');
  const result = read(workspace, '--github-output', output);
  assert.equal(result.status, 0, result.stderr);
  const values = Object.fromEntries(fs.readFileSync(output, 'utf8').trim().split('\n').map(line => line.split('=')));
  assert.equal(values.existing, 'value');
  assert.equal(values.dotnet_sdk, '10.0.999');
  assert.equal(values.terrabuild, '1.2.3');
});

test('incomplete inventories fail before publishing partial GitHub outputs', t => {
  const { directory, workspace } = fixture(t, inventory.replace('        pnpm: "10.33.0"\n', ''));
  const output = path.join(directory, 'outputs');
  const result = read(workspace, '--github-output', output);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /Missing or invalid tool version: pnpm/);
  assert.equal(fs.existsSync(output), false);
});
