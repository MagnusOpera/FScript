import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';
import { readToolVersions } from '../../.build/read-tool-versions.mjs';

const source = `locals {
    versions = {
        dotnet_sdk: "10.0.999" # SDK used by Docker and Actions
        node: "99"
        terrabuild: "latest"
        vsce: "9.8.7"
        ovsx: "9.6"
    }
}`;

test('workflow outputs follow the checked-out WORKSPACE without version fallbacks', () => {
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-tool-versions-'));
    try {
        fs.writeFileSync(path.join(fixture, 'WORKSPACE'), source);
        const script = fileURLToPath(new URL('../../.build/read-tool-versions.mjs', import.meta.url));
        const output = execFileSync(process.execPath, [script], { cwd: fixture, encoding: 'utf8' });
        assert.equal(output, 'dotnet_sdk=10.0.999\nnode=99\nterrabuild=latest\nvsce=9.8.7\novsx=9.6\n');
    } finally {
        fs.rmSync(fixture, { recursive: true, force: true });
    }
});

test('missing, duplicate, or computed versions fail before tool installation', () => {
    assert.throws(() => readToolVersions('locals {}'), /one locals.versions block/);
    assert.throws(() => readToolVersions(source.replace('node: "99"', '')), /Missing tool version: node/);
    assert.throws(() => readToolVersions(source.replace('node: "99"', 'node: "99"\nnode: "98"')), /Duplicate tool version/);
    assert.throws(() => readToolVersions(source.replace('node: "99"', 'node: var.node')), /quoted literal values/);
    assert.throws(() => readToolVersions(source.replace('node: "99"', 'node: "$(whoami)"')), /quoted literal values/);
});

test('native toolchain tags follow workspace tool versions and Dockerfile inputs', () => {
    const root = fileURLToPath(new URL('../../', import.meta.url));
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-toolchain-identity-'));
    try {
        fs.mkdirSync(path.join(fixture, '.build/toolchain'), { recursive: true });
        for (const file of ['WORKSPACE', '.build/toolchain/PROJECT', '.build/toolchain/Dockerfile']) {
            fs.copyFileSync(path.join(root, file), path.join(fixture, file));
        }
        fs.mkdirSync(path.join(fixture, 'app'));
        fs.writeFileSync(path.join(fixture, 'app/App.csproj'), '<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup></Project>');
        fs.writeFileSync(path.join(fixture, 'app/PROJECT'), 'project fixture { @dotnet { } }\ntarget build { @dotnet build { dependencies = false } }\n');
        execFileSync('git', ['init', '--quiet'], { cwd: fixture });
        execFileSync('git', ['add', '.'], { cwd: fixture });
        execFileSync('git', ['-c', 'user.name=Test', '-c', 'user.email=test@example.com', 'commit', '--quiet', '-m', 'fixture'], { cwd: fixture });
        const image = () => {
            const output = execFileSync('terrabuild', ['explain', 'build', '--local-only'], { cwd: fixture, encoding: 'utf8', timeout: 30000 });
            const match = output.match(/container: (ghcr\.io\/magnusopera\/fscript-toolchain:[^\s]+)/);
            assert.ok(match, 'The build must use the workspace toolchain image.');
            return match[1];
        };
        const original = image();
        const workspace = path.join(fixture, 'WORKSPACE');
        const contents = fs.readFileSync(workspace, 'utf8');
        fs.writeFileSync(workspace, fs.readFileSync(workspace, 'utf8').replace(/dotnet_sdk: "[^"]+"/, 'dotnet_sdk: "10.0.999"'));
        assert.notEqual(image(), original);
        fs.writeFileSync(workspace, contents.replace(/node: "[^"]+"/, 'node: "99"'));
        assert.notEqual(image(), original);
        fs.writeFileSync(workspace, contents);
        const dockerfile = path.join(fixture, '.build/toolchain/Dockerfile');
        fs.appendFileSync(dockerfile, '\nENV TOOLCHAIN_IDENTITY_REGRESSION=1\n');
        assert.notEqual(image(), original);
    } finally {
        fs.rmSync(fixture, { recursive: true, force: true });
    }
});
