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
            const match = output.match(/container: (ghcr\.io\/magnusopera\/fscript\/toolchain:[^\s]+)/);
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
test('workflow and changelog edits preserve VS Code cache keys while website tracks release notes', () => {
    const root = fileURLToPath(new URL('../../', import.meta.url));
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-frontend-inputs-'));
    try {
        // Copy tracked inputs only; generated packaging files must stay untracked.
        const files = execFileSync('git', ['ls-files', '-z'], { cwd: root }).toString().split('\0').filter(Boolean);
        for (const file of files) {
            const source = path.join(root, file);
            const destination = path.join(fixture, file);
            fs.mkdirSync(path.dirname(destination), { recursive: true });
            if (fs.lstatSync(source).isSymbolicLink()) fs.symlinkSync(fs.readlinkSync(source), destination);
            else fs.copyFileSync(source, destination);
        }
        const packageFile = path.join(fixture, 'vscode-fscript/package.json');
        const manifest = JSON.parse(fs.readFileSync(packageFile, 'utf8'));
        // Exercise the real packaging hook without installing the compiler in this fixture.
        manifest.scripts.compile = 'node -e ""';
        fs.writeFileSync(packageFile, JSON.stringify(manifest));
        execFileSync('git', ['init', '--quiet'], { cwd: fixture });
        execFileSync('git', ['add', '.'], { cwd: fixture });
        execFileSync('git', ['-c', 'user.name=Test', '-c', 'user.email=test@example.com', 'commit', '--quiet', '-m', 'fixture'], { cwd: fixture });
        const keys = () => {
            execFileSync('terrabuild', ['run', 'build', '--project', 'vscode', 'website', '--dry-run', '--local-only', '--debug'],
                { cwd: fixture, stdio: 'pipe', timeout: 30000 });
            const graph = JSON.parse(fs.readFileSync(path.join(fixture, 'terrabuild-debug.json'), 'utf8'));
            return Object.fromEntries(graph.nodes.filter(node => ['vscode', 'website'].includes(node.projectName) && node.target === 'build')
                .map(node => [node.projectName, node.fingerprint.cacheKey]));
        };
        const original = keys();
        fs.appendFileSync(path.join(fixture, '.github/workflows/ci.yml'), '\n# Workflow-only change\n');
        assert.deepEqual(keys(), original, 'Workflow changes must not invalidate frontend builds.');
        fs.appendFileSync(path.join(fixture, 'CHANGELOG.md'), '\n- Release-note-only change.\n');
        const releaseNotes = keys();
        assert.equal(releaseNotes.vscode, original.vscode, 'Release notes are not a compilation input.');
        assert.notEqual(releaseNotes.website, original.website, 'The website must still track the changelog.');
        execFileSync('npm', ['run', 'vscode:prepublish'], { cwd: path.join(fixture, 'vscode-fscript'), stdio: 'pipe' });
        assert.equal(fs.readFileSync(path.join(fixture, 'vscode-fscript/CHANGELOG.md'), 'utf8'),
            fs.readFileSync(path.join(fixture, 'CHANGELOG.md'), 'utf8'), 'Packaging must include current release notes.');
        assert.deepEqual(keys(), releaseNotes, 'Generated packaging notes must not invalidate builds.');
        fs.appendFileSync(path.join(fixture, 'vscode-fscript/extension.js'), '\n// Source change\n');
        assert.notEqual(keys().vscode, original.vscode, 'Extension source changes must still invalidate its build.');
    } finally {
        fs.rmSync(fixture, { recursive: true, force: true });
    }
});
