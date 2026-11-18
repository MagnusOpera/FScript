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
    }
}`;

test('workflow outputs follow the checked-out WORKSPACE without version fallbacks', () => {
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-tool-versions-'));
    try {
        fs.writeFileSync(path.join(fixture, 'WORKSPACE'), source);
        const script = fileURLToPath(new URL('../../.build/read-tool-versions.mjs', import.meta.url));
        const output = execFileSync(process.execPath, [script], { cwd: fixture, encoding: 'utf8' });
        assert.equal(output, 'dotnet_sdk=10.0.999\nnode=99\nterrabuild=latest\n');
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
        for (const file of ['WORKSPACE', '.build/toolchain/PROJECT', '.build/toolchain/Dockerfile', '.build/workflow.fss', '.build/build-toolchain.sh']) {
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
            const match = output.match(/container: (ghcr\.io\/fscriptlang\/fscript\/toolchain:[^\s]+)/);
            assert.ok(match, 'The build must use the workspace toolchain image.');
            return match[1];
        };
        const original = image();
        process.env.FSCRIPT_PUBLISH_TOOLCHAIN = 'true';
        try {
            assert.equal(image(), original, 'Publishing permission must not change the image hash used by test runners.');
        } finally {
            delete process.env.FSCRIPT_PUBLISH_TOOLCHAIN;
        }
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

test('toolchain builds stay local unless explicitly published, and propagate Docker failures', () => {
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-toolchain-publish-'));
    const script = fileURLToPath(new URL('../../.build/build-toolchain.sh', import.meta.url));
    try {
        const docker = path.join(fixture, 'docker');
        const log = path.join(fixture, 'docker.log');
        fs.writeFileSync(docker, '#!/usr/bin/env bash\nprintf "%s\\n" "$*" >> "$DOCKER_TEST_LOG"\nif [[ "$1" == "${DOCKER_TEST_FAILURE:-}" ]]; then exit 19; fi\n', {mode: 0o755});
        const run = (publish, failure = '') => {
            fs.writeFileSync(log, '');
            return execFileSync('bash', [script, 'ghcr.io/example/toolchain:hash', '11.0.100', '22', 'linux/amd64'], {
                cwd: fixture,
                env: {...process.env, PATH: `${fixture}:${process.env.PATH}`, DOCKER_TEST_LOG: log,
                    FSCRIPT_PUBLISH_TOOLCHAIN: publish, DOCKER_TEST_FAILURE: failure},
            });
        };
        run('false');
        assert.match(fs.readFileSync(log, 'utf8'), /^build .*--build-arg DOTNET_SDK_VERSION=11.0.100 .*--platform linux\/amd64/);
        assert.doesNotMatch(fs.readFileSync(log, 'utf8'), /^push /m);
        run('true');
        assert.match(fs.readFileSync(log, 'utf8'), /^push ghcr.io\/example\/toolchain:hash$/m);
        assert.throws(() => run('true', 'build'), error => error.status === 19);
        assert.doesNotMatch(fs.readFileSync(log, 'utf8'), /^push /m);
        assert.throws(() => run('true', 'push'), error => error.status === 19);
    } finally {
        fs.rmSync(fixture, {recursive: true, force: true});
    }
});
