import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';

test('toolchain publishing, pulling, local fallback, and registry failures', () => {
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-toolchain-image-'));
    try {
        fs.mkdirSync(path.join(fixture, 'bin'));
        const calls = path.join(fixture, 'calls');
        fs.writeFileSync(path.join(fixture, 'bin/docker'), `#!/usr/bin/env bash
printf '%s\\n' "$*" >> "$DOCKER_FIXTURE_CALLS"
case "$1" in
  image) exit "\${DOCKER_FIXTURE_LOCAL:-1}" ;;
  pull) exit "\${DOCKER_FIXTURE_PULL:-1}" ;;
  push) exit "\${DOCKER_FIXTURE_PUSH:-0}" ;;
  build) exit "\${DOCKER_FIXTURE_BUILD:-0}" ;;
  *) exit 2 ;;
esac
`, { mode: 0o755 });
        const script = fileURLToPath(new URL('../../.build/toolchain/image.sh', import.meta.url));
        const image = 'ghcr.io/example/toolchain:content-sdk10-node22-amd64';
        function run(mode, extra = {}) {
            fs.writeFileSync(calls, '');
            const result = spawnSync('bash', [script, image, '10.0.403', '22', 'linux/amd64'], {
                cwd: fixture, encoding: 'utf8',
                env: { ...process.env, PATH: `${fixture}/bin:${process.env.PATH}`, DOCKER_FIXTURE_CALLS: calls,
                    FSCRIPT_TOOLCHAIN_MODE: mode, ...extra }
            });
            return { ...result, calls: fs.readFileSync(calls, 'utf8').trim().split('\n') };
        }
        const published = run('publish');
        assert.equal(published.status, 0, published.stderr);
        assert.deepEqual(published.calls, [
            `image inspect ${image}`, `pull ${image}`,
            `build --platform linux/amd64 --build-arg DOTNET_SDK_VERSION=10.0.403 --build-arg NODE_VERSION=22 --tag ${image} .`,
            `push ${image}`
        ]);
        const reused = run('publish', { DOCKER_FIXTURE_PULL: '0' });
        assert.equal(reused.status, 0);
        assert.deepEqual(reused.calls, [`image inspect ${image}`, `pull ${image}`]);
        const pulled = run('pull', { DOCKER_FIXTURE_PULL: '0' });
        assert.equal(pulled.status, 0);
        assert.deepEqual(pulled.calls, reused.calls);
        const missing = run('pull');
        assert.notEqual(missing.status, 0);
        assert.deepEqual(missing.calls, reused.calls, 'A test runner must never rebuild or push a missing image.');
        const local = run('local');
        assert.equal(local.status, 0);
        assert.ok(!local.calls.some(command => command.startsWith('push ') || command.startsWith('pull ')));
        const alreadyLocal = run('local', { DOCKER_FIXTURE_LOCAL: '0' });
        assert.equal(alreadyLocal.status, 0);
        assert.deepEqual(alreadyLocal.calls, [`image inspect ${image}`]);
        assert.equal(run('publish', { DOCKER_FIXTURE_BUILD: '7' }).status, 7);
        assert.equal(run('publish', { DOCKER_FIXTURE_PUSH: '8' }).status, 8);
        assert.notEqual(run('unknown').status, 0);
    } finally {
        fs.rmSync(fixture, { recursive: true, force: true });
    }
});
