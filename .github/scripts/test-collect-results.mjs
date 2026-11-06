import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { fileURLToPath } from 'node:url';

test('collection selects current TRX results, retains JUnit, and replaces stale artifacts', () => {
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-report-collection-'));
    try {
        const reports = path.join(fixture, 'tests/Suite/TestResults');
        fs.mkdirSync(reports, { recursive: true });
        fs.writeFileSync(path.join(reports, 'old.trx'), '<TestRun><Times finish="2026-11-05T10:00:00Z"/></TestRun>');
        fs.writeFileSync(path.join(reports, 'current.trx'), '<TestRun><Times finish="2026-11-06T10:00:00Z"/></TestRun>');
        // Cache extraction time must not make an older test result look current.
        fs.utimesSync(path.join(reports, 'current.trx'), 0, 0);
        fs.writeFileSync(path.join(reports, 'node.xml'), '<testsuites/>');
        const destination = path.join(fixture, '.out/test-results');
        fs.mkdirSync(destination, { recursive: true });
        fs.writeFileSync(path.join(destination, 'stale.trx'), 'stale');

        const script = fileURLToPath(new URL('./collect-test-results.mjs', import.meta.url));
        const output = execFileSync(process.execPath, [script], { cwd: fixture, encoding: 'utf8' });
        assert.match(output, /Collected 1 .NET reports and 1 JUnit reports/);
        assert.deepEqual(fs.readdirSync(path.join(destination, 'tests/Suite/TestResults')).sort(), ['current.trx', 'node.xml']);
        assert.equal(fs.existsSync(path.join(destination, 'stale.trx')), false);
        assert.equal(fs.existsSync(path.join(reports, 'old.trx')), true);
    } finally {
        fs.rmSync(fixture, { recursive: true, force: true });
    }
});
