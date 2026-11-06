import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { verifyTestReports } from './verify-test-reports.mjs';

test('reports must contain executed tests, including when restored from cache', () => {
  const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-report-evidence-'));
  try {
    const trx = path.join(fixture, 'results.trx');
    const xml = path.join(fixture, 'results.xml');
    assert.throws(() => verifyTestReports(undefined), /declare.*reports/);
    assert.throws(() => verifyTestReports([trx]), /Missing expected/);
    for (const contents of ['', '<TestRun/>', '<TestRun><Results/><Counters executed="0"/></TestRun>']) {
      fs.writeFileSync(trx, contents);
      assert.throws(() => verifyTestReports([trx]), /no executed tests/);
    }
    fs.writeFileSync(trx, '<TestRun><UnitTestResult outcome="Failed"/><Counters executed="1" failed="1"/></TestRun>');
    assert.throws(() => verifyTestReports([trx]), /contains failures/);
    fs.writeFileSync(trx, '<TestRun><UnitTestResult outcome="Passed"/><Counters executed="1" failed="0" error="0"/></TestRun>');
    assert.doesNotThrow(() => verifyTestReports([trx]));
    for (const contents of ['', '<testsuites/>', '<testsuites><testcase><skipped/></testcase></testsuites>']) {
      fs.writeFileSync(xml, contents);
      assert.throws(() => verifyTestReports([xml]), /no executed tests/);
    }
    fs.writeFileSync(xml, '<testsuites><testcase><failure/></testcase></testsuites>');
    assert.throws(() => verifyTestReports([xml]), /contains failures/);
    fs.writeFileSync(xml, '<testsuites><testcase name="passed"/></testsuites>');
    assert.doesNotThrow(() => verifyTestReports([xml]));
    assert.doesNotThrow(() => verifyTestReports([]), 'Website typechecking has no test-report artifact.');
  } finally {
    fs.rmSync(fixture, { recursive: true, force: true });
  }
});
