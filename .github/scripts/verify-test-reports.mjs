import fs from 'node:fs';

// Check execution evidence as well as file presence. A successful dotnet process
// can have run no tests when package-provided MSBuild imports are missing.
export function verifyTestReports(reports) {
  if (!Array.isArray(reports)) throw new Error('Each CI selection must declare its expected reports.');
  for (const report of reports) {
    if (!fs.existsSync(report)) throw new Error(`Missing expected test report: ${report}`);
    const xml = fs.readFileSync(report, 'utf8');
    if (report.endsWith('.trx')) {
      const counters = xml.match(/<Counters\b[^>]*>/)?.[0];
      const executed = Number(counters?.match(/\bexecuted="(\d+)"/)?.[1] ?? 0);
      const failed = Number(counters?.match(/\bfailed="(\d+)"/)?.[1] ?? 0);
      const errors = Number(counters?.match(/\berror="(\d+)"/)?.[1] ?? 0);
      if (!/<TestRun\b/.test(xml) || !xml.includes('</TestRun>') ||
          !/<UnitTestResult\b/.test(xml) || executed < 1) {
        throw new Error(`Test report has no executed tests: ${report}`);
      }
      if (failed > 0 || errors > 0) throw new Error(`Test report contains failures: ${report}`);
    } else if (report.endsWith('.xml')) {
      const cases = [...xml.matchAll(/<testcase\b/g)].length;
      const skipped = [...xml.matchAll(/<skipped\b/g)].length;
      if (!/<testsuites?\b/.test(xml) || !/<\/testsuites?>/.test(xml) || cases <= skipped) {
        throw new Error(`Test report has no executed tests: ${report}`);
      }
      if (/<(?:failure|error)\b/.test(xml)) throw new Error(`Test report contains failures: ${report}`);
    } else {
      throw new Error(`Unsupported test report format: ${report}`);
    }
    console.log(`Verified test report: ${report}`);
  }
}
