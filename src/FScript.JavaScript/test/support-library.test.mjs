import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {run, formatValue} from '../dist/Library.js';

// The F# fixture uses JSON-compatible string triples; share all core contract examples.
const fixture = readFileSync(new URL('../../../tests/SupportLibraryCases.fs', import.meta.url), 'utf8');
const core = fixture.split('    let runtime =')[0].split('\n')
  .filter(line => line.trimStart().startsWith('"'))
  .map(line => JSON.parse(`[${line.trim()}]`));
for (const [name, source, expected] of core) {
  test(`pure support: ${name}`, () => assert.equal(formatValue(run(source)), expected));
}

test('pure support: abs rejects int64 minimum', () => {
  assert.throws(() => run('Int.abs (0 - 9223372036854775807 - 1)'));
});
