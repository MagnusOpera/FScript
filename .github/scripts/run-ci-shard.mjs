import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';

const matrix = JSON.parse(fs.readFileSync('.build/ci-shards.json', 'utf8'));
const shard = matrix.include.find(shard => shard.name === process.argv[2]);
if (!shard) throw new Error(`Unknown CI shard: ${process.argv[2]}`);
let status = 0;
for (const [index, selection] of shard.runs.entries()) {
  const args = [...selection.targets, '--project', ...selection.projects, '--restore', '--debug', '--log'];
  // setup-tools sets this only for a runner without shared-cache credentials.
  if (process.env.TB_FLAGS === '--local-only') args.push('--local-only');
  const destination = path.join('.terrabuild/shards', String(index + 1));
  fs.rmSync(destination, { recursive: true, force: true });
  // Do not attribute diagnostics from an earlier selection to a failed startup.
  const diagnostics = ['.terrabuild/result.json', '.terrabuild/run.log', 'terrabuild-debug.json', 'terrabuild-debug.log'];
  for (const file of diagnostics) fs.rmSync(file, { force: true });
  const result = spawnSync('bash', ['.build/run-terrabuild.sh', ...args], { stdio: 'inherit' });
  fs.mkdirSync(destination, { recursive: true });
  for (const file of diagnostics) {
    if (fs.existsSync(file)) fs.copyFileSync(file, path.join(destination, path.basename(file)));
  }
  if (result.error) throw result.error;
  // Run every selection so a failed suite does not hide another suite's results.
  if (result.status !== 0) status = result.status ?? 1;
}
process.exit(status);
