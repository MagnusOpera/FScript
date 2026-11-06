import { spawnSync } from 'node:child_process';
import fs from 'node:fs';

const matrix = JSON.parse(fs.readFileSync('.build/ci-shards.json', 'utf8'));
const shard = matrix.include.find(shard => shard.name === process.argv[2]);
if (!shard) throw new Error(`Unknown CI shard: ${process.argv[2]}`);
const args = ['ci', '--project', ...shard.projects, '--debug', '--log'];
// setup-tools sets this only for a runner without shared-cache credentials.
if (process.env.TB_FLAGS === '--local-only') args.push('--local-only');
const result = spawnSync('bash', ['.build/run-terrabuild.sh', ...args], { stdio: 'inherit' });
if (result.error) throw result.error;
process.exit(result.status ?? 1);
