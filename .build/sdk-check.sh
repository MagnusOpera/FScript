#!/usr/bin/env bash
# Publishing different assembly versions must not mutate outputs used by tests.
set -euo pipefail
cd "$(dirname "$0")/.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
node - "$work" <<'JS'
const fs = require('node:fs');
const path = require('node:path');
const {execFileSync} = require('node:child_process');
const destination = process.argv[2];
const files = execFileSync('git', ['ls-files', '--cached', '--others', '--exclude-standard', '-z'], {maxBuffer: 16 * 1024 * 1024}).toString().split('\0').filter(Boolean);
for (const file of new Set(files)) {
    if (!fs.existsSync(file) || !fs.statSync(file).isFile()) continue;
    const output = path.join(destination, file);
    fs.mkdirSync(path.dirname(output), {recursive: true});
    fs.copyFileSync(file, output);
}
JS
bash "$work/.github/scripts/verify-sdk-provider.sh"
