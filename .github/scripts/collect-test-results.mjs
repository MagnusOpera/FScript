import fs from 'node:fs';
import path from 'node:path';

const root = process.cwd();
const destination = path.join(root, '.out/test-results');
const latestTrx = new Map();
const reports = [];
const ignored = new Set(['bin', 'obj', 'node_modules', '.git', '.fscript']);

function visit(directory, inResults = false) {
    if (!fs.existsSync(directory)) return;
    for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
        const file = path.join(directory, entry.name);
        if (entry.isDirectory() && !ignored.has(entry.name)) {
            visit(file, inResults || entry.name === 'TestResults');
        } else if (entry.isFile() && inResults) {
            if (entry.name.endsWith('.xml')) reports.push(file);
            if (!entry.name.endsWith('.trx')) continue;
            const finish = fs.readFileSync(file, 'utf8').match(/<Times\b[^>]*\bfinish="([^"]+)"/);
            const parsed = finish ? Date.parse(finish[1]) : NaN;
            const timestamp = Number.isFinite(parsed) ? parsed : fs.statSync(file).mtimeMs;
            const previous = latestTrx.get(directory);
            if (!previous || timestamp > previous.timestamp) {
                latestTrx.set(directory, { file, timestamp });
            }
        }
    }
}

for (const directory of ['src', 'tests', 'samples', 'vscode-fscript']) {
    visit(path.join(root, directory));
}
reports.push(...Array.from(latestTrx.values(), result => result.file));
fs.rmSync(destination, { recursive: true, force: true });
for (const file of reports) {
    const output = path.join(destination, path.relative(root, file));
    fs.mkdirSync(path.dirname(output), { recursive: true });
    fs.copyFileSync(file, output);
}
console.log(`Collected ${latestTrx.size} .NET reports and ${reports.length - latestTrx.size} JUnit reports.`);
