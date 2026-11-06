import fs from 'node:fs';
import { pathToFileURL } from 'node:url';

export function readToolVersions(source) {
    const blocks = [...source.matchAll(/\bversions\s*=\s*\{([^}]*)\}/g)];
    if (blocks.length !== 1) throw new Error('WORKSPACE must define one locals.versions block.');
    const versions = {};
    for (const line of blocks[0][1].split('\n')) {
        if (/^\s*(?:#.*)?$/.test(line)) continue;
        const match = line.match(/^\s*([a-z][a-z0-9_]*)\s*:\s*"([a-zA-Z0-9][a-zA-Z0-9.+_-]*)"\s*(?:#.*)?$/);
        if (!match) throw new Error(`Tool versions must be quoted literal values: ${line.trim()}`);
        const [, name, value] = match;
        if (Object.hasOwn(versions, name)) throw new Error(`Duplicate tool version: ${name}`);
        versions[name] = value;
    }
    for (const name of ['dotnet_sdk', 'node', 'terrabuild', 'vsce', 'ovsx']) {
        if (!versions[name]) throw new Error(`Missing tool version: ${name}`);
    }
    return versions;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
    const versions = readToolVersions(fs.readFileSync('WORKSPACE', 'utf8'));
    for (const [name, value] of Object.entries(versions)) console.log(`${name}=${value}`);
}
