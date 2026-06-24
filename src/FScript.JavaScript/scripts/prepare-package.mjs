import {rm} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import {dirname, join} from 'node:path';

const root = dirname(dirname(fileURLToPath(import.meta.url)));

await rm(join(root, 'dist', 'fable_modules', '.gitignore'), {force: true});
