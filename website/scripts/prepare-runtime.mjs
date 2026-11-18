import {execFileSync} from 'node:child_process';
import {existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {resolveCommit} from './assemble-docs.mjs';

// The website build code can come from a release tag while Next's interpreter
// must come from the same main revision as its documentation.
export function prepareRuntime({root, ref, run = execFileSync}) {
  let temporary;
  let source = root;
  const output = path.join(root, 'website/src/generated/fscript');
  try {
    if (ref) {
      const manifestPath = path.join(root, 'website/.generated/static/website-sources.json');
      const next = existsSync(manifestPath) ? JSON.parse(readFileSync(manifestPath, 'utf8')).next : undefined;
      const commit = next?.ref === ref && next.commit ? next.commit : resolveCommit(root, ref);
      temporary = mkdtempSync(path.join(os.tmpdir(), 'fscript-website-runtime-'));
      const archive = path.join(temporary, 'source.tar');
      source = path.join(temporary, 'source');
      mkdirSync(source);
      execFileSync('git', ['-C', root, 'archive', '--format=tar', `--output=${archive}`, commit]);
      execFileSync('tar', ['-xf', archive, '-C', source]);
    }
    rmSync(output, {recursive: true, force: true});
    run('dotnet', ['tool', 'restore'], {cwd: source, stdio: 'inherit'});
    run('dotnet', ['fable', 'src/FScript.JavaScript/FScript.JavaScript.fsproj', '--outDir', output, '--sourceMaps', 'false'], {cwd: source, stdio: 'inherit'});
  } finally {
    if (temporary) rmSync(temporary, {recursive: true, force: true});
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  prepareRuntime({root: path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..'), ref: process.env.FSCRIPT_WEBSITE_NEXT_REF});
}
