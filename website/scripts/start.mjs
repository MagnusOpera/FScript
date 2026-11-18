import {spawn} from 'node:child_process';
import {watch, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {assembleDocs} from './assemble-docs.mjs';

export function watchDocumentation(root, refresh, watchSource = watch) {
  let timer;
  let pending = Promise.resolve();
  const schedule = () => {
    clearTimeout(timer);
    timer = setTimeout(() => {
      pending = pending.then(refresh).catch((error) => console.error('Could not refresh Next documentation:', error));
    }, 100);
  };
  const watchers = [
    watchSource(path.join(root, 'website/docs'), {recursive: true}, schedule),
    watchSource(path.join(root, 'website'), (_, name) => {if (String(name) === 'sidebars.ts') schedule();}),
    watchSource(root, (_, name) => {if (String(name) === 'CHANGELOG.md') schedule();}),
  ];
  return () => {
    clearTimeout(timer);
    watchers.forEach((watcher) => watcher.close());
  };
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
  const refresh = () => assembleDocs({root,
    releases: process.env.FSCRIPT_WEBSITE_RELEASES ? JSON.parse(readFileSync(process.env.FSCRIPT_WEBSITE_RELEASES, 'utf8')) : undefined});
  const stop = process.env.FSCRIPT_WEBSITE_NEXT_REF ? () => {} : watchDocumentation(root, refresh);
  const server = spawn(process.execPath, [fileURLToPath(new URL('../node_modules/@docusaurus/core/bin/docusaurus.mjs', import.meta.url)), 'start', ...process.argv.slice(2)], {stdio: 'inherit'});
  for (const signal of ['SIGINT', 'SIGTERM']) process.on(signal, () => {stop(); server.kill(signal);});
  server.on('error', (error) => {stop(); console.error(error); process.exitCode = 1;});
  server.on('exit', (code) => {stop(); process.exitCode = code ?? 1;});
}
