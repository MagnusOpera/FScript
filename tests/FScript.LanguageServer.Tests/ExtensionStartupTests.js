const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { test } = require('node:test');

const extensionPath = path.resolve(__dirname, '../../vscode-fscript');

function loadExtension({ initialize = async () => {}, enabled = true, folders, candidates = [], trusted = true, settings = {}, development = false, debugCli = false, cliPaths = {}, documents = [], environmentInfo, buildDotnet = async () => {} } = {}) {
  const disposable = () => ({ dispose() {} });
  const errors = [];
  const clients = [];
  const channels = [];
  const status = { ...disposable(), show() {}, hide() {} };
  let configurationChanged;
  let grantTrust;
  let changeFolders;
  let documentOpened;
  let activeEditorChanged;
  const watchers = [];
  const commands = new Map();
  const information = [];
  const searches = [];
  const processCalls = [];
  const processCommands = [];
  const debugProviders = [];
  const debugFactories = [];
  const debugLaunches = [];
  let choice;

  const selections = new Map();
  const uri = file => ({ fsPath: file, scheme: 'file', toString: () => `file://${file}` });
  const rootFolders = folders?.map(file => ({ name: path.basename(file), uri: uri(file) }));
  const workspaceFolder = value => rootFolders?.filter(folder => value?.fsPath === folder.uri.fsPath || value?.fsPath?.startsWith(`${folder.uri.fsPath}/`)).sort((a,b) => b.uri.fsPath.length-a.uri.fsPath.length)[0];
  const vscode = {
    debug: {
      async startDebugging(folder,configuration) { debugLaunches.push({folder,configuration}); return true; },
      registerDebugConfigurationProvider(type, provider) { debugProviders.push({type,provider}); return disposable(); },
      registerDebugAdapterDescriptorFactory(type, factory) { debugFactories.push({type,factory}); return disposable(); }
    },
    DebugAdapterExecutable: class { constructor(command,args) { this.command = command; this.args = args; } },
    ExtensionMode: { Production: 1, Development: 2, Test: 3 },
    StatusBarAlignment: { Left: 1 },
    Uri: { file: uri },
    RelativePattern: class { constructor(base, pattern) { this.base = base; this.pattern = pattern; } },
    window: {
      createOutputChannel(name, options) {
        const channel = { ...disposable(), name, show() {} };
        // The plain OutputChannel has no logging API. Only { log: true }
        // supplies the API required by vscode-languageclient 10.
        if (options?.log) {
          Object.assign(channel, {
            logLevel: 3,
            onDidChangeLogLevel: disposable,
            trace() {}, debug() {}, info() {}, warn() {}, error() {}
          });
        }
        channels.push(channel);
        return channel;
      },
      createStatusBarItem: () => status,
      showWorkspaceFolderPick: async () => rootFolders?.[0],
      showQuickPick: async items => items.find(item => item.label === choice),
      showInformationMessage: async message => { information.push(message); },
      showErrorMessage: message => errors.push(message),
      showWarningMessage: message => errors.push(message)
    },
    workspace: {
      getConfiguration: (section, resource) => ({ get: (key, fallback) => key === 'cli.path' ? cliPaths[resource?.fsPath] || '' : key === 'lsp.enabled' ? enabled : (section === 'fscript' && key === 'config' ? settings[resource?.fsPath] || '' : fallback) }),
      workspaceFolders: rootFolders,
      getWorkspaceFolder: workspaceFolder,
      isTrusted: trusted,
      createFileSystemWatcher(pattern) {
        const callbacks = {};
        const watcher = { pattern, disposed: false, dispose() { this.disposed = true; } };
        for (const kind of ['Create', 'Change', 'Delete']) watcher[`onDid${kind}`] = callback => { callbacks[kind] = callback; return disposable(); };
        watcher.fire = (kind, file) => callbacks[kind]?.(uri(file));
        watchers.push(watcher);
        return watcher;
      },
      registerTextDocumentContentProvider: disposable,
      onDidChangeTextDocument: disposable,
      textDocuments: documents,
      onDidOpenTextDocument(callback) { documentOpened = callback; return disposable(); },
      onDidChangeConfiguration(callback) {
        configurationChanged = callback;
        return disposable();
      }
    },
    languages: { onDidChangeDiagnostics: disposable },
    commands: {
      registerCommand: (name, callback) => { commands.set(name, callback); return disposable(); },
      executeCommand: async () => ({ dotnetPath: 'dotnet' })
    }
  };
  if (folders) {
    vscode.workspace.findFiles = async (include, exclude) => { searches.push({ include, exclude }); return candidates.map(uri); };
    vscode.workspace.onDidGrantWorkspaceTrust = callback => { grantTrust = callback; return disposable(); };
    vscode.workspace.onDidChangeWorkspaceFolders = callback => { changeFolders = callback; return disposable(); };
    vscode.window.onDidChangeActiveTextEditor = callback => { activeEditorChanged = callback; return disposable(); };
  }
  class LanguageClient {
    constructor(id, name, serverOptions, clientOptions) {
      this.options = clientOptions;
      this.stopped = false;
      clients.push(this);
    }
    async start() {
      // Match the logging contract exercised during real client startup.
      this.options.outputChannel.onDidChangeLogLevel(() => {});
      this.options.outputChannel.info('Starting language server');
      await initialize();
    }
    async sendRequest() { return environmentInfo ? environmentInfo(this.options.initializationOptions) : { externAssemblies: ['/extensions/provider.dll'] }; }
    async stop() { this.stopped = true; }
  }
  const sandbox = {
    module: { exports: {} },
    require(id) {
      if (id === './cli-command') return require(path.join(extensionPath, 'cli-command.js'));
      if (id === './debug-launch') return require(path.join(extensionPath, 'debug-launch.js'));
      if (id === './script-launch') return require(path.join(extensionPath, 'script-launch.js'));
      if (id === './config-selection') return require(path.join(extensionPath, 'config-selection.js'));
      if (id === 'vscode') return vscode;
      if (id === 'vscode-languageclient/node') return { LanguageClient, TransportKind: { stdio: 0 } };
      if (id === 'fs') return { ...fs, existsSync: file => !development || !file.includes('/server/') };
      if (id === 'child_process' && (development || debugCli)) return {
        execFile(command, args, options, callback) {
          processCalls.push(args);
          processCommands.push(command);
          if (args.at(-1) === '--debug-capabilities') { callback(null, '{"protocolVersion":1}'); return; }
          (args[0] === '--list-sdks' ? Promise.resolve() : buildDotnet()).then(() => callback(null, '10.0.401', ''));
        }
      };
      return require(id);
    },
    setTimeout, clearTimeout
  };
  vm.runInNewContext(fs.readFileSync(path.join(extensionPath, 'extension.js'), 'utf8'), sandbox);
  const context = {
    extensionPath,
    extensionMode: development ? 2 : undefined,
    extension: { packageJSON: { publisher: 'MagnusOpera', name: 'fscript' } },
    workspaceState: { get: key => selections.get(key), update: async (key, value) => selections.set(key, value) },
    subscriptions: []
  };
  return {
    extension: sandbox.module.exports, context, errors, clients, channels, status,
    watchers, vscode, selections, commands, information, searches, processCalls, processCommands, debugProviders, debugFactories, debugLaunches,
    replLaunch: () => sandbox.resolveReplLaunchCommand(),
    pick: label => { choice = label; },
    grantTrust: () => { vscode.workspace.isTrusted = true; grantTrust(); },
    changeFolders: async () => changeFolders({}),
    open: async file => {
      const document = { languageId: 'fscript', uri: uri(file), isDirty: true, getText: () => 'let unsaved = 42', async save() { this.isDirty = false; return true; } };
      documents.push(document);
      vscode.window.activeTextEditor = { document };
      documentOpened(document);
      activeEditorChanged?.({ document });
      await pause();
      return document;
    },
    restart: () => configurationChanged({ affectsConfiguration: () => true })
  };
}

test('activation uses a logging channel and registers only disposables', async () => {
  const harness = loadExtension();
  await harness.extension.activate(harness.context);
  assert.deepEqual(harness.errors, []);
  assert.equal(harness.channels.length, 1);
  assert.equal(harness.clients[0].options.outputChannel, harness.channels[0]);
  assert.equal(harness.status.text, '$(check) FScript');
  for (const subscription of harness.context.subscriptions) {
    assert.equal(typeof subscription.dispose, 'function');
  }
  await harness.extension.deactivate();
  assert.equal(harness.clients[0].stopped, true);
});

test('activation stays pending and shows starting until initialization completes', async () => {
  let complete;
  const initialization = new Promise(resolve => { complete = resolve; });
  const harness = loadExtension({ initialize: () => initialization });
  let activated = false;
  const activation = harness.extension.activate(harness.context).then(() => { activated = true; });
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(harness.clients.length, 1);
  assert.equal(activated, false);
  assert.equal(harness.status.text, '$(sync~spin) FScript: starting...');
  complete();
  await activation;
  assert.equal(harness.status.text, '$(check) FScript');
  await harness.extension.deactivate();
});

test('asynchronous startup errors are reported and do not show ready', async () => {
  const harness = loadExtension({ initialize: async () => {
    await Promise.resolve();
    throw new Error('Initialization failed');
  } });
  await harness.extension.activate(harness.context);
  assert.deepEqual(harness.errors, ['FScript extension: failed to start language server. Initialization failed']);
  assert.equal(harness.status.text, '$(error) FScript');
  await harness.extension.deactivate();
});

test('configuration restarts stop the previous client and reuse the logging channel', async () => {
  const harness = loadExtension();
  await harness.extension.activate(harness.context);
  await harness.restart();
  assert.equal(harness.clients.length, 2);
  assert.equal(harness.clients[0].stopped, true);
  assert.equal(harness.clients[1].options.outputChannel, harness.channels[0]);
  assert.equal(harness.channels.length, 1);
  assert.deepEqual(harness.errors, []);
  await harness.extension.deactivate();
  assert.equal(harness.clients[1].stopped, true);
});

test('disabled LSP does not start a language client', async () => {
  const harness = loadExtension({ enabled: false });
  await harness.extension.activate(harness.context);
  assert.equal(harness.clients.length, 0);
  assert.equal(harness.status.text, '$(circle-slash) FScript');
  await harness.extension.deactivate();
});

const pause = () => new Promise(resolve => setTimeout(resolve, 350));

test('workspace clients isolate configurations and nested document ownership', async () => {
  const h = loadExtension({ folders: ['/one', '/one/nested', '/two'], candidates: ['/one/fscript.toml', '/two/fscript.toml'] });
  await h.extension.activate(h.context);
  assert.equal(h.clients.length, 4);
  const options = h.clients.map(c => c.options.initializationOptions);
  assert.equal(options[0].configurationPath, '/one/fscript.toml');
  assert.equal(options[1].configurationPath, undefined);
  assert.equal(options[2].configurationPath, '/two/fscript.toml');
  const document = { uri: { fsPath: '/one/nested/script.fss' } };
  let called = 0;
  h.clients[0].options.middleware.didOpen(document, () => called++);
  h.clients[1].options.middleware.didOpen(document, () => called++);
  h.clients[3].options.middleware.didOpen(document, () => called++);
  assert.equal(called, 1);
  await h.extension.deactivate();
});

test('manifest and assembly events debounce and restart only their owning folder', async () => {
  const h = loadExtension({ folders: ['/one', '/two'], candidates: ['/one/fscript.toml'] });
  await h.extension.activate(h.context);
  const watcher = h.watchers.find(w => w.pattern?.pattern === 'fscript.toml');
  watcher.fire('Change', '/one/fscript.toml');
  watcher.fire('Change', '/one/fscript.toml');
  await pause();
  assert.equal(h.clients.length, 4);
  assert.equal(h.clients[0].stopped, true);
  assert.equal(h.clients[1].stopped, false);
  assert.equal(h.clients[2].stopped, false);
  const dllWatcher = h.watchers.filter(w => !w.disposed).find(w => w.pattern?.pattern === 'provider.dll');
  dllWatcher.fire('Change', '/extensions/provider.dll');
  await pause();
  assert.equal(h.clients.length, 5);
  await h.extension.deactivate();
});

test('untrusted clients pass a blocking trust state and restart when trust is granted', async () => {
  const h = loadExtension({ folders: ['/one'], candidates: ['/one/fscript.toml'], trusted: false });
  await h.extension.activate(h.context);
  assert.equal(h.clients[0].options.initializationOptions.workspaceTrusted, false);
  h.grantTrust();
  await pause();
  assert.equal(h.clients[2].options.initializationOptions.workspaceTrusted, true);
  await h.extension.deactivate();
});


test('picker persists explicit none and the automatic choice restores discovery', async () => {
  const h = loadExtension({ folders: ['/one'], candidates: ['/one/fscript.toml'] });
  await h.extension.activate(h.context);
  h.pick('No configuration');
  await h.commands.get('fscript.selectConfiguration')();
  await pause();
  assert.equal([...h.selections.values()][0].kind, 'none');
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath, undefined);
  h.pick('Automatic selection');
  await h.commands.get('fscript.selectConfiguration')();
  await pause();
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath, '/one/fscript.toml');
  assert.match(h.searches[0].exclude, /node_modules/);
  assert.match(h.searches[0].exclude, /\.fscript/);
  await h.extension.deactivate();
});

test('an explicit setting overrides persistence and picker explains the effective choice', async () => {
  const h = loadExtension({ folders: ['/one'], candidates: ['/one/fscript.toml'], settings: { '/one': './tools/missing.toml' } });
  h.selections.set('fscript.environment:file:///one', {kind: 'none'});
  await h.extension.activate(h.context);
  assert.equal(h.clients[0].options.initializationOptions.configurationPath, '/one/tools/missing.toml');
  await h.commands.get('fscript.selectConfiguration')();
  assert.equal(h.information.length, 1);
  assert.equal([...h.selections.values()][0].kind, 'none');
  await h.extension.deactivate();
});

test('manifest creation and deletion recompute automatic selection', async () => {
  const candidates = [];
  const h = loadExtension({ folders: ['/one'], candidates });
  await h.extension.activate(h.context);
  const discovery = h.watchers.find(w => w.pattern === '**/fscript.toml');
  await h.open('/one/nested/main.fss');
  candidates.push('/one/nested/fscript.toml');
  discovery.fire('Create', candidates[0]);
  await pause();
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath, candidates[0]);
  candidates.length = 0;
  discovery.fire('Delete', '/one/nested/fscript.toml');
  await pause();
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath, undefined);
  await h.extension.deactivate();
});

test('folder removal stops old clients and disposes their watchers', async () => {
  const h = loadExtension({ folders: ['/one', '/two'] });
  await h.extension.activate(h.context);
  h.vscode.workspace.workspaceFolders.splice(0, 1);
  await h.changeFolders();
  assert.ok(h.clients.slice(0,3).every(client => client.stopped));
  assert.equal(h.clients.length, 5);
  await h.extension.deactivate();
  assert.ok(h.clients.every(client => client.stopped));
});


test('development build is asynchronous and shared across workspace clients', async () => {
  let finish;
  const building = new Promise(resolve => { finish = resolve; });
  const h = loadExtension({ development: true, folders: ['/one', '/two'], buildDotnet: () => building });
  let activated = false;
  const activation = h.extension.activate(h.context).then(() => { activated = true; });
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(activated, false);
  assert.equal(h.clients.length, 0);
  assert.equal(h.processCalls.filter(args => args[0] === 'build').length, 1);
  finish();
  await activation;
  assert.equal(h.clients.length, 3);
  assert.equal(h.processCalls.filter(args => args[0] === 'build').length, 1);
  await h.extension.deactivate();
});

test('folder-resolved language-server settings declare resource scope', () => {
  const manifest = JSON.parse(fs.readFileSync(path.join(extensionPath, 'package.json'), 'utf8'));
  for (const name of ['lsp.enabled', 'inlayHints.enabled', 'hoverHints.enabled', 'server.path', 'server.logLevel', 'config']) {
    assert.equal(manifest.contributes.configuration.properties[`fscript.${name}`].scope, 'resource');
  }
});

test('run script forwards the owning folder configuration through process arguments', async () => {
  const harness = loadExtension({ folders: ['/work', '/work/nested'], enabled: false,
    settings: { '/work/nested': 'ffi/fscript.toml' } });
  await harness.extension.activate(harness.context);
  let saved = false;
  let executed;
  harness.vscode.window.activeTextEditor = { document: {
    languageId: 'fscript', isDirty: true,
    uri: { scheme: 'file', fsPath: '/work/nested/teapot main.fss', toString: () => 'file:///work/nested/teapot main.fss' },
    save: async () => { saved = true; return true; }
  } };
  harness.vscode.TaskScope = { Workspace: 1 };
  harness.vscode.TaskRevealKind = { Always: 1 };
  harness.vscode.TaskPanelKind = { New: 1 };
  harness.vscode.ProcessExecution = class { constructor(command, args, options) { Object.assign(this, { command, args, options }); } };
  harness.vscode.Task = class { constructor(definition, scope, name, source, execution) { Object.assign(this, { scope, execution }); } };
  harness.vscode.tasks = { executeTask: async task => { executed = task; } };
  await harness.commands.get('fscript.runScript')();
  assert.equal(saved, true);
  // existsSync reports that the repository CLI project exists: it must not select dotnet run.
  assert.equal(executed.execution.command, 'fscript');
  assert.deepEqual(Array.from(executed.execution.args), ['--config', '/work/nested/ffi/fscript.toml', '/work/nested/teapot main.fss']);
  assert.equal(executed.scope.uri.fsPath, '/work/nested');
  assert.ok(executed.execution.args.includes('/work/nested/ffi/fscript.toml'));
  assert.equal(executed.execution.args.at(-1), '/work/nested/teapot main.fss');
  assert.equal(executed.execution.options.cwd, '/work/nested');
  harness.extension.deactivate();
});

test('run script is blocked in untrusted workspaces', async () => {
  const harness = loadExtension({ trusted: false });
  await harness.extension.activate(harness.context);
  await harness.commands.get('fscript.runScript')();
  assert.ok(harness.errors.some(message => message.includes('trust this workspace')));
  harness.extension.deactivate();
});

test('automatic REPL launch preserves the default environment without CLI discovery', () => {
  assert.equal(loadExtension().replLaunch(), 'fscript --no-config');
  assert.ok(loadExtension({ folders: ['/work'] }).replLaunch().endsWith('-- --no-config'));
});


test('F5 resolves nested folder configuration, saves sources and starts installed CLI adapter', async () => {
  const harness = loadExtension({ folders: ['/workspace', '/workspace/nested'], settings: { '/workspace/nested': 'config/fscript.toml' }, debugCli: true });
  await harness.extension.activate(harness.context);
  let saved = false;
  harness.vscode.workspace.textDocuments = [{ isDirty: true, languageId: 'fscript', uri: harness.vscode.Uri.file('/workspace/nested/main.fss'), save: async () => { saved = true; return true; } }];
  const config = await harness.debugProviders[0].provider.resolveDebugConfigurationWithSubstitutedVariables(undefined, {type:'fscript',request:'launch',program:'/workspace/nested/main.fss'});
  assert.equal(saved,true);
  assert.equal(config.configurationPath,'/workspace/nested/config/fscript.toml');
  assert.equal(config.rootDirectory,'/workspace/nested');
  const executable = harness.debugFactories[0].factory.createDebugAdapterDescriptor();
  assert.equal(executable.command,'fscript');
  assert.deepEqual(Array.from(executable.args),['--debug-adapter']);
  await harness.extension.deactivate();
});

test('F5 refuses execution in Restricted Mode before probing CLI or providers', async () => {
  const harness = loadExtension({ trusted: false, debugCli: true });
  await harness.extension.activate(harness.context);
  const result = await harness.debugProviders[0].provider.resolveDebugConfigurationWithSubstitutedVariables(undefined,{request:'launch',program:'/workspace/main.fss'});
  assert.equal(result,undefined);
  assert.match(harness.errors.at(-1),/trust this workspace/);
  assert.equal(harness.processCalls.length,0);
  await harness.extension.deactivate();
});

test('F5 probes and launches the CLI selected for the owning nested folder', async () => {
  const program = '/workspace/nested/main.fss';
  const harness = loadExtension({ folders: ['/workspace', '/workspace/nested'], debugCli: true, cliPaths: { [program]: './bin/fscript preview.dll' } });
  await harness.extension.activate(harness.context);
  harness.vscode.workspace.textDocuments = [];
  const config = await harness.debugProviders[0].provider.resolveDebugConfigurationWithSubstitutedVariables(undefined, {type:'fscript',request:'launch',program});
  assert.ok(config);
  assert.equal(harness.processCommands.at(-1), 'dotnet');
  assert.deepEqual(Array.from(harness.processCalls.at(-1)), ['/workspace/nested/bin/fscript preview.dll', '--debug-capabilities']);
  const executable = harness.debugFactories[0].factory.createDebugAdapterDescriptor({configuration: config});
  assert.equal(executable.command,'dotnet');
  assert.deepEqual(Array.from(executable.args), ['/workspace/nested/bin/fscript preview.dll', '--debug-adapter']);
  await harness.extension.deactivate();
});

test('debug adapter factory also refuses execution in Restricted Mode', async () => {
  const harness = loadExtension({ trusted: false, debugCli: true });
  await harness.extension.activate(harness.context);
  assert.throws(() => harness.debugFactories[0].factory.createDebugAdapterDescriptor(), /Trust this workspace/);
  assert.equal(harness.processCalls.length, 0);
  await harness.extension.deactivate();
});


test('Debug Current Script launches the owning folder through the debug configuration provider', async () => {
  for (const folders of [['/workspace', '/workspace/nested'], undefined]) {
    const harness = loadExtension({ folders, enabled: false });
    await harness.extension.activate(harness.context);
    harness.vscode.window.activeTextEditor = { document: { languageId: 'fscript', uri: harness.vscode.Uri.file('/workspace/nested/main.fss') } };
    assert.equal(await harness.commands.get('fscript.debugScript')(), true);
    const launch = harness.debugLaunches[0];
    assert.equal(launch.folder?.uri.fsPath, folders ? '/workspace/nested' : undefined);
    assert.equal(launch.configuration.type, 'fscript');
    assert.equal(launch.configuration.request, 'launch');
    assert.equal(launch.configuration.program, '/workspace/nested/main.fss');
    await harness.extension.deactivate();
  }
});

test('Debug Current Script refuses untrusted workspaces and unsaved documents', async () => {
  for (const trusted of [false, true]) {
    const harness = loadExtension({ trusted, enabled: false });
    await harness.extension.activate(harness.context);
    harness.vscode.window.activeTextEditor = { document: { languageId: 'fscript', uri: {scheme: 'untitled'} } };
    await harness.commands.get('fscript.debugScript')();
    assert.equal(harness.debugLaunches.length, 0);
    assert.match(harness.errors.at(-1), trusted ? /save the script/i : /trust this workspace/i);
    await harness.extension.deactivate();
  }
});


test('umbrella workspace automatically isolates nearby Numerics and Graphics environments', async () => {
  const paths=['/umbrella/product/samples/ffi/fscript.toml','/umbrella/numerics/examples/fscript.toml'];
  const h=loadExtension({folders:['/umbrella'],candidates:paths});
  await h.extension.activate(h.context);
  assert.equal(h.clients.length,2,'unopened nested manifests must not execute providers');
  const numerical=await h.open('/umbrella/numerics/examples/kmeans.fss');
  const graphics=await h.open('/umbrella/product/samples/ffi/main.fss');
  const plain=await h.open('/umbrella/plain.fss');
  const owner=document=>h.clients.filter(c=>!c.stopped).filter(c=>{
    let opened=false;c.options.middleware.didOpen(document,()=>{opened=true;});return opened;
  });
  assert.equal(owner(numerical).length,1);
  assert.equal(owner(numerical)[0].options.initializationOptions.configurationPath,paths[1]);
  assert.equal(owner(graphics).length,1);
  assert.equal(owner(graphics)[0].options.initializationOptions.configurationPath,paths[0]);
  assert.equal(owner(plain).length,1);
  assert.equal(owner(plain)[0].options.initializationOptions.configurationPath,undefined);
  for(const c of h.clients.filter(c=>!c.stopped)) {
    const result=c.options.middleware.provideHover(numerical,{},()=> 'numerical-hover');
    assert.equal(result,c===owner(numerical)[0]?'numerical-hover':null);
  }
  const before=h.clients.length;
  await h.open('/umbrella/numerics/examples/other.fss');
  assert.equal(h.clients.length,before,'documents sharing a manifest reuse their process');
  assert.equal(h.errors.length,0);
  await h.extension.deactivate();
  assert.ok(h.clients.every(c=>c.stopped));
});

test('nested automatic overrides persist for simultaneously open unsaved documents and explicit none', async () => {
  const candidates=['/work/fscript.toml','/work/nested/fscript.toml'];
  const h=loadExtension({folders:['/work'],candidates});
  await h.extension.activate(h.context);
  await h.open('/work/nested/main.fss');
  const child=h.clients.find(c=>c.options.initializationOptions.configurationPath===candidates[1]);
  assert.ok(child);
  const parent=h.clients[0];
  h.pick('No configuration');await h.commands.get('fscript.selectConfiguration')();await pause();
  assert.ok(parent.stopped&&child.stopped);
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath,undefined);
  h.pick('Automatic selection');await h.commands.get('fscript.selectConfiguration')();await pause();
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath,candidates[1]);
  assert.ok(h.vscode.workspace.textDocuments[0].isDirty);
  candidates.pop();h.watchers.find(w=>w.pattern==='**/fscript.toml').fire('Delete','/work/nested/fscript.toml');await pause();
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath,candidates[0]);
  await h.extension.deactivate();
});


test('automatic launch and debugger resolve the manifest beside the actual script', async () => {
  const h=loadExtension({folders:['/umbrella'],candidates:['/umbrella/graphics/fscript.toml','/umbrella/numerics/examples/fscript.toml'],debugCli:true});
  await h.extension.activate(h.context);
  await h.open('/umbrella/numerics/examples/kmeans.fss');
  const result=await h.debugProviders[0].provider.resolveDebugConfigurationWithSubstitutedVariables(undefined,{type:'fscript',request:'launch',program:'/umbrella/numerics/examples/kmeans.fss'});
  assert.equal(result.configurationPath,'/umbrella/numerics/examples/fscript.toml');
  let launched;
  h.vscode.TaskRevealKind={Always:1};h.vscode.TaskPanelKind={New:1};
  h.vscode.ProcessExecution=class{constructor(command,args,options){Object.assign(this,{command,args,options});}};
  h.vscode.Task=class{constructor(definition,scope,name,source,execution){this.execution=execution;}};
  h.vscode.tasks={executeTask:async task=>{launched=task;}};
  await h.commands.get('fscript.runScript')();
  assert.deepEqual(Array.from(launched.execution.args),['--config','/umbrella/numerics/examples/fscript.toml','/umbrella/numerics/examples/kmeans.fss']);
  await h.extension.deactivate();
});

test('DLL changes restart only their automatic environment without disturbing sibling documents', async () => {
  const h=loadExtension({folders:['/umbrella'],candidates:['/umbrella/one/fscript.toml','/umbrella/two/fscript.toml'],
    environmentInfo: options => ({externAssemblies:options.configurationPath ? ['/extensions/'+path.basename(path.dirname(options.configurationPath))+'.dll']:[]})});
  await h.extension.activate(h.context);
  await h.open('/umbrella/one/a.fss');await h.open('/umbrella/two/b.fss');
  const first=h.clients.find(c=>c.options.initializationOptions.configurationPath==='/umbrella/one/fscript.toml');
  const second=h.clients.find(c=>c.options.initializationOptions.configurationPath==='/umbrella/two/fscript.toml');
  const firstDll=h.watchers.find(w=>w.pattern?.pattern==='one.dll');
  firstDll.fire('Change','/extensions/one.dll');firstDll.fire('Change','/extensions/one.dll');await pause();
  assert.ok(first.stopped);assert.equal(second.stopped,false);
  assert.equal(h.clients.at(-1).options.initializationOptions.configurationPath,'/umbrella/one/fscript.toml');
  await h.extension.deactivate();
});

test('folder removal during automatic child startup leaves no orphan process or watcher', async () => {
  let count=0,release;
  const waiting=new Promise(resolve=>{release=resolve;});
  const h=loadExtension({folders:['/work'],candidates:['/work/nested/fscript.toml'],initialize:async()=>{if(++count===3)await waiting;}});
  await h.extension.activate(h.context);
  await h.open('/work/nested/main.fss');
  h.vscode.workspace.workspaceFolders.splice(0,1);
  const removing=h.changeFolders();release();await removing;
  assert.ok(h.clients.slice(0,3).every(c=>c.stopped));
  await h.extension.deactivate();assert.ok(h.clients.every(c=>c.stopped));
});
