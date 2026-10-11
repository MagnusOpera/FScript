const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { test } = require('node:test');

const extensionPath = path.resolve(__dirname, '../../vscode-fscript');

function loadExtension({ initialize = async () => {}, enabled = true } = {}) {
  const disposable = () => ({ dispose() {} });
  const errors = [];
  const clients = [];
  const channels = [];
  const status = { ...disposable(), show() {} };
  let configurationChanged;
  const vscode = {
    StatusBarAlignment: { Left: 1 },
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
      showErrorMessage: message => errors.push(message),
      showWarningMessage: message => errors.push(message)
    },
    workspace: {
      getConfiguration: () => ({ get: (key, fallback) => key === 'lsp.enabled' ? enabled : fallback }),
      createFileSystemWatcher: disposable,
      registerTextDocumentContentProvider: disposable,
      onDidChangeTextDocument: disposable,
      onDidOpenTextDocument: disposable,
      onDidChangeConfiguration(callback) {
        configurationChanged = callback;
        return disposable();
      }
    },
    languages: { onDidChangeDiagnostics: disposable },
    commands: {
      registerCommand: disposable,
      executeCommand: async () => ({ dotnetPath: 'dotnet' })
    }
  };
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
    async stop() { this.stopped = true; }
  }
  const sandbox = {
    module: { exports: {} },
    require(id) {
      if (id === 'vscode') return vscode;
      if (id === 'vscode-languageclient/node') return { LanguageClient, TransportKind: { stdio: 0 } };
      if (id === 'fs') return { ...fs, existsSync: () => true };
      return require(id);
    },
    setTimeout, clearTimeout
  };
  vm.runInNewContext(fs.readFileSync(path.join(extensionPath, 'extension.js'), 'utf8'), sandbox);
  const context = {
    extensionPath,
    extension: { packageJSON: { publisher: 'MagnusOpera', name: 'fscript' } },
    subscriptions: []
  };
  return {
    extension: sandbox.module.exports, context, errors, clients, channels, status,
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
