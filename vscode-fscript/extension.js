const fs = require('fs');
const path = require('path');
const os = require('os');
const cp = require('child_process');
const vscode = require('vscode');
const { selectConfiguration } = require('./config-selection');
const { scriptLaunch } = require('./script-launch');
const { debugLaunch } = require('./debug-launch');
const { cliCommand } = require('./cli-command');
const clients = new Map();
let extensionContext;
let configurationStatus;
let discoveryWatcher;

let client;
let configSubscription;
let languageClientModule;
let developmentServerBuild;
let outputChannel;
let statusItem;
let showOutputCommandSubscription;
let viewAstCommandSubscription;
let viewInferredAstCommandSubscription;
let openReplCommandSubscription;
let sendSelectionToReplCommandSubscription;
let executeCurrentScriptInReplCommandSubscription;
let diagnosticsSubscription;
let textChangeSubscription;
let textOpenSubscription;
let stdlibContentProviderSubscription;
let replTerminal;
const pendingAnalysis = new Map();
const REPL_TERMINAL_NAME = 'FScript REPL';

function getConfig(uri) {
  const cfg = vscode.workspace.getConfiguration('fscript', uri ?? null);
  const windowConfig = vscode.workspace.getConfiguration('fscript');
  return {
    configurationPath: (cfg.get('config', '') || '').trim(),
    cliPath: (cfg.get('cli.path', '') || '').trim(),
    lspEnabled: cfg.get('lsp.enabled', true),
    inlayHintsEnabled: cfg.get('inlayHints.enabled', true),
    hoverHintsEnabled: cfg.get('hoverHints.enabled', true),
    serverPath: (cfg.get('server.path', '') || '').trim(),
    logLevel: cfg.get('server.logLevel', 'info'),
    replCommand: (windowConfig.get('repl.command', 'auto') || '').trim() || 'auto'
  };
}

function getDocumentSelector() {
  return [
    { scheme: 'file', language: 'fscript' },
    { scheme: 'untitled', language: 'fscript' }
  ];
}

async function resolveDotnetCommand(context) {
  const requestingExtensionId = `${context.extension.packageJSON.publisher}.${context.extension.packageJSON.name}`;
  try {
    const runtime = await vscode.commands.executeCommand('dotnet.acquire', {
      version: '10.0',
      requestingExtensionId
    });
    if (runtime && runtime.dotnetPath) {
      return runtime.dotnetPath;
    }
  } catch {
    // Fallback to PATH-based dotnet when acquisition is unavailable.
  }
  return 'dotnet';
}

function runDotnet(args, cwd) {
  return new Promise(resolve => {
    cp.execFile('dotnet', args, { cwd, encoding: 'utf8' }, (error, stdout, stderr) => {
      resolve({ status: error ? (error.code || 1) : 0, stdout: stdout || '', stderr: stderr || error?.message || '' });
    });
  });
}

async function createServerOptions(context, config, folder) {
  const { TransportKind } = require('vscode-languageclient/node');
  const runtimeDotnetCommand = await resolveDotnetCommand(context);
  if (config.serverPath) {
    const userProvidedPath = path.isAbsolute(config.serverPath)
      ? config.serverPath
      : path.resolve(folder?.uri.fsPath || context.extensionPath, config.serverPath);

    if (fs.existsSync(userProvidedPath)) {
      return {
        run: { command: runtimeDotnetCommand, args: [userProvidedPath], transport: TransportKind.stdio },
        debug: { command: runtimeDotnetCommand, args: [userProvidedPath], transport: TransportKind.stdio }
      };
    }

    vscode.window.showWarningMessage(
      `FScript extension: configured server path does not exist: ${userProvidedPath}`
    );
  }

  const packagedDll = path.join(context.extensionPath, 'server', 'FScript.LanguageServer.dll');

  if (fs.existsSync(packagedDll)) {
    return {
      run: { command: runtimeDotnetCommand, args: [packagedDll], transport: TransportKind.stdio },
      debug: { command: runtimeDotnetCommand, args: [packagedDll], transport: TransportKind.stdio }
    };
  }

  if (context.extensionMode !== vscode.ExtensionMode.Development && context.extensionMode !== vscode.ExtensionMode.Test) {
    vscode.window.showErrorMessage(
      'FScript extension: packaged language server is missing. Reinstall the extension or set fscript.server.path.'
    );
    return null;
  }

  const projectPath = path.resolve(
    context.extensionPath,
    '..',
    'src',
    'FScript.LanguageServer',
    'FScript.LanguageServer.csproj'
  );
  const outputDll = path.resolve(
    context.extensionPath,
    '..',
    'src',
    'FScript.LanguageServer',
    'bin',
    'Debug',
    'net10.0',
    'FScript.LanguageServer.dll'
  );

  if (!developmentServerBuild) {
    developmentServerBuild = (async () => {
      const probe = await runDotnet(['--list-sdks'], context.extensionPath);
      if (probe.status !== 0 || !probe.stdout.trim()) {
        return { status: 1, stdout: '', stderr: 'Language server development requires a .NET SDK in PATH. Install an SDK or set fscript.server.path.' };
      }
      return runDotnet(['build', projectPath, '-nologo', '-v', 'q'], context.extensionPath);
    })();
  }
  const build = await developmentServerBuild;

  if (build.status !== 0 || !fs.existsSync(outputDll)) {
    developmentServerBuild = undefined;
    const details = [build.stdout, build.stderr].filter(Boolean).join('\n');
    vscode.window.showErrorMessage(
      `FScript extension: unable to build language server.\n${details}`.trim()
    );
    return null;
  }

  return {
    run: { command: runtimeDotnetCommand, args: [outputDll], transport: TransportKind.stdio },
    debug: { command: runtimeDotnetCommand, args: [outputDll], transport: TransportKind.stdio }
  };
}

async function stopClient() {
  const current = [...clients.values()];
  clients.clear();
  client = undefined;
  for (const entry of current) {
    entry.disposed = true;
    clearTimeout(entry.timer);
    clearTimeout(entry.folderTimer);
    for (const watcher of entry.watchers || []) watcher.dispose();
    entry.sourceWatcher?.dispose();
    await entry.routingQueue?.catch(() => {});
    await entry.queue?.catch(() => {});
    if (entry.client) await entry.client.stop();
  }
  for (const timeout of pendingAnalysis.values()) {
    clearTimeout(timeout);
  }
  pendingAnalysis.clear();
}

function getActiveFscriptFileUri() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('FScript extension: no active editor.');
    return null;
  }

  const document = editor.document;
  if (document.languageId !== 'fscript') {
    vscode.window.showErrorMessage('FScript extension: active editor is not an FScript file.');
    return null;
  }

  if (document.isUntitled || document.uri.scheme !== 'file') {
    vscode.window.showErrorMessage('FScript extension: save the FScript file before viewing AST.');
    return null;
  }

  return document.uri;
}

function getActiveFscriptEditor() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('FScript extension: no active editor.');
    return null;
  }
  if (editor.document.languageId !== 'fscript') {
    vscode.window.showErrorMessage('FScript extension: active editor is not an FScript file.');
    return null;
  }
  return editor;
}

function getTerminalWorkingDirectory() {
  const editor = vscode.window.activeTextEditor;
  const activeDocDir = editor?.document?.uri?.scheme === 'file'
    ? path.dirname(editor.document.uri.fsPath)
    : undefined;
  return activeDocDir || vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

function resolveWorkspaceRoot() {
  return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

function resolveReplLaunchCommand() {
  const config = getConfig();
  if (config.replCommand !== 'auto') {
    return config.replCommand;
  }

  const workspaceRoot = resolveWorkspaceRoot();
  if (workspaceRoot) {
    const localCliProject = path.join(workspaceRoot, 'src', 'FScript', 'FScript.fsproj');
    if (fs.existsSync(localCliProject)) {
      return `dotnet run --project "${localCliProject}" -- --no-config`;
    }
  }

  return 'fscript --no-config';
}

function ensureReplTerminal() {
  if (vscode.workspace.isTrusted === false) {
    vscode.window.showErrorMessage('FScript: trust this workspace before running scripts.');
    return null;
  }
  if (replTerminal && !replTerminal.exitStatus) {
    replTerminal.show(true);
    return replTerminal;
  }

  const replLaunchCommand = resolveReplLaunchCommand();
  replTerminal = vscode.window.createTerminal({
    name: REPL_TERMINAL_NAME,
    cwd: getTerminalWorkingDirectory()
  });
  replTerminal.show(true);
  replTerminal.sendText(replLaunchCommand, true);
  return replTerminal;
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function sendCodeToRepl(terminal, code) {
  const normalized = code.replace(/\r\n/g, '\n');
  for (const line of normalized.split('\n')) {
    terminal.sendText(line, true);
  }
  // Force execution for multiline/pending blocks in the REPL.
  terminal.sendText('', true);
  terminal.sendText('', true);
}

function dedentMultilineBlock(code) {
  const lines = code.replace(/\r\n/g, '\n').split('\n');
  let minIndent = Number.MAX_SAFE_INTEGER;

  for (const line of lines) {
    if (line.trim().length === 0) {
      continue;
    }
    const match = line.match(/^(\s*)/);
    const indent = match ? match[1].length : 0;
    if (indent < minIndent) {
      minIndent = indent;
    }
  }

  if (!Number.isFinite(minIndent) || minIndent === Number.MAX_SAFE_INTEGER || minIndent === 0) {
    return lines.join('\n');
  }

  return lines
    .map((line) => {
      if (line.trim().length === 0) {
        return line;
      }
      return line.slice(minIndent);
    })
    .join('\n');
}

function wrapCodeForAtomicReplExecution(code) {
  const normalized = dedentMultilineBlock(code).trimEnd();
  const runId = `__vscode_run_${Date.now()}`;
  const valueId = `__vscode_value_${Date.now()}`;
  const lines = normalized.split('\n');

  if (lines.length <= 1) {
    return `let ${runId} =\n  let ${valueId} =\n    ${normalized}\n  ${valueId}`;
  }

  const body = lines
    .map((line) => (line.length > 0 ? `    ${line}` : line))
    .join('\n');

  return `let ${runId} =\n  let ${valueId} =\n${body}\n  ${valueId}`;
}

function getTopLevelLetBindings(code) {
  const lines = dedentMultilineBlock(code).split('\n');
  const bindings = [];
  const seen = new Set();
  const pattern = /^\s*let\s+(?:rec\s+)?([A-Za-z_][A-Za-z0-9_]*)\b/;
  for (const line of lines) {
    const match = line.match(pattern);
    if (!match) {
      continue;
    }
    const name = match[1];
    if (!seen.has(name)) {
      bindings.push(name);
      seen.add(name);
    }
  }
  return bindings;
}

function isSingleTopLevelLetBindingSelection(code, expectedName) {
  const lines = dedentMultilineBlock(code).split('\n');
  const nonEmpty = lines.filter((line) => line.trim().length > 0);
  if (nonEmpty.length === 0) {
    return false;
  }

  const first = nonEmpty[0];
  const firstMatch = first.match(/^\s*let\s+(?:rec\s+)?([A-Za-z_][A-Za-z0-9_]*)\b/);
  if (!firstMatch || firstMatch[1] !== expectedName) {
    return false;
  }

  for (let i = 1; i < nonEmpty.length; i += 1) {
    const line = nonEmpty[i];
    const indentMatch = line.match(/^(\s*)/);
    const indent = indentMatch ? indentMatch[1].length : 0;
    // Any extra top-level statement means this is not a pure binding selection.
    if (indent === 0) {
      return false;
    }
  }

  return true;
}

function wrapSelectionForPersistentBindings(code) {
  const normalized = dedentMultilineBlock(code).trimEnd();
  const bindings = getTopLevelLetBindings(normalized);
  if (bindings.length !== 1) {
    return wrapCodeForAtomicReplExecution(normalized);
  }

  const [name] = bindings;
  if (!isSingleTopLevelLetBindingSelection(normalized, name)) {
    return wrapCodeForAtomicReplExecution(normalized);
  }

  const runId = `__vscode_run_${Date.now()}`;
  const indentedBody = normalized
    .split('\n')
    .map((line) => (line.length > 0 ? `  ${line}` : line))
    .join('\n');

  return `let ${runId} =\n${indentedBody}\n  ${name}\nlet ${name} = ${runId}`;
}

async function openJsonInTempFile(prefix, data) {
  const nonce = `${Date.now()}-${Math.random().toString(36).slice(2, 10)}`;
  const tempPath = path.join(os.tmpdir(), `${prefix}-${nonce}.json`);
  fs.writeFileSync(tempPath, `${JSON.stringify(data, null, 2)}\n`, 'utf8');
  const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(tempPath));
  await vscode.languages.setTextDocumentLanguage(doc, 'json');
  await vscode.window.showTextDocument(doc, { preview: false });
}

async function runAstCommand(methodName, tempPrefix) {
  if (!client) {
    vscode.window.showErrorMessage('FScript extension: language server is not running. Enable fscript.lsp.enabled.');
    return;
  }

  const uri = getActiveFscriptFileUri();
  if (!uri) {
    return;
  }

  try {
    const targetClient = clientForUri(uri);
    if (!targetClient) throw new Error('Language server for this folder is not running.');
    const result = await targetClient.sendRequest(methodName, {
      textDocument: { uri: uri.toString() }
    });

    if (!result || result.ok !== true) {
      const message = result?.error?.message || 'Unknown AST command error.';
      vscode.window.showErrorMessage(`FScript extension: ${message}`);
      return;
    }

    await openJsonInTempFile(tempPrefix, result.data);
  } catch (err) {
    const msg = err && err.message ? err.message : String(err);
    vscode.window.showErrorMessage(`FScript extension: failed to execute AST command. ${msg}`);
  }
}

function ensureUi(context) {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel('FScript Language Server', { log: true });
    context.subscriptions.push(outputChannel);
  }
  if (!statusItem) {
    statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    statusItem.name = 'FScript Language Server';
    statusItem.command = 'fscript.showLanguageServerOutput';
    statusItem.tooltip = 'FScript Language Server status';
    statusItem.text = '$(circle-large-outline) FScript';
    statusItem.show();
    context.subscriptions.push(statusItem);
  }
}

function setStatusStarting() {
  if (statusItem) {
    statusItem.text = '$(sync~spin) FScript: starting...';
    statusItem.tooltip = 'FScript Language Server is starting';
    statusItem.show();
  }
}

function setStatusReady() {
  if (statusItem) {
    statusItem.text = '$(check) FScript';
    statusItem.tooltip = 'FScript Language Server is ready';
    statusItem.show();
  }
}

function setStatusAnalyzing() {
  if (statusItem) {
    statusItem.text = '$(sync~spin) FScript: analyzing...';
    statusItem.tooltip = 'FScript Language Server is analyzing current script';
    statusItem.show();
  }
}

function setStatusDisabled() {
  if (statusItem) {
    statusItem.text = '$(circle-slash) FScript';
    statusItem.tooltip = 'FScript Language Server is disabled by settings';
    statusItem.show();
  }
}

function setStatusError(message) {
  if (statusItem) {
    statusItem.text = '$(error) FScript';
    statusItem.tooltip = `FScript Language Server error: ${message}`;
    statusItem.show();
  }
}

function queueAnalysis(uri) {
  if (!client || !uri) {
    return;
  }

  const key = uri.toString();
  const existing = pendingAnalysis.get(key);
  if (existing) {
    clearTimeout(existing);
  }

  const timeout = setTimeout(() => {
    pendingAnalysis.delete(key);
    if (pendingAnalysis.size === 0 && client) {
      setStatusReady();
    }
  }, 5000);

  pendingAnalysis.set(key, timeout);
  setStatusAnalyzing();
}

function completeAnalysis(uri) {
  const key = uri.toString();
  const timeout = pendingAnalysis.get(key);
  if (!timeout) {
    return;
  }

  clearTimeout(timeout);
  pendingAnalysis.delete(key);
  if (pendingAnalysis.size === 0 && client) {
    setStatusReady();
  }
}

function registerStdlibContentProvider(context) {
  if (stdlibContentProviderSubscription) {
    return;
  }

  const provider = {
    async provideTextDocumentContent(uri) {
      if (!client) {
        throw new Error('FScript language server is not running.');
      }

      const targetClient = clientForUri(vscode.window.activeTextEditor?.document.uri) || client;
      const response = await targetClient.sendRequest('fscript/stdlibSource', { uri: uri.toString() });
      if (!response || response.ok !== true || !response.data || typeof response.data.text !== 'string') {
        const message = response?.error?.message || `Unable to load stdlib source for ${uri.toString()}.`;
        throw new Error(message);
      }

      return response.data.text;
    }
  };

  stdlibContentProviderSubscription = vscode.workspace.registerTextDocumentContentProvider('fscript-stdlib', provider);
  context.subscriptions.push(stdlibContentProviderSubscription);
}

async function startFolderClient(context, entry) {
  ensureUi(context);
  try {
    if (!languageClientModule) {
      languageClientModule = require('vscode-languageclient/node');
    }
  } catch {
    vscode.window.showWarningMessage(
      'FScript extension: syntax highlighting is active, but LSP is disabled (missing vscode-languageclient). Run `npm install` in vscode-fscript/.'
    );
    return;
  }

  const config = getConfig(entry.folder?.uri);
  if (!config.lspEnabled) {
    setStatusDisabled();
    return;
  }

  try {
    setStatusStarting();
    const serverOptions = await createServerOptions(context, config, entry.folder);
    if (!serverOptions) {
      setStatusError('Server build/resolution failed');
      return;
    }

    const owns = document => environmentKey(document.uri) === entry.key;
    const middleware = {
      didOpen: async (document, next) => { if (owns(document)) await next(document); },
      didChange: async (event, next) => { if (owns(event.document)) await next(event); },
      didClose: async (document, next) => { if (owns(document)) await next(document); }
    };
    for (const name of ['provideCompletionItem', 'provideHover', 'provideDefinition', 'provideTypeDefinition',
      'provideDocumentSymbols', 'provideReferences', 'provideDocumentHighlights', 'provideSignatureHelp',
      'provideRenameEdits', 'prepareRename', 'provideCodeActions', 'provideInlayHints', 'provideDocumentSemanticTokens', 'provideDocumentSemanticTokensEdits']) {
      middleware[name] = (document, ...args) => owns(document) ? args.pop()(document, ...args) : null;
    }
    entry.sourceWatcher?.dispose();
    entry.sourceWatcher = vscode.workspace.createFileSystemWatcher(entry.folder ? new vscode.RelativePattern(entry.folder, '**/*.fss') : '**/*.fss');
    const clientOptions = {
      middleware,
      workspaceFolder: entry.folder,
      documentSelector: entry.folder
        ? [{ scheme: 'file', language: 'fscript', pattern: { baseUri: entry.folder.uri.toString(), pattern: '**/*' } }]
        : getDocumentSelector(),
      outputChannel,
      synchronize: {
        fileEvents: entry.sourceWatcher
      },
      initializationOptions: {
        configurationPath: entry.selection?.path,
        workspaceFolderUri: entry.folder?.uri.toString(),
        workspaceTrusted: vscode.workspace.isTrusted === true,
        logLevel: config.logLevel,
        inlayHintsEnabled: config.inlayHintsEnabled,
        hoverHintsEnabled: config.hoverHintsEnabled
      }
    };

    outputChannel?.debug(`Starting environment ${entry.key}: ${entry.selection?.path || 'defaults'}`);
    const LanguageClient = languageClientModule.LanguageClient;
    entry.client = new LanguageClient(`fscriptLanguageServer${entry.key}`, 'FScript Language Server', serverOptions, clientOptions);
    await entry.client.start();
    if (entry.disposed) { await entry.client.stop(); entry.client = undefined; return; }
    client = entry.client;
    if (entry.client.sendRequest) {
      entry.info = await entry.client.sendRequest('fscript/environmentInfo', {});
    }
    if (entry.disposed) return;
    installEnvironmentWatchers(entry);
    setStatusReady();
    updateConfigurationStatus();
  } catch (err) {
    const msg = err && err.message ? err.message : String(err);
    vscode.window.showErrorMessage(`FScript extension: failed to start language server. ${msg}`);
    setStatusError(msg);
    entry.sourceWatcher?.dispose();
    if (entry.client) { await entry.client.stop().catch(() => {}); entry.client = undefined; }
  }
}

function folderKey(uri) {
  return uri ? (vscode.workspace.getWorkspaceFolder?.(uri)?.uri.toString() || '') : '';
}

function documentSelection(base, uri) {
  const selected = selectConfiguration(getConfig(base.folder.uri).configurationPath,
    extensionContext.workspaceState?.get(selectionKey(base.folder)), base.candidates || [], uri?.fsPath);
  if (selected.path) selected.path = path.resolve(base.folder.uri.fsPath, selected.path);
  return selected;
}

function environmentKey(uri) {
  const key = folderKey(uri);
  const base = clients.get(key);
  if (!base?.folder || !uri?.fsPath) return key;
  const selected = documentSelection(base, uri);
  // Settings and saved explicit selections use the folder's existing process.
  if (selected.source !== 'automatic' || selected.path === base.selection?.path) return key;
  return `${key}::${selected.path}`;
}

function clientForUri(uri) {
  return clients.get(environmentKey(uri))?.client;
}

async function ensureDocumentEnvironment(base, uri) {
  if (base.disposed || clients.get(base.key) !== base) return;
  const key = environmentKey(uri);
  if (clients.has(key)) return;
  const entry = { key, folder: base.folder, base, selection: documentSelection(base, uri), watchers: [], disposed: false };
  clients.set(key, entry);
  await startFolderClient(extensionContext, entry);
}

function enqueueFolder(base, action) {
  base.routingQueue = (base.routingQueue || Promise.resolve()).catch(() => {}).then(async () => {
    if (!base.disposed && clients.get(base.key) === base) await action();
  }).catch(error => { outputChannel?.error(String(error)); setStatusError(String(error)); });
  return base.routingQueue;
}

function openDocumentEnvironment(document) {
  if (document.languageId !== 'fscript' || document.isClosed) return;
  const base = clients.get(folderKey(document.uri));
  if (base?.folder) return enqueueFolder(base, () => document.isClosed ? undefined : ensureDocumentEnvironment(base, document.uri));
}

async function synchronizeOpenEnvironments(base) {
  const documents = [...(vscode.workspace.textDocuments || [])];
  const active = vscode.window.activeTextEditor?.document;
  if (active && !documents.includes(active)) documents.push(active);
  for (const document of documents) {
    if (document.languageId === 'fscript' && folderKey(document.uri) === base.key) await ensureDocumentEnvironment(base, document.uri);
  }
}

function scheduleFolderRestart(base) {
  clearTimeout(base.folderTimer);
  base.folderTimer = setTimeout(() => enqueueFolder(base, async () => {
    // Stop every old process before changing routing. New clients resynchronize open/unsaved documents.
    const related = [...clients.values()].filter(entry => entry === base || entry.base === base);
    for (const entry of related) {
      entry.disposed = true;
      clearTimeout(entry.timer);
      for (const watcher of entry.watchers || []) watcher.dispose();
      entry.sourceWatcher?.dispose();
      await entry.queue?.catch(() => {});
      if (entry.client) await entry.client.stop();
      entry.client = undefined;
      entry.info = undefined;
      if (entry !== base) clients.delete(entry.key);
    }
    if (clients.get(base.key) !== base) return;
    base.disposed = false;
    await refreshSelection(base);
    if (base.disposed) return;
    await startFolderClient(extensionContext, base);
    await synchronizeOpenEnvironments(base);
  }), 250);
}

function selectionKey(folder) { return `fscript.environment:${folder.uri.toString()}`; }

async function discoverConfigurations(folder) {
  const exclusions = ['**/.git/**', '**/.out/**', '**/.fscript/**', '**/node_modules/**', '**/bin/**', '**/obj/**'];
  for (const section of ['files', 'search']) {
    const values = vscode.workspace.getConfiguration(section, folder.uri).get('exclude', {});
    for (const [pattern, enabled] of Object.entries(values)) if (enabled === true) exclusions.push(pattern);
  }
  const found = await vscode.workspace.findFiles(new vscode.RelativePattern(folder, '**/fscript.toml'), `{${exclusions.join(',')}}`);
  return found.filter(uri => folderKey(uri) === folder.uri.toString()).map(uri => uri.fsPath).sort();
}

async function refreshSelection(entry, uri) {
  if (!entry.folder || entry.base) return;
  entry.candidates = await discoverConfigurations(entry.folder);
  const configured = getConfig(entry.folder.uri).configurationPath;
  const selected = selectConfiguration(configured, extensionContext.workspaceState?.get(selectionKey(entry.folder)), entry.candidates, uri?.fsPath || path.join(entry.folder.uri.fsPath, '.fscript-environment.fss'));
  if (selected.path) selected.path = path.resolve(entry.folder.uri.fsPath, selected.path);
  entry.selection = selected;
}

function updateConfigurationStatus() {
  if (!configurationStatus) return;
  const uri = vscode.window.activeTextEditor?.document.uri;
  const base = clients.get(folderKey(uri));
  const roots = [...clients.values()].filter(entry => entry.folder && !entry.base);
  const entry = clients.get(environmentKey(uri)) || base || (roots.length === 1 ? roots[0] : undefined);
  if (!entry?.folder) { configurationStatus.hide(); return; }
  const selection = base?.folder && uri?.fsPath ? documentSelection(base, uri) : entry.selection || { source: 'default' };
  const label = selection.path ? path.relative(entry.folder.uri.fsPath, selection.path) : 'No configuration';
  const state = entry.info?.failed ? 'error' : entry.info?.blockedByTrust ? 'trust required' : selection.source;
  configurationStatus.text = `${entry.info?.failed ? '$(error)' : '$(settings-gear)'} ${label}${state === 'trust required' ? ': trust required' : ''}`;
  const packages = (entry.info?.packages || []).map(pkg => `${pkg.name} ${pkg.version}`).join(', ');
  configurationStatus.tooltip = `${entry.folder.name}: ${label} (${state})${packages ? `\nNuGet: ${packages}` : ''}${entry.info?.error ? `\n${entry.info.error}` : ''}`;
  configurationStatus.show();
}

function installEnvironmentWatchers(entry) {
  for (const watcher of entry.watchers || []) watcher.dispose();
  entry.watchers = [];
  if (!entry.folder || !vscode.workspace.createFileSystemWatcher) return;
  const patterns = new Set();
  if (entry.selection?.path) patterns.add(entry.selection.path);
  for (const assembly of entry.info?.externAssemblies || []) {
    patterns.add(assembly);
    patterns.add(path.join(path.dirname(assembly), '*.dll'));
  }
  for (const absolutePattern of patterns) {
    const watcher = vscode.workspace.createFileSystemWatcher(new vscode.RelativePattern(path.dirname(absolutePattern), path.basename(absolutePattern)));
    for (const event of ['onDidCreate', 'onDidChange', 'onDidDelete']) watcher[event]?.(() => scheduleEnvironmentRestart(entry));
    entry.watchers.push(watcher);
  }
}

function scheduleEnvironmentRestart(entry) {
  clearTimeout(entry.timer);
  entry.timer = setTimeout(() => {
    entry.queue = (entry.queue || Promise.resolve()).catch(() => {}).then(async () => {
      if (entry.disposed) return;
      await refreshSelection(entry);
      if (entry.disposed) return;
      const previous = entry.client;
      entry.client = undefined;
      entry.info = undefined;
      if (previous) await previous.stop();
      entry.sourceWatcher?.dispose();
      if (client === previous) client = [...clients.values()].find(e => e.client)?.client;
      if (!entry.disposed) await startFolderClient(extensionContext, entry);
    }).catch(error => { outputChannel?.error(String(error)); setStatusError(String(error)); });
  }, 250);
}

async function startClient(context) {
  for (const folder of [...(vscode.workspace.workspaceFolders || []), undefined]) {
    const key = folder?.uri.toString() || '';
    if (clients.has(key)) continue;
    const entry = { key, folder, watchers: [], disposed: false };
    clients.set(key, entry);
    await refreshSelection(entry);
    await startFolderClient(context, entry);
    if (folder) await enqueueFolder(entry, () => synchronizeOpenEnvironments(entry));
  }
}

function registerEnvironmentUi(context) {
  if (!vscode.workspace.findFiles) return;
  configurationStatus = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99);
  configurationStatus.command = 'fscript.selectConfiguration';
  context.subscriptions.push(configurationStatus);
  context.subscriptions.push(vscode.commands.registerCommand('fscript.selectConfiguration', async () => {
    let folder = (vscode.window.activeTextEditor ? vscode.workspace.getWorkspaceFolder(vscode.window.activeTextEditor.document.uri) : undefined);
    if (!folder) folder = await vscode.window.showWorkspaceFolderPick();
    if (!folder) return;
    const entry = clients.get(folder.uri.toString());
    if (!entry) return;
    await refreshSelection(entry);
    if (getConfig(folder.uri).configurationPath) {
      const action = await vscode.window.showInformationMessage('FScript: fscript.config controls this folder. Clear it to use a saved selection.', 'Open Settings');
      if (action) await vscode.commands.executeCommand('workbench.action.openWorkspaceSettings', { query: 'fscript.config' });
      return;
    }
    const picked = await vscode.window.showQuickPick([
      { label: 'No configuration', choice: { kind: 'none' } },
      { label: 'Automatic selection', choice: { kind: 'automatic' } },
      ...entry.candidates.map(file => ({ label: path.relative(folder.uri.fsPath, file), choice: { kind: 'manifest', path: file } }))
    ], { title: `FScript configuration — ${folder.name}` });
    if (!picked) return;
    await context.workspaceState.update(selectionKey(folder), picked.choice);
    scheduleFolderRestart(entry);
  }));
  const rediscover = uri => {
    const entry = clients.get(folderKey(uri));
    if (entry?.folder) scheduleFolderRestart(entry);
  };
  discoveryWatcher = vscode.workspace.createFileSystemWatcher('**/fscript.toml');
  discoveryWatcher.onDidCreate(rediscover);
  discoveryWatcher.onDidDelete(rediscover);
  context.subscriptions.push(discoveryWatcher);
  context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(editor => {
    if (editor) openDocumentEnvironment(editor.document);
    updateConfigurationStatus();
  }));
  context.subscriptions.push(vscode.workspace.onDidGrantWorkspaceTrust(() => {
    for (const entry of clients.values()) scheduleEnvironmentRestart(entry);
  }));
  context.subscriptions.push(vscode.workspace.onDidChangeWorkspaceFolders(async () => {
    // Ownership can change for documents in nested roots, so recreate the default client too.
    await stopClient();
    await startClient(context);
  }));
}

function registerDebugger(context) {
  if (!vscode.debug?.registerDebugConfigurationProvider) return;
  context.subscriptions.push(vscode.commands.registerCommand('fscript.debugScript', async () => {
    if (vscode.workspace.isTrusted !== true) {
      vscode.window.showErrorMessage('FScript: trust this workspace before debugging scripts.');
      return;
    }
    const editor = getActiveFscriptEditor();
    if (!editor) return;
    const document = editor.document;
    if (document.uri.scheme !== 'file') {
      vscode.window.showErrorMessage('FScript: save the script to a file before debugging it.');
      return;
    }
    return vscode.debug.startDebugging(vscode.workspace.getWorkspaceFolder(document.uri), {
      type: 'fscript', request: 'launch', name: 'Debug Current Script', program: document.uri.fsPath
    });
  }));
  context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('fscript', {
    resolveDebugConfiguration(_folder, configuration) {
      if (!configuration.type) return { ...configuration, type: 'fscript', request: 'launch', name: 'Debug FScript', program: '${file}' };
      return configuration;
    },
    async resolveDebugConfigurationWithSubstitutedVariables(_folder, configuration) {
      if (vscode.workspace.isTrusted !== true) {
        vscode.window.showErrorMessage('FScript: trust this workspace before debugging scripts.');
        return undefined;
      }
      try {
        if (configuration.request !== 'launch') throw new Error('FScript supports launch debugging; attaching is not available.');
        const requestedProgram = configuration.program || getActiveFscriptEditor()?.document.uri.fsPath;
        if (!requestedProgram) throw new Error('Open and save an FScript file before debugging.');
        const program = (path.isAbsolute(requestedProgram) ? requestedProgram : path.resolve(_folder?.uri.fsPath || configuration.cwd || process.cwd(), requestedProgram));
        const folder = vscode.workspace.getWorkspaceFolder(vscode.Uri.file(program));
        for (const document of vscode.workspace.textDocuments) {
          if (!document.isDirty || document.languageId !== 'fscript') continue;
          const owner = vscode.workspace.getWorkspaceFolder(document.uri);
          if ((folder ? owner?.uri.toString() === folder.uri.toString() : document.uri.fsPath === program) && !(await document.save())) return undefined;
        }
        const entry = { folder };
        if (folder) await refreshSelection(entry, vscode.Uri.file(program));
        const cli = cliCommand(getConfig(vscode.Uri.file(program)).cliPath, folder?.uri.fsPath, program);
        const capability = await new Promise(resolve => {
          cp.execFile(cli.command, [...cli.args, '--debug-capabilities'], { timeout: 5000, encoding: 'utf8' }, (error, stdout) => {
            try { resolve(!error && JSON.parse(stdout).protocolVersion === 1); } catch { resolve(false); }
          });
        });
        if (!capability) throw new Error('The selected FScript CLI could not report debugger support. Check fscript.cli.path, or install a CLI supporting debugging on PATH.');
        return debugLaunch(configuration, program, folder?.uri.fsPath, entry.selection?.path);
      } catch (error) {
        vscode.window.showErrorMessage(`FScript: ${error.message}`);
        return undefined;
      }
    }
  }));
  context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('fscript', {
    createDebugAdapterDescriptor(session) {
      if (vscode.workspace.isTrusted !== true) throw new Error('Trust this workspace before debugging scripts.');
      const program = session?.configuration?.program;
      const uri = program ? vscode.Uri.file(program) : undefined;
      const folder = uri ? vscode.workspace.getWorkspaceFolder(uri) : session?.workspaceFolder;
      const cli = cliCommand(getConfig(uri || folder?.uri).cliPath, folder?.uri.fsPath, program);
      return new vscode.DebugAdapterExecutable(cli.command, [...cli.args, '--debug-adapter']);
    }
  }));
}

async function activate(context) {
  registerDebugger(context);
  extensionContext = context;
  ensureUi(context);
  registerEnvironmentUi(context);
  registerStdlibContentProvider(context);
  showOutputCommandSubscription = vscode.commands.registerCommand('fscript.showLanguageServerOutput', () => {
    if (outputChannel) {
      outputChannel.show(true);
    } else {
      vscode.commands.executeCommand('workbench.action.output.toggleOutput');
    }
  });
  context.subscriptions.push(showOutputCommandSubscription);

  viewAstCommandSubscription = vscode.commands.registerCommand('fscript.viewAst', async () => {
    await runAstCommand('fscript/viewAst', 'fscript-ast');
  });
  context.subscriptions.push(viewAstCommandSubscription);

  viewInferredAstCommandSubscription = vscode.commands.registerCommand('fscript.viewInferredAst', async () => {
    await runAstCommand('fscript/viewInferredAst', 'fscript-inferred-ast');
  });
  context.subscriptions.push(viewInferredAstCommandSubscription);

  context.subscriptions.push(vscode.commands.registerCommand('fscript.runScript', async () => {
    if (vscode.workspace.isTrusted === false) {
      vscode.window.showErrorMessage('FScript: trust this workspace before running scripts.');
      return;
    }
    const editor = getActiveFscriptEditor();
    if (!editor) return;
    const document = editor.document;
    if (document.uri.scheme !== 'file') {
      vscode.window.showErrorMessage('FScript: save the script to a file before running it.');
      return;
    }
    if (document.isDirty && !(await document.save())) return;
    const folder = vscode.workspace.getWorkspaceFolder(document.uri);
    // Resolve fresh selection even when language-server features are disabled.
    const entry = { folder };
    if (folder) await refreshSelection(entry, document.uri);
    const launch = scriptLaunch(document.uri.fsPath, folder?.uri.fsPath, entry.selection?.path, getConfig(document.uri).cliPath);
    const task = new vscode.Task({ type: 'fscript', script: document.uri.toString() },
      folder || vscode.TaskScope.Workspace, 'Run Current Script', 'FScript',
      new vscode.ProcessExecution(launch.command, launch.args, { cwd: launch.cwd }), []);
    task.presentationOptions = { reveal: vscode.TaskRevealKind.Always, panel: vscode.TaskPanelKind.New };
    await vscode.tasks.executeTask(task);
  }));

  openReplCommandSubscription = vscode.commands.registerCommand('fscript.openRepl', () => {
    ensureReplTerminal();
  });
  context.subscriptions.push(openReplCommandSubscription);

  sendSelectionToReplCommandSubscription = vscode.commands.registerCommand('fscript.sendSelectionToRepl', async () => {
    const editor = getActiveFscriptEditor();
    if (!editor) {
      return;
    }

    const selectedText = editor.document.getText(editor.selection);
    if (!selectedText || selectedText.trim().length === 0) {
      vscode.window.showErrorMessage('FScript extension: selection is empty.');
      return;
    }

    const payload = selectedText.includes('\n')
      ? wrapSelectionForPersistentBindings(selectedText)
      : selectedText;

    const terminal = ensureReplTerminal();
    if (!terminal) return;
    await sleep(150);
    sendCodeToRepl(terminal, payload);
  });
  context.subscriptions.push(sendSelectionToReplCommandSubscription);

  executeCurrentScriptInReplCommandSubscription = vscode.commands.registerCommand('fscript.executeCurrentScriptInRepl', async () => {
    const editor = getActiveFscriptEditor();
    if (!editor) {
      return;
    }

    const scriptText = editor.document.getText();
    if (!scriptText || scriptText.trim().length === 0) {
      vscode.window.showErrorMessage('FScript extension: current script is empty.');
      return;
    }

    const payload = wrapCodeForAtomicReplExecution(scriptText);

    const terminal = ensureReplTerminal();
    if (!terminal) return;
    await sleep(150);
    sendCodeToRepl(terminal, payload);
  });
  context.subscriptions.push(executeCurrentScriptInReplCommandSubscription);

  diagnosticsSubscription = vscode.languages.onDidChangeDiagnostics((event) => {
    for (const uri of event.uris) {
      completeAnalysis(uri);
    }
  });
  context.subscriptions.push(diagnosticsSubscription);

  textChangeSubscription = vscode.workspace.onDidChangeTextDocument((event) => {
    if (event.document.languageId === 'fscript') {
      queueAnalysis(event.document.uri);
    }
  });
  context.subscriptions.push(textChangeSubscription);

  textOpenSubscription = vscode.workspace.onDidOpenTextDocument((doc) => {
    if (doc.languageId === 'fscript') {
      openDocumentEnvironment(doc);
      queueAnalysis(doc.uri);
    }
  });
  context.subscriptions.push(textOpenSubscription);

  await startClient(context);

  configSubscription = vscode.workspace.onDidChangeConfiguration(async (event) => {
    if (
      event.affectsConfiguration('files.exclude') ||
      event.affectsConfiguration('search.exclude') ||
      event.affectsConfiguration('fscript.config') ||
      event.affectsConfiguration('fscript.lsp.enabled') ||
      event.affectsConfiguration('fscript.inlayHints.enabled') ||
      event.affectsConfiguration('fscript.hoverHints.enabled') ||
      event.affectsConfiguration('fscript.server.path') ||
      event.affectsConfiguration('fscript.server.logLevel')
    ) {
      if (!vscode.workspace.findFiles) {
        await stopClient();
        await startClient(context);
      } else {
        for (const entry of clients.values()) {
          if (entry.base) continue;
          if (['fscript', 'files.exclude', 'search.exclude'].some(key => event.affectsConfiguration(key, entry.folder?.uri))) {
            if (entry.folder) scheduleFolderRestart(entry); else scheduleEnvironmentRestart(entry);
          }
        }
      }
    }
  });
  context.subscriptions.push(configSubscription);
}

async function deactivate() {
  discoveryWatcher?.dispose();
  if (showOutputCommandSubscription) {
    showOutputCommandSubscription.dispose();
    showOutputCommandSubscription = undefined;
  }
  if (viewAstCommandSubscription) {
    viewAstCommandSubscription.dispose();
    viewAstCommandSubscription = undefined;
  }
  if (viewInferredAstCommandSubscription) {
    viewInferredAstCommandSubscription.dispose();
    viewInferredAstCommandSubscription = undefined;
  }
  if (openReplCommandSubscription) {
    openReplCommandSubscription.dispose();
    openReplCommandSubscription = undefined;
  }
  if (sendSelectionToReplCommandSubscription) {
    sendSelectionToReplCommandSubscription.dispose();
    sendSelectionToReplCommandSubscription = undefined;
  }
  if (executeCurrentScriptInReplCommandSubscription) {
    executeCurrentScriptInReplCommandSubscription.dispose();
    executeCurrentScriptInReplCommandSubscription = undefined;
  }
  if (diagnosticsSubscription) {
    diagnosticsSubscription.dispose();
    diagnosticsSubscription = undefined;
  }
  if (textChangeSubscription) {
    textChangeSubscription.dispose();
    textChangeSubscription = undefined;
  }
  if (textOpenSubscription) {
    textOpenSubscription.dispose();
    textOpenSubscription = undefined;
  }
  if (configSubscription) {
    configSubscription.dispose();
    configSubscription = undefined;
  }
  if (stdlibContentProviderSubscription) {
    stdlibContentProviderSubscription.dispose();
    stdlibContentProviderSubscription = undefined;
  }
  if (replTerminal) {
    replTerminal.dispose();
    replTerminal = undefined;
  }
  await stopClient();
}

module.exports = {
  activate,
  deactivate
};
