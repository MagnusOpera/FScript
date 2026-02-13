const fs = require('fs');
const path = require('path');
const cp = require('child_process');
const vscode = require('vscode');

let client;
let configSubscription;
let languageClientModule;
let outputChannel;
let statusItem;
let showOutputCommandSubscription;
let diagnosticsSubscription;
let textChangeSubscription;
let textOpenSubscription;
const pendingAnalysis = new Map();

function getConfig() {
  const cfg = vscode.workspace.getConfiguration('fscript');
  return {
    lspEnabled: cfg.get('lsp.enabled', true),
    inlayHintsEnabled: cfg.get('inlayHints.enabled', true),
    serverPath: (cfg.get('server.path', '') || '').trim(),
    logLevel: cfg.get('server.logLevel', 'info')
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

function hasDotnetSdkOnPath() {
  const probe = cp.spawnSync('dotnet', ['--list-sdks'], { encoding: 'utf8' });
  return probe.status === 0 && Boolean((probe.stdout || '').trim());
}

async function createServerOptions(context, config) {
  const { TransportKind } = require('vscode-languageclient/node');
  const runtimeDotnetCommand = await resolveDotnetCommand(context);
  if (config.serverPath) {
    const userProvidedPath = path.isAbsolute(config.serverPath)
      ? config.serverPath
      : path.resolve(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath || context.extensionPath, config.serverPath);

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

  if (context.extensionMode !== vscode.ExtensionMode.Development) {
    vscode.window.showErrorMessage(
      'FScript extension: packaged language server is missing. Reinstall the extension or set fscript.server.path.'
    );
    return null;
  }

  const projectPath = path.resolve(context.extensionPath, '..', 'src', 'FScript.LanguageServer', 'FScript.LanguageServer.fsproj');
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

  if (!hasDotnetSdkOnPath()) {
    vscode.window.showErrorMessage(
      'FScript extension: language server build fallback requires a .NET SDK in PATH. Install an SDK or set fscript.server.path.'
    );
    return null;
  }

  const build = cp.spawnSync('dotnet', ['build', projectPath, '-nologo', '-v', 'q'], {
    cwd: context.extensionPath,
    encoding: 'utf8'
  });

  if (build.status !== 0 || !fs.existsSync(outputDll)) {
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
  if (client) {
    const current = client;
    client = undefined;
    await current.stop();
  }
  for (const timeout of pendingAnalysis.values()) {
    clearTimeout(timeout);
  }
  pendingAnalysis.clear();
}

function ensureUi(context) {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel('FScript Language Server');
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

async function startClient(context) {
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

  const config = getConfig();
  if (!config.lspEnabled) {
    setStatusDisabled();
    return;
  }

  try {
    setStatusStarting();
    const serverOptions = await createServerOptions(context, config);
    if (!serverOptions) {
      setStatusError('Server build/resolution failed');
      return;
    }

    const clientOptions = {
      documentSelector: getDocumentSelector(),
      outputChannel,
      synchronize: {
        fileEvents: vscode.workspace.createFileSystemWatcher('**/*.fss')
      },
      initializationOptions: {
        logLevel: config.logLevel,
        inlayHintsEnabled: config.inlayHintsEnabled
      }
    };

    const LanguageClient = languageClientModule.LanguageClient;
    client = new LanguageClient('fscriptLanguageServer', 'FScript Language Server', serverOptions, clientOptions);
    const startDisposable = client.start();
    context.subscriptions.push(startDisposable);
    setStatusReady();
  } catch (err) {
    const msg = err && err.message ? err.message : String(err);
    vscode.window.showErrorMessage(`FScript extension: failed to start language server. ${msg}`);
    setStatusError(msg);
  }
}

function activate(context) {
  ensureUi(context);
  showOutputCommandSubscription = vscode.commands.registerCommand('fscript.showLanguageServerOutput', () => {
    if (outputChannel) {
      outputChannel.show(true);
    } else {
      vscode.commands.executeCommand('workbench.action.output.toggleOutput');
    }
  });
  context.subscriptions.push(showOutputCommandSubscription);

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
      queueAnalysis(doc.uri);
    }
  });
  context.subscriptions.push(textOpenSubscription);

  startClient(context);

  configSubscription = vscode.workspace.onDidChangeConfiguration(async (event) => {
    if (
      event.affectsConfiguration('fscript.lsp.enabled') ||
      event.affectsConfiguration('fscript.inlayHints.enabled') ||
      event.affectsConfiguration('fscript.server.path') ||
      event.affectsConfiguration('fscript.server.logLevel')
    ) {
      await stopClient();
      await startClient(context);
    }
  });
  context.subscriptions.push(configSubscription);
}

async function deactivate() {
  if (showOutputCommandSubscription) {
    showOutputCommandSubscription.dispose();
    showOutputCommandSubscription = undefined;
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
  await stopClient();
}

module.exports = {
  activate,
  deactivate
};
