// Runs inside a real VS Code Extension Development Host.
const assert = require('node:assert/strict');
const vscode = require('vscode');

exports.run = async function run() {
  const folder = vscode.workspace.workspaceFolders[0];
  const cliPath = vscode.workspace.getConfiguration('fscript',folder.uri).get('cli.path');
  assert.ok(cliPath.endsWith('fscript.dll'),'Smoke test must use the configured local CLI');
  const capabilities = require('node:child_process').execFileSync('dotnet',[cliPath,'--debug-capabilities'],{encoding:'utf8'});
  assert.equal(JSON.parse(capabilities).protocolVersion,1);
  const program = vscode.Uri.joinPath(folder.uri,'main.fss');
  const document = await vscode.workspace.openTextDocument(program);
  await vscode.window.showTextDocument(document);
  const extension = vscode.extensions.getExtension('MagnusOpera.fscript');
  assert.ok(extension,'Development extension must be installed');
  await extension.activate();
  // Reproduce an umbrella workspace with two sibling manifests in a real language client.
  const fs = require('node:fs');
  const path = require('node:path');
  const repo = path.resolve(__dirname, '../..');
  const provider = path.join(repo, 'tests/FScript.Cli.Tests.Fixtures.Valid/bin/Release/net10.0/FScript.Cli.Tests.Fixtures.Valid.dll');
  assert.ok(fs.existsSync(provider), 'Build the real provider fixture before the smoke test');
  const ffiDirectory = path.join(folder.uri.fsPath, 'numerics/examples');
  const plainDirectory = path.join(folder.uri.fsPath, 'other/examples');
  fs.mkdirSync(ffiDirectory, {recursive:true});
  fs.mkdirSync(plainDirectory, {recursive:true});
  const manifest = path.join(ffiDirectory, 'fscript.toml');
  const manifestText = `[runtime]\nextern-assemblies = ['${provider.replaceAll('\\','/')}']\n`;
  fs.writeFileSync(path.join(folder.uri.fsPath, 'provider-registration.requested'), '');
  fs.writeFileSync(manifest, manifestText);
  fs.writeFileSync(path.join(plainDirectory, 'fscript.toml'), '[runtime]\n');
  const validSource = 'let node = Ext.node 3\nlet value = node.Value\nvalue\n';
  const typedUri = vscode.Uri.file(path.join(ffiDirectory, 'main.fss'));
  const plainUri = vscode.Uri.file(path.join(plainDirectory, 'main.fss'));
  fs.writeFileSync(typedUri.fsPath, validSource);
  fs.writeFileSync(plainUri.fsPath, 'Ext.node 3\n');
  const typedDocument = await vscode.workspace.openTextDocument(typedUri);
  await vscode.window.showTextDocument(typedDocument,{preview:false});
  await vscode.window.showTextDocument(await vscode.workspace.openTextDocument(plainUri),{preview:false});
  async function until(check, label) {
    const deadline = Date.now()+30000;
    while(Date.now()<deadline) {
      if(await check()) return;
      await new Promise(resolve=>setTimeout(resolve,100));
    }
    throw new Error(`Timed out: ${label}; state: ${JSON.stringify({language:typedDocument.languageId,closed:typedDocument.isClosed,uri:typedUri.toString(),folder:folder.uri.toString(),candidates:(await vscode.workspace.findFiles('**/fscript.toml')).map(u=>u.toString()),enabled:vscode.workspace.getConfiguration('fscript',folder.uri).get('lsp.enabled'),server:vscode.workspace.getConfiguration('fscript',folder.uri).get('server.path'),diagnostics:vscode.languages.getDiagnostics().map(([u,d])=>[u.toString(),d])})}`);
  }
  const hoverHasInt = async () => {
    const hovers = await vscode.commands.executeCommand('vscode.executeHoverProvider', typedUri, new vscode.Position(1,19));
    return (hovers || []).some(hover => hover.contents.some(content => (typeof content === 'string' ? content : content.value || '').includes('int')));
  };
  await until(hoverHasInt, 'nearby FFI manifest supplies field hover while a sibling script is active');
  await until(()=>vscode.languages.getDiagnostics(plainUri).some(d=>d.message.includes('Unbound variable')), 'sibling environment remains independent');
  assert.equal(vscode.languages.getDiagnostics(typedUri).filter(d=>d.severity===vscode.DiagnosticSeverity.Error).length,0);
  const edit = new vscode.WorkspaceEdit();
  edit.replace(typedUri, new vscode.Range(typedDocument.positionAt(0),typedDocument.positionAt(typedDocument.getText().length)), 'let node = Ext.node "wrong"\n');
  await vscode.workspace.applyEdit(edit);
  await until(()=>vscode.languages.getDiagnostics(typedUri).some(d=>!d.message.includes('Unbound variable')), 'authoritative FFI error in unsaved buffer');
  const marker=path.join(folder.uri.fsPath,'provider-registration.executed');
  fs.rmSync(marker,{force:true});
  fs.writeFileSync(manifest,manifestText+'# saved manifest restart\n');
  await until(()=>fs.existsSync(marker),'automatic environment restarted after manifest save');
  await until(()=>vscode.languages.getDiagnostics(typedUri).some(d=>!d.message.includes('Unbound variable')), 'unsaved buffer survives process restart');
  assert.ok(typedDocument.isDirty);
  const restore=new vscode.WorkspaceEdit();
  restore.replace(typedUri,new vscode.Range(typedDocument.positionAt(0),typedDocument.positionAt(typedDocument.getText().length)),validSource);
  await vscode.workspace.applyEdit(restore);
  await until(async()=>await hoverHasInt()&&vscode.languages.getDiagnostics(typedUri).length===0,'updated unsaved buffer reanalyzed');
  await vscode.window.showTextDocument(document);
  console.log('Real VS Code automatic sibling environments, hover, diagnostics and unsaved-buffer restart passed.');
  const breakpoint = new vscode.SourceBreakpoint(new vscode.Location(program,new vscode.Position(2,4)));
  vscode.debug.addBreakpoints([breakpoint]);
  let resolveStop, rejectStop, resolveEnd;
  const stopped = new Promise((resolve,reject) => { resolveStop = resolve; rejectStop = reject; });
  const ended = new Promise(resolve => { resolveEnd = resolve; });
  const tracker = vscode.debug.registerDebugAdapterTrackerFactory('fscript',{
    createDebugAdapterTracker(session) {
      return {
        onDidSendMessage(message) {
          if (message.event === 'stopped') resolveStop({session,threadId:message.body.threadId});
          if (message.type === 'response' && message.command === 'launch' && !message.success) rejectStop(new Error(message.message));
        }
      };
    }
  });
  const termination = vscode.debug.onDidTerminateDebugSession(session => { if (session.type === 'fscript') resolveEnd(); });
  const timeout = setTimeout(() => rejectStop(new Error('Timed out waiting for real VS Code debugger stop')),30000);
  try {
    assert.equal(await vscode.commands.executeCommand('fscript.debugScript'),true);
    const {session,threadId} = await stopped;
    const stack = await session.customRequest('stackTrace',{threadId});
    assert.equal(stack.stackFrames[0].name,'calculate');
    const frameId = stack.stackFrames[0].id;
    const value = await session.customRequest('evaluate',{frameId,expression:'answer',context:'watch'});
    assert.equal(value.result,'42');
    const scopes = await session.customRequest('scopes',{frameId});
    assert.ok(scopes.scopes.some(scope => scope.name === 'Locals'));
    await session.customRequest('continue',{threadId,singleThread:true});
    await Promise.race([ended,new Promise((_,reject) => setTimeout(() => reject(new Error('Debugger did not terminate')),10000))]);
    console.log('Real VS Code FScript debugger smoke passed.');
    require('node:fs').writeFileSync(process.env.FSCRIPT_VSCODE_SMOKE_RESULT,'passed');
  } finally {
    clearTimeout(timeout); tracker.dispose(); termination.dispose();
    vscode.debug.removeBreakpoints([breakpoint]);
    if (vscode.debug.activeDebugSession?.type === 'fscript') await vscode.debug.stopDebugging();
  }
};
