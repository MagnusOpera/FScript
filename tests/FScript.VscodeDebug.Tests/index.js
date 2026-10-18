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
    assert.equal(await vscode.debug.startDebugging(folder,{ type:'fscript',request:'launch',name:'FScript smoke',program:program.fsPath }),true);
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
