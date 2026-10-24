const path = require('path');

// A single executable or managed CLI DLL, never a shell command.
function cliCommand(configuredPath, folderPath, program) {
  const selected = (configuredPath || '').trim() || 'fscript';
  const managed = selected.toLowerCase().endsWith('.dll');
  const filePath = managed || selected.includes('/') || selected.includes('\\');
  const executable = filePath
    ? path.resolve(folderPath || (program && path.dirname(program)) || process.cwd(), selected)
    : selected;
  return managed ? { command: 'dotnet', args: [executable] } : { command: executable, args: [] };
}
module.exports = { cliCommand };
