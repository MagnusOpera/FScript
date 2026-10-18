const path = require('path');

// Keep arguments separate: ProcessExecution bypasses shell interpolation on all platforms.
function scriptLaunch(scriptPath, folderPath, configurationPath, localCliProject) {
  const args = [];
  if (configurationPath) args.push('--config', configurationPath);
  args.push(scriptPath);
  return {
    command: localCliProject ? 'dotnet' : 'fscript',
    args: localCliProject ? ['run', '--project', localCliProject, '--', ...args] : args,
    cwd: folderPath || path.dirname(scriptPath)
  };
}
module.exports = { scriptLaunch };
