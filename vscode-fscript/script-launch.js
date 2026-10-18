const path = require('path');

// Keep arguments separate: ProcessExecution bypasses shell interpolation on all platforms.
function scriptLaunch(scriptPath, folderPath, configurationPath) {
  const args = [];
  if (configurationPath) args.push('--config', configurationPath);
  args.push(scriptPath);
  return {
    command: 'fscript',
    args,
    cwd: folderPath || path.dirname(scriptPath)
  };
}
module.exports = { scriptLaunch };
