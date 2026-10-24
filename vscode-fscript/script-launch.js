const path = require('path');
const { cliCommand } = require('./cli-command');

// Keep arguments separate: ProcessExecution bypasses shell interpolation on all platforms.
function scriptLaunch(scriptPath, folderPath, configurationPath, configuredCli) {
  const cli = cliCommand(configuredCli, folderPath, scriptPath);
  const args = [...cli.args];
  if (configurationPath) args.push('--config', configurationPath);
  else args.push('--no-config');
  args.push(scriptPath);
  return {
    command: cli.command,
    args,
    cwd: folderPath || path.dirname(scriptPath)
  };
}
module.exports = { scriptLaunch };
