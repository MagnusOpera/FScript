const path = require('path');

function debugLaunch(configuration, program, folderPath, configurationPath) {
  if (!program || path.extname(program).toLowerCase() !== '.fss') {
    throw new Error('Save an FScript .fss file before starting the debugger.');
  }
  return {
    ...configuration,
    type: 'fscript', request: 'launch', name: configuration.name || 'Debug FScript',
    program: path.resolve(program),
    args: configuration.args || [],
    cwd: configuration.cwd || folderPath || path.dirname(program),
    stopOnEntry: configuration.stopOnEntry === true,
    configurationPath: configurationPath || '',
    rootDirectory: folderPath || path.dirname(program)
  };
}
module.exports = { debugLaunch };
