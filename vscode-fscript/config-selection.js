const path = require('path');

// Pure selection policy, shared by the extension controller and its tests.
function selectConfiguration(setting, persisted, candidates, documentPath) {
  if (setting) return { path: setting, source: 'setting' };
  if (persisted?.kind === 'none') return { source: 'explicit none' };
  if (persisted?.kind === 'manifest') return { path: persisted.path, source: 'selection' };
  if (documentPath) {
    const containing = candidates.filter(candidate => {
      const relative = path.relative(path.dirname(candidate), documentPath);
      return relative !== '..' && !relative.startsWith(`..${path.sep}`) && !path.isAbsolute(relative);
    }).sort((a, b) => path.dirname(b).length - path.dirname(a).length);
    return containing.length ? { path: containing[0], source: 'automatic' } : { source: 'default' };
  }
  return { source: 'default' };
}
module.exports = { selectConfiguration };
