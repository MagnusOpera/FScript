// Pure selection policy, shared by the extension controller and its tests.
function selectConfiguration(setting, persisted, candidates) {
  if (setting) return { path: setting, source: 'setting' };
  if (persisted?.kind === 'none') return { source: 'explicit none' };
  if (persisted?.kind === 'manifest') return { path: persisted.path, source: 'selection' };
  if (candidates.length === 1) return { path: candidates[0], source: 'automatic' };
  return { source: candidates.length > 1 ? 'selection needed' : 'default' };
}
module.exports = { selectConfiguration };
