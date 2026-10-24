#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
repo=$(pwd)
if [[ -n "${VSCODE_EXECUTABLE:-}" ]]; then
  vscode_executable="$VSCODE_EXECUTABLE"
elif [[ -x '/Applications/Visual Studio Code.app/Contents/MacOS/Code' ]]; then
  vscode_executable='/Applications/Visual Studio Code.app/Contents/MacOS/Code'
elif [[ -x '/Applications/Visual Studio Code.app/Contents/MacOS/Electron' ]]; then
  vscode_executable='/Applications/Visual Studio Code.app/Contents/MacOS/Electron'
else
  vscode_executable=$(command -v code)
fi
fixture=$(mktemp -d)
trap 'if [[ ! -f "$fixture/result" ]]; then mkdir -p "$repo/.out/vscode-routing-failure"; cp -R "$fixture/user-data/logs" "$repo/.out/vscode-routing-failure/" 2>/dev/null || true; fi; rm -rf "$fixture"' EXIT
mkdir -p "$fixture/workspace/.vscode" "$fixture/extensions"
for dependency in "$HOME"/.vscode/extensions/ms-dotnettools.vscode-dotnet-runtime-*; do
  if [[ -d "$dependency" ]]; then ln -s "$dependency" "$fixture/extensions/$(basename "$dependency")"; fi
done
cat > "$fixture/workspace/main.fss" <<'SCRIPT'
let calculate value =
    let answer = value + 1
    answer * 2
calculate 41
SCRIPT
python3 - "$fixture/workspace/.vscode/settings.json" "$repo/src/FScript/bin/Release/net10.0/fscript.dll" <<'PYSETTINGS'
import json,pathlib,sys
pathlib.Path(sys.argv[1]).write_text(json.dumps({
    'fscript.lsp.enabled': True,
    'fscript.server.path': str(pathlib.Path(sys.argv[2]).parents[5] / 'src/FScript.LanguageServer/bin/Release/net10.0/FScript.LanguageServer.dll'),
    'fscript.cli.path': sys.argv[2],
    'terminal.integrated.enablePersistentSessions': False
}))
PYSETTINGS
FSCRIPT_VSCODE_SMOKE_RESULT="$fixture/result" "$vscode_executable" \
  --user-data-dir "$fixture/user-data" \
  --extensions-dir "$fixture/extensions" \
  --extensionDevelopmentPath="$repo/vscode-fscript" \
  --extensionTestsPath="$repo/tests/FScript.VscodeDebug.Tests" \
  --disable-workspace-trust --skip-welcome --skip-release-notes \
  --new-window "$fixture/workspace"

test -f "$fixture/result" || { echo "VS Code did not complete its debugger smoke test." >&2; exit 1; }
