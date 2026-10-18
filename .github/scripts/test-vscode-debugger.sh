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
trap 'rm -rf "$fixture"' EXIT
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
    'fscript.lsp.enabled': False,
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
