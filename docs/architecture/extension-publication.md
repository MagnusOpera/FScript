# VS Code extension publication

[FScriptLang/vscode-extension](https://github.com/FScriptLang/vscode-extension)
owns the VS Code extension, its grammar and client tests, real editor smoke
test, VSIX packaging, and marketplace publication. The publisher and extension
identifier remain `MagnusOpera.fscript`, under the Magnus Opera umbrella.

FScript retains `FScript.LanguageServer`, its server-side tests, the CLI debug
adapter, and debugger protocol tests. Its releases distribute platform binaries,
NuGet packages, npm, and the Homebrew tap. They neither package VSIX files nor
require `VSCE_TOKEN` or `OPEN_VSX_TOKEN`. Client/grammar checks run in the
extension repository; the FScript web shard checks only the JavaScript package.

The extension builds its bundled language server from a pinned public FScript
commit. Its `language-server.json` records that immutable revision, numeric
language version, .NET SDK, and acquired runtime. Packaging passes the pinned
language version to `dotnet publish` and runs FScript's server assembly and
provider-loading regressions against the staged binaries before generating a
VSIX. This preserves assembly compatibility with NuGet-based providers without
coupling extension releases to language releases or requiring a cross-repository
token. It initially pins the source of the shipped 0.94.3 release.

Extension main pushes and pull requests build a testable VSIX. Its own canonical
`vX.Y.Z` tags validate the committed extension version and extension changelog,
build and test the package, create a GitHub release, and publish the same VSIX
to both registries. The extension version and pinned language-server version
can advance independently. VS Code Marketplace uses GitHub OIDC Trusted
Publishing bound to that repository's `release.yml`, with job-scoped
`id-token: write` and `vsce publish --oidc`. No Marketplace token is used.
Only Open VSX requires the `OPEN_VSX_TOKEN` secret in the extension repository.
Existing Git tags, GitHub releases, marketplace versions, and publisher identity
are not migrated or rewritten. The first independent publication must use a new
extension version rather than republishing 0.94.3.

For local language development, configure the installed extension's
`fscript.server.path` and `fscript.cli.path` to FScript build outputs. Extension
development instructions and real editor smoke tests live in its repository.
