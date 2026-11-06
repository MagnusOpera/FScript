import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { verifyTestReports } from './verify-test-reports.mjs';

// Run inside the exact toolchain selected by the build, without requiring a
// host SDK. Package versions follow the repository's test project.
const graph = JSON.parse(fs.readFileSync('terrabuild-debug.json', 'utf8'));
const operation = graph.nodes.flatMap(node => node.resolvedOperations)
  .find(operation => operation.metaCommand === '@dotnet build');
assert.ok(operation?.container, 'Build diagnostics must identify the .NET toolchain image.');
const project = fs.readFileSync('samples/ffi/Tests/FfiSample.Tests.fsproj', 'utf8');
const packages = ['Microsoft.NET.Test.Sdk', 'NUnit', 'NUnit3TestAdapter'].map(name => {
  const reference = [...project.matchAll(/<PackageReference Include="([^"]+)" Version="([^"]+)"\s*\/>/g)]
    .find(match => match[1] === name);
  assert.ok(reference, `Missing repository test dependency: ${name}`);
  return `<PackageReference Include="${name}" Version="${reference[2]}" />`;
});
const framework = fs.readFileSync('Directory.Build.props', 'utf8').match(/<TargetFramework>([^<]+)<\/TargetFramework>/)?.[1];
assert.ok(framework);
const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript-dotnet-packages-'));
try {
  fs.writeFileSync(path.join(fixture, 'Fixture.csproj'), `<Project Sdk="Microsoft.NET.Sdk">
<PropertyGroup><TargetFramework>${framework}</TargetFramework></PropertyGroup>
<ItemGroup>${packages.join('\n')}</ItemGroup>
</Project>`);
  fs.writeFileSync(path.join(fixture, 'Tests.cs'), `public class Tests {
  [NUnit.Framework.Test] public void Executes() { NUnit.Framework.Assert.That(true); }
}`);
  for (const directory of ['home', 'tmp']) fs.mkdirSync(path.join(fixture, directory));
  const script = `set -eu
dotnet restore Fixture.csproj --packages /fixture/warm
dotnet build Fixture.csproj --no-restore -c Release
mv warm unavailable-packages
# Cached bin/obj refer to a package directory absent on a new runner.
set +e
dotnet test Fixture.csproj --no-build --no-restore -c Release --logger 'trx;LogFileName=baseline.trx' --results-directory baseline
baseline_status=$?
set -e
echo "Without package imports: exit $baseline_status"
test ! -f baseline/baseline.trx
# The corrected graph restores packages explicitly before using --no-build.
dotnet restore Fixture.csproj --packages /fixture/warm
dotnet test Fixture.csproj --no-build --no-restore -c Release --logger 'trx;LogFileName=results.trx' --results-directory fixed
`;
  execFileSync('docker', ['run', '--rm', '--user', `${process.getuid()}:${process.getgid()}`,
    '-e', 'HOME=/fixture/home', '-e', 'DOTNET_CLI_HOME=/fixture/home', '-e', 'TMPDIR=/fixture/tmp',
    '-v', `${fixture}:/fixture`, '-w', '/fixture', '--entrypoint', 'bash', operation.container,
    '-c', script], { stdio: 'inherit', timeout: 180000 });
  verifyTestReports([path.join(fixture, 'fixed/results.trx')]);
  console.log('Fresh-package-cache regression passed: restored binaries execute tests after explicit package restore.');
} finally {
  fs.rmSync(fixture, { recursive: true, force: true });
}
