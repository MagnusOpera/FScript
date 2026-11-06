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
<PropertyGroup><TargetFramework>${framework}</TargetFramework><EnableDefaultCompileItems>false</EnableDefaultCompileItems></PropertyGroup>
<ItemGroup><Compile Include="Tests.cs"/><ProjectReference Include="Middle/Middle.csproj"/></ItemGroup>
<ItemGroup>${packages.join('\n')}</ItemGroup>
</Project>`);
  fs.writeFileSync(path.join(fixture, 'Tests.cs'), `public class Tests {
  [NUnit.Framework.Test] public void Executes() { NUnit.Framework.Assert.That(new FixtureReference.Derived().Value, NUnit.Framework.Is.EqualTo(42)); }
}`);
  for (const directory of ['home', 'tmp', 'Middle', 'Leaf']) fs.mkdirSync(path.join(fixture, directory));
  fs.writeFileSync(path.join(fixture, 'Leaf/Leaf.csproj'), `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><TargetFramework>${framework}</TargetFramework></PropertyGroup></Project>`);
  fs.writeFileSync(path.join(fixture, 'Leaf/Leaf.cs'), 'namespace FixtureReference { public class Leaf { public int Value => 42; } }');
  fs.writeFileSync(path.join(fixture, 'Middle/Middle.csproj'), `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><TargetFramework>${framework}</TargetFramework></PropertyGroup><ItemGroup><ProjectReference Include="../Leaf/Leaf.csproj"/></ItemGroup></Project>`);
  fs.writeFileSync(path.join(fixture, 'Middle/Middle.cs'), 'namespace FixtureReference { public class Derived : Leaf { } }');
  const script = `set -eu
dotnet restore Fixture.csproj --packages /fixture/warm
dotnet build Fixture.csproj --no-restore -c Release
# Keep the direct dependency, but remove the transitive reference outputs.
rm -rf Leaf/bin Leaf/obj bin obj/Release
dotnet restore Fixture.csproj --packages /fixture/warm
set +e
dotnet build Fixture.csproj --no-restore --no-dependencies -c Release > no-dependencies.log 2>&1
reference_status=$?
set -e
echo "Without project-reference builds: exit $reference_status"
test "$reference_status" -ne 0
# Standard SDK reference handling recreates the missing reference assemblies.
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
  console.log('Partial-build regression passed: SDK reference builds and explicit package restore produce executed tests.');
} finally {
  fs.rmSync(fixture, { recursive: true, force: true });
}
