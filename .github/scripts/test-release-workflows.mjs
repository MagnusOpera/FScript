import assert from 'node:assert/strict';
import {readFileSync, existsSync} from 'node:fs';
import {load} from 'js-yaml';
import {test} from 'node:test';

const workflow = (name) => load(readFileSync(new URL(`../workflows/${name}.yml`, import.meta.url), 'utf8'));
const list = (value) => value === undefined ? [] : Array.isArray(value) ? value : [value];
const stepUsing = (job, action) => job.steps.find((step) => step.uses?.startsWith(action));
function dependsOn(jobs, job, prerequisite, seen = new Set()) {
  assert.ok(jobs[job], `Missing job ${job}`);
  if (seen.has(job)) return false;
  seen.add(job);
  return list(jobs[job].needs).some((name) => name === prerequisite || dependsOn(jobs, name, prerequisite, seen));
}

const tagged = workflow('on-push-tag');
const publishing = workflow('on-release-published');

test('a tag directly invokes all publication stages without a release event or manual draft', () => {
  assert.deepEqual(tagged.on.push.tags, ['v[0-9]*.[0-9]*.[0-9]*']);
  assert.equal(tagged.env.RELEASE_TAG, '${{ github.ref_name }}');
  for (const job of ['package-release', 'verify-macos-artifact', 'sign-and-notarize']) {
    assert.equal(tagged.jobs[job].env.BUILD_VERSION, '${{ needs.validate-release.outputs.version }}');
  }
  const release = tagged.jobs['create-release'];
  for (const gate of ['validate-release', 'release-ci', 'verify-macos-artifact', 'sign-and-notarize']) {
    assert.ok(dependsOn(tagged.jobs, 'create-release', gate), `Public release must wait for ${gate}`);
  }
  assert.equal(stepUsing(release, 'softprops/action-gh-release').with.draft, false);
  assert.ok(dependsOn(tagged.jobs, 'publish-artifacts', 'create-release'));
  assert.equal(tagged.jobs['publish-artifacts'].uses, './.github/workflows/on-release-published.yml');
  assert.equal(tagged.jobs['publish-artifacts'].permissions['id-token'], 'write');
  assert.equal(tagged.jobs['publish-artifacts'].permissions.pages, undefined);
  assert.ok(publishing.on.workflow_call.inputs.release_tag.required);
  assert.ok(publishing.on.workflow_dispatch.inputs.release_tag.required);
  assert.equal(publishing.on.release, undefined, 'Publication must not depend on a recursive release event');
});

test('only signed and notarized macOS output can reach the public release', () => {
  const signing = tagged.jobs['sign-and-notarize'];
  assert.equal(stepUsing(signing, 'actions/download-artifact').with.name, 'release-assets-macos');
  assert.ok(signing.steps.some((step) => step.run?.includes('codesign --verify')));
  assert.ok(signing.steps.some((step) => step.run?.includes('notarytool submit') && step.run.includes('Accepted')));
  assert.equal(stepUsing(signing, 'actions/upload-artifact').with.name, 'release-assets-signed-macos');
  const downloads = tagged.jobs['create-release'].steps.filter((step) => step.uses?.startsWith('actions/download-artifact'));
  assert.deepEqual(downloads.map((step) => step.with.name).sort(), ['release-assets-packages', 'release-assets-signed-macos']);
  assert.ok(!downloads.some((step) => step.with.pattern), 'Wildcard downloads could expose unsigned staging assets');
});

test('all registries support direct invocation and retries against the explicit published tag', () => {
  const jobs = publishing.jobs;
  for (const name of ['release-nuget', 'release-npm']) {
    const job = jobs[name];
    assert.equal(job.if, undefined, `${name} must also run for workflow_call and workflow_dispatch`);
    assert.ok(dependsOn(jobs, name, 'validate-published-release'));
    const checkout = stepUsing(job, 'actions/checkout');
    assert.match(checkout.with.ref, /refs\/tags/);
    assert.match(checkout.with.ref, /inputs.release_tag/);
    assert.equal(stepUsing(job, 'robinraju/release-downloader').with.tag, '${{ env.RELEASE_TAG }}');
    assert.ok(dependsOn(jobs, 'update-homebrew-tap', name), `Tap must wait for ${name}`);
  }
  assert.ok(jobs['validate-published-release'].steps.some((step) => step.run?.includes('--json isDraft')));
  assert.equal(stepUsing(jobs['release-nuget'], 'NuGet/login').with.user, 'pct-magnusopera');
  assert.equal(jobs['release-nuget'].permissions['id-token'], 'write');
  assert.equal(jobs['release-npm'].env.RELEASE_VERSION, '${{ needs.validate-published-release.outputs.version }}');
  assert.match(jobs['update-homebrew-tap'].with.release_tag, /inputs.release_tag/);
  assert.equal(jobs['update-homebrew-tap'].with.version, undefined);
});

test('language CI and publication have no website checkout, deployment, or Pages permissions', () => {
  assert.equal(existsSync(new URL('../workflows/publish-website.yml', import.meta.url)), false);
  assert.equal(existsSync(new URL('../../website/PROJECT', import.meta.url)), false);
  for (const name of ['on-push-tag', 'on-release-published', 'ci']) {
    const parsed = workflow(name);
    assert.equal(parsed.permissions?.pages, undefined);
    for (const job of Object.values(parsed.jobs)) {
      assert.equal(job.permissions?.pages, undefined);
      assert.doesNotMatch(JSON.stringify(job), /publish-website|deploy-pages|configure-pages|website\/|make website/);
    }
  }
  const shards = JSON.parse(readFileSync(new URL('../../.build/ci-shards.json', import.meta.url), 'utf8'));
  assert.ok(shards.include.every(shard => shard.runs.every(run => !run.projects.includes('website'))));
});

test('extension packaging and registry credentials belong to the standalone extension repository', () => {
  assert.equal(existsSync(new URL('../../vscode-fscript/PROJECT', import.meta.url)), false);
  for (const name of ['on-push-tag', 'on-release-published', 'ci']) {
    assert.doesNotMatch(JSON.stringify(workflow(name)), /vscode-fscript|\.vsix|VSCE_TOKEN|OPEN_VSX_TOKEN|publish-vscode-marketplace|publish-open-vsx/);
  }
  assert.deepEqual(list(publishing.jobs['update-homebrew-tap'].needs), ['release-nuget', 'release-npm']);
  const shards = JSON.parse(readFileSync(new URL('../../.build/ci-shards.json', import.meta.url), 'utf8'));
  assert.ok(shards.include.every(shard => shard.runs.every(run => !run.projects.includes('vscode'))));
});
