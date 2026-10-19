import test from 'node:test';
import assert from 'node:assert/strict';
import {T, extern, HostTypes, HostFunctions, registration, run, parse, infer, evaluate,
  load, invoke, getValue, createSession, submit, resetSession, formatValue, fromJs, toJs} from '../dist/Library.js';

const Node = HostTypes.describe({name: 'Node', fields: {Value: T.int}});
const Point = HostTypes.describe({name: 'Point', fields: {X: T.float}, scriptConstructible: true});
const Mode = HostTypes.describeUnion({name: 'Mode', cases: {Off: null, On: T.named('Node')}});
const types = registration({types: [Node, Point], unions: [Mode]});
const payload = {token: 'private', mutable: [1]};
const node = HostTypes.wrap(types, 'Node', {Value: 7n}, payload);
const make = HostFunctions.create({name: 'Host.node', parameters: [T.int], result: T.named('Node'),
  invoke: ([v]) => HostTypes.wrap(types, 'Node', {Value: v}, payload)});
const identity = HostFunctions.create({name: 'Host.identity', parameters: [T.named('Node')], result: T.named('Node'),
  invoke: ([v]) => {assert.equal(HostTypes.unwrap(types, 'Node', v), payload); return v;}});
const env = registration({types: [Node, Point], unions: [Mode], functions: [make, identity]});
const options = {registration: env};
const phase = expected => e => e.kind === 'fscript-error' && e.phase === expected;

test('host descriptors and complete registrations work through all compiler stages', () => {
  const typed = infer(parse('let read (n: Node) = n.Value\nread (Host.node 7)', options));
  assert.equal(evaluate(typed).value, 7n);
  assert.equal(evaluate(infer(parse('Host.node 7'), env)).typeName, 'Node');
  assert.equal(run('Host.node 7', options).kind, 'host');
  assert.throws(() => run('Host.node "wrong"', options), phase('type'));
  assert.throws(() => run('type Node = { Other: int }\n()', options));
});

test('snapshots hide payloads and survive host/export/container round trips', () => {
  const fields = {Value: 7n};
  const wrapped = HostTypes.wrap(types, 'Node', fields, payload);
  fields.Value = 99n;
  assert.equal(HostTypes.fields(wrapped).Value.value, 7n);
  assert.equal(Object.isFrozen(wrapped), true);
  assert.equal(Object.isFrozen(wrapped.fields), true);
  assert.equal(JSON.stringify(wrapped, (_, v) => typeof v === 'bigint' ? String(v) : v).includes('private'), false);
  assert.equal(HostTypes.unwrap(types, 'Node', toJs(fromJs(wrapped))), payload);
  const loaded = load('export let round (n: Node) = Host.identity n\nexport let n = Host.node 7\nexport let nested n = ([n], (n, Some n), { ["n"] = n }, On n)', options);
  assert.equal(HostTypes.unwrap(types, 'Node', getValue(loaded, 'n')), payload);
  assert.equal(HostTypes.unwrap(types, 'Node', invoke(loaded, 'round', [wrapped])), payload);
  const nested = invoke(loaded, 'nested', [wrapped]);
  for (const v of [nested.values[0].values[0], nested.values[1].values[0], nested.values[1].values[1].value,
    nested.values[2].entries[0].value, nested.values[3].value]) assert.equal(HostTypes.unwrap(types, 'Node', v), payload);
  assert.throws(() => invoke(loaded, 'round', [{Value: 7n}]), /Host value/);
  assert.throws(() => HostTypes.unwrap(types, 'Node', {...wrapped}), /forged/);
  assert.throws(() => fromJs(JSON.parse(JSON.stringify(wrapped, (_, v) => typeof v === 'bigint' ? String(v) : v))), /forged/);
  assert.throws(() => HostTypes.wrap(types, 'Node', {Value: 'wrong'}, payload));
  assert.throws(() => HostTypes.wrap(types, 'Node', {Value: 7n, Hidden: 1}, payload));
});

test('host values allow readers, patterns and recursive visible equality but reject construction and updates', () => {
  assert.equal(run('let read n = n.Value\nread (Host.node 7)', options).value, 7n);
  assert.equal(run('let keep n =\n    let v = n.Value\n    n\nHost.identity (keep (Host.node 7)) |> ignore', options).kind, 'unit');
  assert.equal(run('let (v, n) = ((Host.node 7).Value, Host.node 7)\nv', options).value, 7n);
  assert.equal(run('match Host.node 7 with | { Value = v } -> v', options).value, 7n);
  for (const script of ['let accept (n: Node) = n\naccept { Value = 7 }', 'Host.identity { Value = 7 }',
    '(Host.node 7).Token', 'match Host.node 7 with | { Token = x } -> x',
    'let n = Host.node 7\n{ n with Value = 8 }', 'let update n = { n with Value = 8 }\nupdate (Host.node 7)',
    'let n = Host.node 7\n{| n with Value = 8 |}', 'let update n = {| n with Value = 8 |}\nupdate (Host.node 7)',
    'let reconstruct n = {| Value = n.Value |}\nHost.identity (reconstruct (Host.node 7))'])
    assert.throws(() => run(script, options), phase('type'));
  const other = HostFunctions.create({name: 'Host.other', parameters: [T.unit], result: T.named('Node'),
    invoke: () => HostTypes.wrap(types, 'Node', {Value: 7n}, {different: true})});
  const opts = {...options, externs: [other]};
  assert.equal(run('[Some (Host.node 7)] = [Some (Host.other ())]', opts).value, true);
  assert.equal(run('List.distinct [Host.node 7; Host.other ()] |> List.length', opts).value, 1n);
  assert.equal(formatValue(node).includes('private'), false);
});

test('constructible records and unions have ordinary script semantics', () => {
  const value = run('let p = { X = 1.5 }\n{ p with X = 2.5 }', options);
  assert.equal(HostTypes.unwrap(types, 'Point', value).X.value, 2.5);
  assert.throws(() => HostTypes.wrap(types, 'Point', {X: {kind: 'float', value: 1}}, payload), /hidden payload/);
  assert.equal(run('match On (Host.node 7) with | On n -> n.Value | Off -> 0', options).value, 7n);
  assert.equal(HostTypes.unwrap(types, 'Node', run('On (Host.node 7)', options).value), payload);
});

test('registration validation and runtime external result checks are authoritative', () => {
  assert.throws(() => registration({types: [Node, Node]}), /conflict/);
  assert.throws(() => HostTypes.describe({name: 'int', fields: {}}), /conflict/);
  assert.throws(() => registration({types: [HostTypes.describe({name: 'Bad', fields: {X: T.named('Missing')}})]}), /Unknown/);
  assert.throws(() => registration({unions: [{name: 'Mode', cases: {}}]}));
  assert.throws(() => registration({types: [Node], functions: [make, make]}), /conflict/);
  assert.throws(() => HostFunctions.create({name: 'bad', parameters: [T.var(0)], result: T.var(0), invoke: ([x]) => x}), /closed/);
  const bad = extern({name: 'Host.bad', arity: 1, scheme: T.scheme(T.func(T.unit, T.named('Node'))), invoke: () => ({Value: 7n})});
  assert.throws(() => run('Host.bad ()', {...options, externs: [bad]}), /Host value/);
  const otherType = HostTypes.describe({name: 'Other', fields: {Value: T.int}});
  const otherReg = registration({types: [otherType]});
  const wrong = extern({name: 'Host.wrong', arity: 1, scheme: T.scheme(T.func(T.unit, T.named('Node'))),
    invoke: () => HostTypes.wrap(otherReg, 'Other', {Value: 7n}, payload)});
  assert.throws(() => run('Host.wrong ()', {...options, externs: [wrong]}), /Host value/);
  // An environment without the descriptor cannot use the nominal type from another registration.
  assert.throws(() => run('let accept (n: Node) = n\naccept { Value = 7 }'));
});

test('sessions preserve registrations and opaque payloads through reset', () => {
  const session = createSession(options);
  submit(session, 'let n = Host.node 7');
  submit(session, 'let keep x =\n    let v = x.Value\n    x');
  assert.equal(HostTypes.unwrap(types, 'Node', submit(session, 'Host.identity (keep n)').value), payload);
  resetSession(session);
  assert.throws(() => submit(session, 'n'), phase('type'));
  assert.equal(submit(session, '(Host.node 8).Value').value.value, 8n);
});

test('callbacks use an active invocation context and reject expired handles', () => {
  let saved, savedContext;
  const apply = HostFunctions.create({name: 'Host.apply', parameters: [T.func(T.named('Node'), T.named('Node')), T.named('Node')], result: T.named('Node'),
    invoke: ([fn, n], context) => {
      context.checkRuntime(); saved = fn; savedContext = context;
      return context.apply(fn, [n]);
    }});
  assert.equal(HostTypes.unwrap(types, 'Node', run('Host.apply (fun n -> Host.identity n) (Host.node 7)', {...options, externs: [apply]})), payload);
  assert.throws(() => savedContext.apply(saved, [node]), /Expired/);
  assert.throws(() => fromJs(saved), /Expired/);
  const revive = HostFunctions.create({name: 'Host.revive', parameters: [T.unit], result: T.named('Node'),
    invoke: (_, context) => context.apply(saved, [node])});
  assert.throws(() => run('Host.revive ()', {...options, externs: [revive]}), /Foreign|Expired/);
  const fail = HostFunctions.create({name: 'Host.fail', parameters: [T.func(T.int, T.int)], result: T.int,
    invoke: ([fn], context) => context.apply(fn, [0n])});
  assert.throws(() => run('Host.fail (fun x -> match None with | Some n -> n + x)', {externs: [fail]}), e => e.span?.start.line === 1);
});


test('callbacks reject foreign handles hidden inside containers during a nested host invocation', () => {
  let captured;
  const outer = HostFunctions.create({name: 'Host.outer', parameters: [T.func(T.unit, T.int)], result: T.int,
    invoke: ([fn]) => {
      captured = fn;
      return run('Host.inner ()', {externs: [inner]});
    }});
  const inner = HostFunctions.create({name: 'Host.inner', parameters: [T.unit], result: T.int,
    invoke: (_, ctx) => ctx.apply(captured, [])});
  assert.throws(() => run('Host.outer (fun () -> 1)', {externs: [outer]}), /Foreign/);
  const listInner = HostFunctions.create({name: 'Host.listInner', parameters: [T.unit], result: T.list(T.func(T.unit, T.int)),
    invoke: () => [captured]});
  const listOuter = HostFunctions.create({name: 'Host.listOuter', parameters: [T.func(T.unit, T.int)], result: T.int,
    invoke: ([fn]) => { captured = fn; run('Host.listInner ()', {externs: [listInner]}); return 1n; }});
  assert.throws(() => run('Host.listOuter (fun () -> 1)', {externs: [listOuter]}), /Foreign/);
});

test('type tokens round trip and unsupported task and async operations fail clearly', () => {
  const token = run('typeof Node', options);
  assert.deepEqual(toJs(fromJs(token)), token);
  assert.throws(() => fromJs({...token}), /forged/);
  const task = HostFunctions.create({name: 'Host.task', parameters: [T.func(T.unit, T.int)], result: T.task(T.int),
    invoke: ([fn], ctx) => ctx.spawnTask(fn)});
  assert.throws(() => run('Host.task (fun () -> 1)', {externs: [task]}), /not supported in Fable/);
  const asyncFn = HostFunctions.create({name: 'Host.async', parameters: [T.unit], result: T.int,
    invoke: async () => 1n});
  assert.throws(() => run('Host.async ()', {externs: [asyncFn]}), /synchronously/);
});


test('visible snapshots freeze nested output and payload mutation stays explicit', () => {
  const Box = HostTypes.describe({name: 'Box', fields: {Items: T.list(T.int)}});
  const boxes = registration({types: [Box]});
  const data = [1n, 2n];
  const state = {value: 1};
  const box = HostTypes.wrap(boxes, 'Box', {Items: data}, state);
  data[0] = 99n;
  assert.equal(box.fields.Items.values[0].value, 1n);
  assert.throws(() => { box.fields.Items.values[0].value = 99n; }, TypeError);
  HostTypes.unwrap(boxes, 'Box', box).value = 2;
  assert.equal(state.value, 2);
  assert.equal(box.fields.Items.values[0].value, 1n);
  assert.throws(() => HostTypes.describe({name: 'Bad', fields: {}, scriptConstructible: 'yes'}), /boolean/);
  assert.throws(() => HostTypes.describe({name: 'Bad', fields: null}), /fields/);
  assert.throws(() => registration({types: [{name: 'Bad', fields: {X: T.var(0)}}]}), /closed/);
  assert.throws(() => registration({unions: [{name: 'BadUnion', cases: {Case: T.var(0)}}]}), /closed/);
});


test('union helpers validate registered cases and nested opaque payloads', () => {
  const on = HostTypes.wrapUnion(types, 'Mode', 'On', node);
  assert.equal(HostTypes.unwrap(types, 'Node', HostTypes.unwrapUnion(types, 'Mode', on).value), payload);
  assert.equal(HostTypes.wrapUnion(types, 'Mode', 'Off').value, null);
  assert.throws(() => HostTypes.wrapUnion(types, 'Mode', 'Missing'), /Host value/);
  assert.throws(() => HostTypes.wrapUnion(types, 'Mode', 'On', {Value: 7n}), /Host value/);
});

test('callback arguments are validated against declared nominal and container types', () => {
  const bad = HostFunctions.create({name: 'Host.badCallback', parameters: [T.func(T.named('Node'), T.int)], result: T.int,
    invoke: ([fn], ctx) => ctx.apply(fn, [{Value: 7n}])});
  assert.throws(() => run('Host.badCallback (fun n -> n.Value)', {...options, externs: [bad]}), /Host value/);
  const nested = HostFunctions.create({name: 'Host.nestedCallback', parameters: [T.list(T.func(T.named('Node'), T.int))], result: T.int,
    invoke: ([list], ctx) => ctx.apply(list.values[0], [{Value: 7n}])});
  assert.throws(() => run('Host.nestedCallback [fun n -> n.Value]', {...options, externs: [nested]}), /Host value/);
  const partial = HostFunctions.create({name: 'Host.partialCallback', parameters: [T.func(T.named('Node'), T.func(T.int, T.int))], result: T.int,
    invoke: ([fn], ctx) => ctx.apply(ctx.apply(fn, [node]), ['wrong'])});
  assert.throws(() => run('Host.partialCallback (fun n k -> n.Value + k)', {...options, externs: [partial]}), /Host value/);
});

test('registered environments remain local through virtual imports and stage overrides', () => {
  const source = 'import "node.fss" as N\nN.read (Host.node 7)';
  const opts = {...options, sources: {'/node.fss': 'let read (n: Node) = n.Value'}};
  assert.equal(run(source, opts).value, 7n);
  assert.throws(() => run(source, {sources: opts.sources}));
  const typed = infer(parse('Host.node 7', options), [make]);
  assert.equal(evaluate(typed, [make]).fields.Value.value, 7n);
  assert.throws(() => registration({functions: [extern({name: 'bad', arity: 2, scheme: T.scheme(T.func(T.int, T.int)), invoke: () => 1n})]}), /Scheme/);
});

test('the JavaScript hosting sample executes with stable output', async () => {
  const {execFileSync} = await import('node:child_process');
  const output = execFileSync(process.execPath, [new URL('../examples/hosting.mjs', import.meta.url).pathname], {encoding: 'utf8'});
  assert.equal(output.trim(), '41\n42\n40\n42');
});


test('field snapshots preserve JavaScript prototype-like names without exposing interpreter state', () => {
  const fieldTypes = Object.fromEntries([['__proto__', T.int], ['constructor', T.string]]);
  const type = HostTypes.describe({name: 'PrototypeFields', fields: fieldTypes});
  const env = registration({types: [type]});
  const value = HostTypes.wrap(env, 'PrototypeFields', Object.fromEntries([['__proto__', 1n], ['constructor', 'ok']]), {});
  assert.equal(value.fields.__proto__.value, 1n);
  assert.equal(value.fields.constructor.value, 'ok');
  assert.equal(HostTypes.fields(value).__proto__.value, 1n);
  for (const [kind, value] of [['float', 'bad'], ['bool', 1], ['string', false]]) {
    assert.throws(() => fromJs({kind, value}), /Expected a JavaScript/);
  }
  const bad = HostFunctions.create({name: 'Host.asyncList', parameters: [T.unit], result: T.list(T.record({})),
    invoke: () => [Promise.resolve({})]});
  assert.throws(() => run('Host.asyncList ()', {externs: [bad]}), /synchronously/);
});

test('runtime guards both update forms when inference is deliberately bypassed', () => {
  const plainType = HostTypes.describe({name: 'Node', fields: {Value: T.int}, scriptConstructible: true});
  const plainMake = HostFunctions.create({name: 'Host.node', parameters: [T.int], result: T.named('Node'),
    invoke: ([v]) => ({Value: v})});
  const plain = registration({types: [plainType], functions: [plainMake]});
  for (const update of ['{ n with Value = 8 }', '{| n with Value = 8 |}']) {
    const typed = infer(parse(`let n = Host.node 7\n${update}`, {registration: plain}));
    assert.throws(() => evaluate(typed, env), phase('eval'));
  }
});


test('open callback schemes fail explicitly instead of weakening nominal validation', () => {
  const a = T.var(0);
  const open = extern({name: 'Host.open', arity: 2,
    scheme: T.forall([0], T.func(T.func(a, T.int), T.func(a, T.int))),
    invoke: ([fn], ctx) => ctx.apply(fn, [{Value: 7n}])});
  assert.throws(() => run('Host.open (fun n -> n.Value) (Host.node 7)', {...options, externs: [open]}), /closed function type/);
});


test('host wrapping validates map key types recursively', () => {
  const type = HostTypes.describe({name: 'Indexed', fields: {Items: T.mapOf(T.int, T.named('Node'))}});
  const env = registration({types: [Node, type]});
  const item = {kind: 'map', entries: [{key: {kind: 'int', value: 1n}, value: node}]};
  assert.equal(HostTypes.wrap(env, 'Indexed', {Items: item}, {}).fields.Items.entries[0].key.value, 1n);
  const invalid = {kind: 'map', entries: [{key: {kind: 'string', value: 'wrong'}, value: node}]};
  assert.throws(() => HostTypes.wrap(env, 'Indexed', {Items: invalid}, {}), /Host value/);
});
