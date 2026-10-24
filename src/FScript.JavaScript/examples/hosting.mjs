// Run after `make fable-build`: node src/FScript.JavaScript/examples/hosting.mjs
// In an installed application, import from "@magnusopera/fscript" instead.
import {T, HostTypes, HostFunctions, registration, load, invoke, getValue} from '../dist/Library.js';

const counterType = HostTypes.describe({name: 'Counter', fields: {Initial: T.int}});
const pointType = HostTypes.describe({name: 'Point', fields: {X: T.int}, scriptConstructible: true});
const types = registration({types: [counterType, pointType]});
const create = HostFunctions.create({
  name: 'Counter.create', parameters: [T.int], result: T.named('Counter'),
  invoke([initial]) {
    return HostTypes.wrap(types, 'Counter', {Initial: initial}, {current: initial.value});
  },
});
const next = HostFunctions.create({
  name: 'Counter.next', parameters: [T.named('Counter')], result: T.int,
  invoke([counter]) {
    // Mutation is explicit. The visible Initial snapshot stays unchanged.
    const state = HostTypes.unwrap(types, 'Counter', counter);
    state.current += 1n;
    return state.current;
  },
});
const apply = HostFunctions.create({
  name: 'Host.apply', parameters: [T.func(T.int, T.int), T.int], result: T.int,
  invoke([fn, value], context) { return context.apply(fn, [value]); },
});
const environment = registration({types: [counterType, pointType], functions: [create, next, apply]});
const script = load(`
export let counter = Counter.create 40
export let advance () = Counter.next counter
export let point = { X = Host.apply (fun n -> n + 1) 41 }
`, {registration: environment});
console.log(invoke(script, 'advance', [null]).value.toString()); // 41
console.log(invoke(script, 'advance', [null]).value.toString()); // 42
console.log(getValue(script, 'counter').fields.Initial.value.toString()); // 40
console.log(HostTypes.unwrap(types, 'Point', getValue(script, 'point')).X.value.toString()); // 42
