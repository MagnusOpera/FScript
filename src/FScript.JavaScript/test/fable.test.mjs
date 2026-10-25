import test from "node:test";
import assert from "node:assert/strict";
import {
  T,
  extern,
  run,
  load,
  invoke,
  listFunctions,
  listValues,
  getValue,
  parse,
  infer,
  evaluate,
  formatValue,
  fromJs,
  toJs,
  createSession,
  resetSession,
  submit
} from "../dist/Library.js";

test("run returns tagged primitive values", () => {
  const result = run("let x = 41\nx + 1");
  assert.equal(result.kind, "int");
  assert.equal(result.value, 42n);
});

test("map keys round-trip through tagged JavaScript values", () => {
  const value = run("type Color = | Red | Blue\n{ [Red] = 1; [Blue] = 2 }");
  assert.equal(value.kind, "map");
  assert.deepEqual(value.entries.map(entry => entry.key.caseName).sort(), ["Blue", "Red"]);
  assert.deepEqual(toJs(fromJs(value)), value);
  const boolMap = run("{ [true] = 3 }");
  assert.deepEqual(toJs(fromJs(boolMap)), boolMap);
});

test("load and invoke exported functions repeatedly", () => {
  const script = "export let add x y = x + y\nexport let answer = 42";
  const loaded = load(script);
  assert.deepEqual(listFunctions(loaded), ["add"]);
  assert.deepEqual(listValues(loaded), ["answer"]);
  assert.equal(getValue(loaded, "answer").value, 42n);
  assert.equal(invoke(loaded, "add", [1, 2]).value, 3n);
  assert.equal(invoke(loaded, "add", [10n, 20n]).value, 30n);
});

test("virtual imports resolve from an in-memory source map", () => {
  const loaded = load("import \"shared.fss\" as Shared\nexport let value = Shared.inc 41", {
    rootDirectory: "/",
    entryFile: "/main.fss",
    sources: {
      "/shared.fss": "let inc x = x + 1"
    }
  });
  assert.equal(getValue(loaded, "value").value, 42n);
});

test("parse, infer, and evaluate handles compose", () => {
  const program = parse("let x = 20\nx + 22");
  const typed = infer(program);
  const result = evaluate(typed);
  assert.equal(result.kind, "int");
  assert.equal(result.value, 42n);
  assert.equal(formatValue(result), "42");
});

test("session submissions retain declarations", () => {
  const session = createSession();
  const first = submit(session, "let add x y = x + y");
  assert.equal(first.hasValue, false);
  assert.equal(first.retainedCount, 1);
  const second = submit(session, "add 20 22");
  assert.equal(second.hasValue, true);
  assert.equal(second.value.value, 42n);
  resetSession(session);
  assert.throws(() => submit(session, "add 1 2"), error => {
    assert.equal(error.kind, "fscript-error");
    assert.equal(error.phase, "type");
    return true;
  });
});

test("numeric constraints persist across session applications", () => {
  const session = createSession();
  submit(session, "let add x y = x + y");
  assert.equal(submit(session, "add 1 2").value.value, 3n);
  assert.equal(submit(session, "add 1.5 2.6").value.value, 4.1);
  for (const call of ['add "toto" "titi"', "add true false", "add 1 2.6"]) {
    assert.throws(() => submit(session, call), error => {
      assert.equal(error.phase, "type");
      return true;
    });
  }
  assert.equal(submit(session, "add 3 4").value.value, 7n);
});

test("errors are structured and span-aware", () => {
  assert.throws(
    () => run("let = 1"),
    error => {
      assert.equal(error.kind, "fscript-error");
      assert.equal(error.phase, "parse");
      assert.equal(typeof error.message, "string");
      assert.equal(error.span.start.line, 1);
      return true;
    }
  );
});

test("JavaScript externs round-trip typed arguments and results", () => {
  const shout = extern({
    name: "Host.shout",
    arity: 1,
    scheme: T.scheme(T.func(T.string, T.string)),
    invoke(args) {
      assert.deepEqual(args, [{ kind: "string", value: "fable" }]);
      return { kind: "string", value: `${args[0].value}!` };
    }
  });
  const result = run("Host.shout \"fable\"", { externs: [shout] });
  assert.deepEqual(result, { kind: "string", value: "fable!" });
});


test("postfix map annotations preserve numeric key inference", () => {
  assert.equal(run('let get (m: string map[int]) = m[0]\nget { [0] = "ok" }').value.value, "ok");
  const result = run('let make k = { [k + k] = "ok" }\nmake 2');
  assert.deepEqual(result.entries[0].key, { kind: "int", value: 4n });
  assert.throws(() => run('let make k = { [k + k] = "ok" }\nmake 2.5'), error => error.phase === "type");
});


test("export is a contextual declaration prefix rather than a reserved identifier", () => {
  const loaded = load("let export = 40\nexport let answer = export + 2\nexport let export export = export");
  assert.equal(getValue(loaded, "answer").value, 42n);
  assert.equal(invoke(loaded, "export", [7]).value, 7n);
  assert.throws(() => run("[<export>] let answer = 42"), error => error.phase === "parse");
  assert.throws(() => run("let outer =\n  export let answer = 42\n  answer\nouter"), error => error.phase === "parse");
});

test("attribute forms cannot declare exports", () => {
  for (const attribute of ["export", "Export"]) {
    for (const source of [
      `[<${attribute}>] let answer = 42`,
      `[<${attribute}>] let echo value = value`,
      `[<${attribute}("argument")>] let answer = 42`,
      `[<${attribute}>] type Answer = { Value: int }`
    ]) {
      assert.throws(() => load(source), error => error.phase === "parse" && error.message === "Unexpected attribute");
    }
  }
});
