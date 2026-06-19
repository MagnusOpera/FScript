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
  createSession,
  resetSession,
  submit
} from "../dist/Library.js";

test("run returns tagged primitive values", () => {
  const result = run("let x = 41\nx + 1");
  assert.equal(result.kind, "int");
  assert.equal(result.value, 42n);
});

test("load and invoke exported functions repeatedly", () => {
  const script = "[<export>] let add x y = x + y\n[<export>] let answer = 42";
  const loaded = load(script);
  assert.deepEqual(listFunctions(loaded), ["add"]);
  assert.deepEqual(listValues(loaded), ["answer"]);
  assert.equal(getValue(loaded, "answer").value, 42n);
  assert.equal(invoke(loaded, "add", [1, 2]).value, 3n);
  assert.equal(invoke(loaded, "add", [10n, 20n]).value, 30n);
});

test("virtual imports resolve from an in-memory source map", () => {
  const loaded = load("import \"shared.fss\" as Shared\n[<export>] let value = Shared.inc 41", {
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
