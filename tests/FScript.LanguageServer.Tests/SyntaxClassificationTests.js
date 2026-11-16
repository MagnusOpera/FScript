const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");

const grammar = JSON.parse(fs.readFileSync(path.join(__dirname, "../../vscode-fscript/syntaxes/fscript.tmLanguage.json"), "utf8"));
const declarations = grammar.repository.declarations.patterns;
const prefix = declarations.find(pattern => pattern.name === "keyword.control.fscript");
const binding = declarations.find(pattern => pattern.name === "meta.let.binding.fscript");
const typePatterns = grammar.repository.types.patterns;

test("TextMate highlights generic parameters, applications and postfix task types", () => {
  const parameter = typePatterns.find(pattern => pattern.name === "entity.name.type.parameter.fscript");
  const parameters = new RegExp(parameter.match, "gu");
  const source = "let copy<'item> (items: 'item list) = items'";
  assert.deepEqual([...source.matchAll(parameters)].map(match => match[0]), ["'item", "'item"]);

  const application = typePatterns.find(pattern => pattern.name === "meta.type.arguments.fscript");
  const opening = new RegExp(application.begin, "gu");
  assert.equal([..."Box<Box<int list>>".matchAll(opening)].length, 2);
  assert.equal([..."copy<int> [42]".matchAll(opening)].length, 1);
  assert.equal([..."copy<(int -> int)> fn".matchAll(opening)].length, 1);
  assert.equal([..."left < right && a>b".matchAll(opening)].length, 0);
  assert.equal([..."left<right".matchAll(opening)].length, 0);
  assert.equal(new RegExp(application.end).exec("int -> int>")[0], ">");
  const builtIn = typePatterns.find(pattern => pattern.name === "storage.type.fscript");
  assert.deepEqual("int task".match(new RegExp(builtIn.match, "g")), ["int", "task"]);
});

test("TextMate classifies the contextual export prefix for values, functions and recursive groups", () => {
  for (const source of ["export let value = 42", "export let run x = x", "export let rec run x = run x", "  export\tlet export = 42"]) {
    const match = new RegExp(prefix.match).exec(source);
    assert.ok(match, source);
    assert.equal(match[0], "export");
  }
});

test("TextMate preserves export binding, parameter, field and expression identifiers", () => {
  for (const source of ["let export = 42", "let id export = export", "export", "record.export", "let exporting = 1", "export\nlet value = 42"]) {
    assert.equal(new RegExp(prefix.match).test(source), false, source);
  }
  const match = new RegExp(binding.match).exec("export let export = 42");
  assert.equal(match[3], "export");
  assert.equal(binding.captures[3].name, "entity.name.function.fscript");
  assert.equal(grammar.repository.attributes, undefined);
  assert.equal(grammar.patterns.some(pattern => pattern.include === "#attributes"), false);
});
