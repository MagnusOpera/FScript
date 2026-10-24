const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawn } = require('node:child_process');

const root = path.resolve(__dirname, '../..');
const cli = process.env.FSCRIPT_DEBUG_CLI || path.join(root, 'src/FScript/bin/Release/net10.0/fscript.dll');
class Client {
  constructor() {
    this.process = spawn(cli.endsWith('.dll') ? 'dotnet' : cli, cli.endsWith('.dll') ? [cli, '--debug-adapter'] : ['--debug-adapter'], { cwd: root, stdio: ['pipe', 'pipe', 'pipe'] });
    this.buffer = Buffer.alloc(0); this.seq = 0; this.pending = new Map(); this.messages = []; this.waits = [];
    this.errors = ''; this.children = [];
    this.process.stderr.on('data', data => { this.errors += data; });
    this.process.stdout.on('data', data => {
      this.buffer = Buffer.concat([this.buffer, data]);
      while (true) {
        const end = this.buffer.indexOf('\r\n\r\n');
        if (end < 0) return;
        const size = Number(/Content-Length:\s*(\d+)/i.exec(this.buffer.subarray(0, end).toString())[1]);
        if (this.buffer.length < end + 4 + size) return;
        const message = JSON.parse(this.buffer.subarray(end + 4, end + 4 + size).toString());
        this.buffer = this.buffer.subarray(end + 4 + size);
        this.receive(message);
      }
    });
  }
  receive(message) {
    if (message.type === 'request' && message.command === 'runInTerminal') {
      const child = spawn(message.arguments.args[0], message.arguments.args.slice(1), { cwd: message.arguments.cwd, stdio: ['pipe','pipe','pipe'] });
      this.children.push(child); child.stderr.on('data', data => { this.errors += data; });
      child.stdout.on('data', () => {});
      this.write({ type: 'response', command: message.command, request_seq: message.seq, success: true, body: { processId: child.pid } });
    } else if (message.type === 'response' && this.pending.has(message.request_seq)) {
      const { resolve, timer } = this.pending.get(message.request_seq);
      this.pending.delete(message.request_seq); clearTimeout(timer); resolve(message);
    } else {
      const i = this.waits.findIndex(wait => wait.predicate(message));
      if (i >= 0) { const [wait] = this.waits.splice(i,1); clearTimeout(wait.timer); wait.resolve(message); }
      else this.messages.push(message);
    }
  }
  write(message) {
    message.seq = ++this.seq;
    const body = Buffer.from(JSON.stringify(message));
    this.process.stdin.write(Buffer.concat([Buffer.from(`Content-Length: ${body.length}\r\n\r\n`), body]));
    return message.seq;
  }
  request(command, args = {}) {
    return new Promise((resolve, reject) => {
      const seq = this.write({ type: 'request', command, arguments: args });
      const timer = setTimeout(() => { this.pending.delete(seq); reject(new Error(`Timed out on ${command}: ${this.errors}`)); }, 10000);
      this.pending.set(seq, { resolve, timer });
    });
  }
  wait(predicate) {
    const i = this.messages.findIndex(predicate);
    if (i >= 0) return Promise.resolve(this.messages.splice(i,1)[0]);
    return new Promise((resolve,reject) => {
      const timer = setTimeout(() => reject(new Error(`Timed out waiting for event: ${this.errors}`)),10000);
      this.waits.push({ predicate, resolve, timer });
    });
  }
  async launch(program, extras = {}) {
    const initialized = await this.request('initialize', { adapterID: 'fscript', supportsRunInTerminalRequest: true, linesStartAt1: true, columnsStartAt1: true });
    assert.equal(initialized.success,true);
    assert.equal(initialized.body.supportsSingleThreadExecutionRequests,true);
    const launch = await this.request('launch', { program, ...extras });
    assert.equal(launch.success,true,launch.message);
    await this.wait(message => message.event === 'initialized');
  }
  close() {
    for (const child of this.children) child.kill();
    this.process.stdin.end(); this.process.kill();
    for (const pending of this.pending.values()) clearTimeout(pending.timer);
    for (const wait of this.waits) clearTimeout(wait.timer);
  }
}

async function fixture(t, source) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'fscript debug 🫖 '));
  const program = path.join(dir, 'main.fss'); fs.writeFileSync(program,source);
  const client = new Client();
  t.after(() => { client.close(); fs.rmSync(dir,{recursive:true,force:true}); });
  return { client, program, dir };
}

test('real CLI DAP lifecycle, locals, safe watches, stepping, and terminal arguments', async t => {
  const { client, program } = await fixture(t,'let calculate x =\n    let answer = x + 1\n    answer * 2\ncalculate 41\n');
  await client.launch(program,{args:['hello world','🫖']});
  const points = await client.request('setBreakpoints',{source:{path:program},breakpoints:[{line:3,column:5}]});
  assert.equal(points.body.breakpoints[0].verified,true);
  await client.request('configurationDone');
  const stop = await client.wait(message => message.event === 'stopped');
  assert.equal(stop.body.allThreadsStopped,false);
  const stack = await client.request('stackTrace',{threadId:stop.body.threadId});
  assert.equal(stack.body.stackFrames[0].name,'calculate');
  const frameId = stack.body.stackFrames[0].id;
  const mainId = stack.body.stackFrames.at(-1).id;
  const argumentsValue = await client.request('evaluate',{frameId:mainId,expression:'Env.Arguments'});
  const argumentsItems = await client.request('variables',{variablesReference:argumentsValue.body.variablesReference});
  assert.deepEqual(argumentsItems.body.variables.map(value => value.value),['"hello world"','"🫖"']);
  const scopes = await client.request('scopes',{frameId});
  const locals = scopes.body.scopes.find(scope => scope.name === 'Locals');
  const variables = await client.request('variables',{variablesReference:locals.variablesReference});
  assert.ok(variables.body.variables.some(value => value.name === 'answer' && value.value === '42'));
  assert.equal((await client.request('evaluate',{frameId,expression:'answer'})).body.result,'42');
  assert.equal((await client.request('evaluate',{frameId,expression:'Console.writeLine "unsafe"'})).success,false);
  await client.request('setBreakpoints',{source:{path:program},breakpoints:[]});
  assert.equal((await client.request('stepOut',{threadId:stop.body.threadId,singleThread:true})).success,true);
  const returned = await client.wait(message => message.event === 'stopped');
  assert.equal(returned.body.reason,'step');
  assert.equal((await client.request('scopes',{frameId})).success,false);
  await client.request('continue',{threadId:stop.body.threadId,singleThread:true});
  await client.wait(message => message.event === 'terminated');
});

test('imported breakpoints and selected manifest failures use the real host', async t => {
  const { client,program,dir } = await fixture(t,'import "worker.fss" as Worker\nWorker.run 20\n');
  const imported = path.join(dir,'worker.fss'); fs.writeFileSync(imported,'export let run x = x * 2\n');
  await client.launch(program);
  assert.equal((await client.request('setBreakpoints',{source:{path:imported},breakpoints:[{line:1,column:20}]})).body.breakpoints[0].verified,true);
  await client.request('configurationDone');
  const stop = await client.wait(message => message.event === 'stopped');
  const stack = await client.request('stackTrace',{threadId:stop.body.threadId});
  assert.equal(stack.body.stackFrames[0].source.path,imported);
  assert.ok(!stack.body.stackFrames[0].name.includes('__imp'));
  const mainFrame = stack.body.stackFrames.at(-1);
  assert.equal((await client.request('evaluate',{frameId:mainFrame.id,expression:'Worker.run'})).body.result,'<function>');
  await client.request('disconnect');
});

test('configuration errors fail launch before any script execution', async t => {
  const {client,program,dir} = await fixture(t,'raise "must not run"');
  const config = path.join(dir,'fscript.toml'); fs.writeFileSync(config,'[unknown]\nvalue = true\n');
  await client.request('initialize',{supportsRunInTerminalRequest:true});
  const result = await client.request('launch',{program,configurationPath:config});
  assert.equal(result.success,false);
  assert.match(result.message,/Unknown configuration/);
});

test('native teapot callbacks can pause and resume on the CLI main thread', async t => {
  const client = new Client(); t.after(() => client.close());
  const program = path.join(root,'samples/ffi/main.fss');
  const scene = path.join(root,'samples/ffi/scene.fss');
  await client.launch(program,{configurationPath:path.join(root,'samples/ffi/fscript.toml'),rootDirectory:root,args:['--headless']});
  const points = await client.request('setBreakpoints',{source:{path:scene},breakpoints:[{line:23}]});
  assert.equal(points.body.breakpoints[0].verified,true);
  await client.request('configurationDone');
  const stop = await client.wait(message => message.event === 'stopped');
  const stack = await client.request('stackTrace',{threadId:stop.body.threadId});
  assert.equal(stack.body.stackFrames[0].name,'render');
  const scopes = await client.request('scopes',{frameId:stack.body.stackFrames[0].id});
  const parameters = scopes.body.scopes.find(scope => scope.name === 'Parameters');
  const values = await client.request('variables',{variablesReference:parameters.variablesReference});
  assert.ok(values.body.variables.some(value => value.name === 'window'));
  assert.ok(values.body.variables.some(value => value.name === 'mesh'));
  assert.ok(values.body.variables.some(value => value.name === 'seconds'));
  await client.request('setBreakpoints',{source:{path:scene},breakpoints:[]});
  await client.request('continue',{threadId:stop.body.threadId,singleThread:true});
  await client.wait(message => message.event === 'terminated');
});

test('DAP zero-based source positions are normalized without changing runtime locations', async t => {
  const {client,program} = await fixture(t,'let value = 41\nvalue + 1');
  await client.request('initialize',{supportsRunInTerminalRequest:true,linesStartAt1:false,columnsStartAt1:false});
  assert.equal((await client.request('launch',{program})).success,true);
  await client.wait(message => message.event === 'initialized');
  const points = await client.request('setBreakpoints',{source:{path:program},breakpoints:[{line:1,column:0}]});
  assert.equal(points.body.breakpoints[0].line,1);
  assert.equal(points.body.breakpoints[0].column,0);
  await client.request('configurationDone');
  const stopped = await client.wait(message => message.event === 'stopped');
  const stack = await client.request('stackTrace',{threadId:stopped.body.threadId});
  assert.equal(stack.body.stackFrames[0].line,1);
  await client.request('continue',{threadId:stopped.body.threadId,singleThread:true});
  await client.wait(message => message.event === 'terminated');
});

test('client without terminal support receives a launch error rather than hanging', async t => {
  const {client,program} = await fixture(t,'42');
  await client.request('initialize');
  const launch = await client.request('launch',{program});
  assert.equal(launch.success,false);
  assert.match(launch.message,/runInTerminal/);
});

test('unsupported conditional breakpoints are rejected rather than silently becoming unconditional', async t => {
  const {client,program} = await fixture(t,'42');
  await client.launch(program);
  const result = await client.request('setBreakpoints',{source:{path:program},breakpoints:[{line:1,condition:'false'}]});
  assert.equal(result.success,false);
  assert.match(result.message,/Conditional breakpoints/);
  await client.request('disconnect');
});
