const { test } = require('node:test');
const assert = require('node:assert/strict');
const { waitForMessage } = require('./EventWait');

test('expensive startup may use a longer event deadline without losing a delayed breakpoint', async () => {
  const client = { messages: [], waits: [], errors: '' };
  const stopped = { event: 'stopped' };
  let timer;
  try {
    const waiting = waitForMessage(client, message => message.event === 'stopped', { timeoutMs: 1000, label: 'first render breakpoint' });
    timer = setTimeout(() => {
      const wait = client.waits.shift();
      clearTimeout(wait.timer);
      wait.resolve(stopped);
    }, 30);
    assert.equal(await waiting, stopped);
    assert.equal(client.waits.length, 0);
  } finally {
    clearTimeout(timer);
    for (const wait of client.waits) clearTimeout(wait.timer);
  }
});

test('an expired event wait is removed and identifies the missing event', async () => {
  const client = { messages: [{ event: 'output' }], waits: [], errors: 'child diagnostic' };
  await assert.rejects(
    waitForMessage(client, message => message.event === 'stopped', { timeoutMs: 5, label: 'first render breakpoint' }),
    /waiting for first render breakpoint\. Received: output\. Stderr: child diagnostic/
  );
  assert.equal(client.waits.length, 0);
});

test('an already buffered event is consumed without creating a timer', async () => {
  const stopped = { event: 'stopped' };
  const client = { messages: [stopped], waits: [], errors: '' };
  assert.equal(await waitForMessage(client, message => message.event === 'stopped'), stopped);
  assert.equal(client.messages.length, 0);
  assert.equal(client.waits.length, 0);
});
