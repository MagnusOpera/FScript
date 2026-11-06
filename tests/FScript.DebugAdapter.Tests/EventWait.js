function waitForMessage(client, predicate, { timeoutMs = 10000, label = 'matching event' } = {}) {
  const index = client.messages.findIndex(predicate);
  if (index >= 0) return Promise.resolve(client.messages.splice(index, 1)[0]);
  return new Promise((resolve, reject) => {
    const wait = { predicate, resolve };
    wait.timer = setTimeout(() => {
      const index = client.waits.indexOf(wait);
      if (index >= 0) client.waits.splice(index, 1);
      const received = client.messages.map(message => message.event || message.command || message.type).join(', ');
      reject(new Error(`Timed out after ${timeoutMs}ms waiting for ${label}. Received: ${received || 'none'}. Stderr: ${client.errors}`));
    }, timeoutMs);
    client.waits.push(wait);
  });
}

module.exports = { waitForMessage };
