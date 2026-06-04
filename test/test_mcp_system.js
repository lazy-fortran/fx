#!/usr/bin/env node
// System test for fx MCP server.

const { spawn } = require('child_process');
const fs = require('fs');
const path = require('path');

const FX_MCP = process.argv[2] || findLocalServer();
let passed = 0;
let failed = 0;

function assert(cond, msg) {
  if (cond) {
    passed += 1;
    process.stdout.write('  ok: ' + msg + '\n');
  } else {
    failed += 1;
    process.stdout.write('  FAIL: ' + msg + '\n');
  }
}

function findLocalServer() {
  const root = path.join(__dirname, '..');
  const candidates = [
    path.join(process.env.HOME || '', '.local', 'bin', 'fx-mcp-server'),
    path.join(root, 'app', 'fx-mcp-server'),
  ];

  const rootBuild = path.join(root, 'build');
  if (fs.existsSync(rootBuild)) {
    candidates.push(...findBuildBinaries(rootBuild));
    for (const entry of fs.readdirSync(rootBuild)) {
      const p = path.join(rootBuild, entry);
      if (fs.statSync(p).isDirectory()) {
        candidates.push(...findBuildBinaries(p));
      }
    }
  }

  for (const c of candidates) {
    if (c && fs.existsSync(c)) return c;
  }
  return '';
}

function findBuildBinaries(baseDir) {
  if (!fs.existsSync(baseDir) || !fs.statSync(baseDir).isDirectory()) return [];

  const found = [];
  for (const entry of fs.readdirSync(baseDir)) {
    const p = path.join(baseDir, entry, 'app', 'fx-mcp-server');
    if (fs.existsSync(p)) found.push(p);
  }
  return found;
}

if (!FX_MCP) {
  process.stdout.write('No fx-mcp-server binary found. Set test arg: node test/test_mcp_system.js /path/to/fx-mcp-server\n');
  process.exit(1);
}

function startServer() {
  const proc = spawn(FX_MCP, [], {
    stdio: ['pipe', 'pipe', 'pipe'],
    cwd: path.dirname(FX_MCP),
  });
  let stderr = '';
  proc.stderr.on('data', (d) => { stderr += d.toString(); });
  return { proc, getStderr: () => stderr };
}

function sendFramed(proc, obj) {
  const data = JSON.stringify(obj);
  proc.stdin.write('Content-Length: ' + Buffer.byteLength(data) + '\r\n\r\n' + data);
}

function sendBare(proc, obj) {
  const data = JSON.stringify(obj);
  proc.stdin.write(data + '\n');
}

function sendRaw(proc, data) {
  proc.stdin.write(data);
}

function readFramedResponse(proc, timeout) {
  timeout = timeout || 10000;
  return new Promise((resolve, reject) => {
    let buf = Buffer.alloc(0);
    let contentLength = null;
    let headerEnd = -1;
    const timer = setTimeout(() => {
      proc.stdout.removeListener('data', onData);
      reject(new Error('timeout waiting for framed response'));
    }, timeout);

    function onData(chunk) {
      buf = Buffer.concat([buf, chunk]);
      if (contentLength === null) {
        headerEnd = buf.indexOf('\r\n\r\n');
        if (headerEnd === -1) return;
        const header = buf.slice(0, headerEnd).toString();
        const m = header.match(/Content-Length:\s*(\d+)/i);
        if (!m) {
          clearTimeout(timer);
          proc.stdout.removeListener('data', onData);
          reject(new Error('bad header: ' + header));
          return;
        }
        contentLength = parseInt(m[1]);
      }
      const bodyStart = headerEnd + 4;
      if (buf.length >= bodyStart + contentLength) {
        clearTimeout(timer);
        proc.stdout.removeListener('data', onData);
        resolve(JSON.parse(buf.slice(bodyStart, bodyStart + contentLength).toString()));
      }
    }

    proc.stdout.on('data', onData);
  });
}

function readBareResponse(proc, timeout) {
  timeout = timeout || 10000;
  return new Promise((resolve, reject) => {
    let buf = '';
    const timer = setTimeout(() => {
      proc.stdout.removeListener('data', onData);
      reject(new Error('timeout waiting for bare response'));
    }, timeout);

    function onData(chunk) {
      buf += chunk.toString();
      const nl = buf.indexOf('\n');
      if (nl >= 0) {
        clearTimeout(timer);
        proc.stdout.removeListener('data', onData);
        resolve(JSON.parse(buf.slice(0, nl)));
      }
    }

    proc.stdout.on('data', onData);
  });
}

function makeToolsCallPayload(targetBytes, id, action = 'status') {
  const message = {
    jsonrpc: '2.0',
    id,
    method: 'tools/call',
    params: {
      name: 'fx',
      arguments: {
        action,
        payload: '',
      },
    },
  };
  const baseLength = JSON.stringify(message).length;
  const payloadLength = Math.max(0, targetBytes - baseLength);
  message.params.arguments.payload = 'x'.repeat(payloadLength);
  return message;
}

async function runSuite(label, send, readResponse) {
  process.stdout.write('\n--- ' + label + ' ---\n');
  const srv = startServer();

  try {
    process.stdout.write('initialize:\n');
    send(srv.proc, {
      jsonrpc: '2.0',
      id: 1,
      method: 'initialize',
      params: { protocolVersion: '2025-03-26', capabilities: {},
                clientInfo: { name: 'test', version: '1.0' } },
    });
    const init = await readResponse(srv.proc);
    assert(init.jsonrpc === '2.0', 'initialize has jsonrpc 2.0');
    assert(init.id === 1, 'initialize id matches');
    assert(init.result.protocolVersion === '2025-03-26', 'initialize echoes protocol version');

    process.stdout.write('ping:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 2, method: 'ping' });
    const ping = await readResponse(srv.proc);
    assert(ping.result !== undefined, 'ping returns a result block');

    process.stdout.write('initialized notification + tools/list:\n');
    send(srv.proc, { jsonrpc: '2.0', method: 'notifications/initialized' });
    send(srv.proc, { jsonrpc: '2.0', id: 3, method: 'tools/list' });
    const tools = await readResponse(srv.proc);
    assert(Array.isArray(tools.result.tools), 'tools/list returns tools array');
    assert(tools.result.tools.length > 0, 'tools/list has at least one tool');

    process.stdout.write('tools/call with missing id:\n');
    send(srv.proc, { jsonrpc: '2.0', method: 'tools/call', params: {
      name: 'fx',
      arguments: { action: 'check' },
    }});
    send(srv.proc, { jsonrpc: '2.0', id: 4, method: 'tools/call',
      params: { name: 'fx', arguments: { action: 'status' } } });
    const callResp = await readResponse(srv.proc);
    assert(callResp.id === 4, 'tools/call with id returns response');

    process.stdout.write('tools/call boundary payload (32KB):\n');
    const boundaryPayload = makeToolsCallPayload(32768, 7);
    assert(
      JSON.stringify(boundaryPayload).length <= 32768,
      'boundary tools/call payload is within 32KB',
    );
    send(srv.proc, boundaryPayload);
    const boundaryResp = await readResponse(srv.proc);
    assert(boundaryResp.id === 7, 'boundary payload returns a tools/call response');

    process.stdout.write('tools/call oversize payload (>32KB):\n');
    const oversizedPayload = makeToolsCallPayload(33000, 8);
    assert(
      JSON.stringify(oversizedPayload).length > 32768,
      'oversized tools/call payload exceeds 32KB',
    );
    send(srv.proc, oversizedPayload);
    const oversizeResp = await readResponse(srv.proc);
    assert(oversizeResp.error && oversizeResp.error.code === -32700,
      'oversize payload yields parse error');

    process.stdout.write('post-oversize tools/call recovery check:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 9, method: 'tools/list' });
    const recovery = await readResponse(srv.proc);
    assert(Array.isArray(recovery.result.tools), 'server recovers from oversize payload');

    process.stdout.write('unknown method:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 10, method: 'bogus/method' });
    const unknown = await readResponse(srv.proc);
    assert(unknown.error && unknown.error.code === -32601, 'unknown method uses method not found');

    process.stdout.write('malformed json request:\n');
    if (readResponse === readFramedResponse) {
      srv.proc.stdin.write('Content-Length: 2\r\n\r\n}{');
    } else {
      srv.proc.stdin.write('broken-json\n');
    }
    const bad = await readResponse(srv.proc);
    assert(bad.error && bad.error.code === -32700, 'malformed json returns parse error');

    if (readResponse === readFramedResponse) {
      process.stdout.write('malformed content-length header:\n');
      sendRaw(srv.proc, 'Content-Length: abc\r\n\r\n');
      const badHeader = await readResponse(srv.proc);
      assert(badHeader.error && badHeader.error.code === -32700,
             'malformed content-length header yields parse error');

      process.stdout.write('post-malformed-header recovery check:\n');
      send(srv.proc, { jsonrpc: '2.0', id: 12, method: 'ping' });
      const recoverPing = await readResponse(srv.proc);
      assert(recoverPing.result !== undefined, 'server recovers after malformed header');
    }

    process.stdout.write('shutdown:\n');
    send(srv.proc, { jsonrpc: '2.0', id: 11, method: 'shutdown' });
    const shut = await readResponse(srv.proc);
    assert(shut.result === null || (shut.result && shut.result.closed === true) || typeof shut.result === 'object',
      'shutdown responds');

    await new Promise((resolve) => {
      srv.proc.on('exit', (code) => {
        assert(code === 0, 'server exits cleanly after shutdown');
        resolve();
      });
      setTimeout(() => {
        assert(false, 'server exited within timeout');
        srv.proc.kill();
        resolve();
      }, 3000);
    });
  } catch (err) {
    failed++;
    process.stdout.write('  FAIL: exception: ' + err.message + '\n');
    srv.proc.kill();
  }
}

async function run() {
  process.stdout.write('fx MCP system test\n');
  process.stdout.write('binary: ' + FX_MCP + '\n');

  await runSuite('Content-Length framing', sendFramed, readFramedResponse);
  await runSuite('bare JSON framing', sendBare, readBareResponse);

  process.stdout.write('\n' + passed + ' passed, ' + failed + ' failed\n');
  process.exit(failed > 0 ? 1 : 0);
}

run();
