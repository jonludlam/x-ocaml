#!/usr/bin/env node
// Serves the test pages and examples next to the built x-ocaml scripts, to
// go through them by hand, with an index at /. Test pages get a panel
// showing their checks as they finish.
//   dune build && node test/serve.js [port]

const http = require('http');
const fs = require('fs');
const path = require('path');
const { TESTS, EXAMPLES } = require('./pages.js');

const PORT = Number(process.argv[2]) || 8800;
const testDir = __dirname;
const rootDir = path.resolve(testDir, '..');
const types = { '.html': 'text/html; charset=utf-8', '.js': 'application/javascript', '.css': 'text/css' };
const escape = (s) => s.replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]);

function index() {
  const rows = (pages) => pages.map(([p, what]) =>
    `<li><a href="${p}">${escape(p)}</a> &mdash; ${escape(what)}</li>`).join('\n');
  return `<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>x-ocaml pages</title>
<style>
  body { font-family: system-ui, sans-serif; max-width: 50rem; margin: 2rem auto; padding: 0 1rem; line-height: 1.6; }
  li { margin: 0.2rem 0; }
  .note { color: #555; }
  button { font: inherit; }
</style></head><body>
<h1>x-ocaml pages</h1>
<p class="note">Served from ${escape(rootDir)}. Rebuild with <code>dune build</code>
  and reload a page to see a change; the server reads the files each time.</p>
<h2>Examples</h2>
<p class="note">Pages to read and play with.</p>
<ul>${rows(EXAMPLES)}</ul>
<h2>Tests</h2>
<p class="note">Each runs its checks by itself when it opens (some click and
  type for you), and shows how they went in a panel at the bottom right.
  Several use this origin's local storage;
  <button onclick="localStorage.clear(); this.textContent = 'cleared'">clear it</button>
  to start afresh.</p>
<ul>${rows(TESTS)}</ul>
</body></html>`;
}

// Shown on the test pages: their checks as they come.
const panel = `
<script>
(function () {
  const box = document.createElement('div');
  box.style.cssText = 'position:fixed;right:1rem;bottom:1rem;max-width:28rem;max-height:60vh;overflow:auto;' +
    'background:#fff;border:1px solid #999;border-radius:6px;padding:0.6rem 0.8rem;font:13px system-ui,sans-serif;' +
    'box-shadow:0 2px 8px rgba(0,0,0,0.2);z-index:100000';
  document.body.appendChild(box);
  function draw() {
    const r = window.testResults;
    const rows = r ? r.details.map((d) =>
      '<div style="color:' + (d.ok ? '#2e7d32' : '#c62828') + '">' + (d.ok ? '\\u2713 ' : '\\u2717 ') +
      d.name.replace(/</g, '&lt;') + '</div>').join('') : '';
    const head = !r ? 'waiting for the page' : r.done
      ? '<b style="color:' + (r.failed ? '#c62828' : '#2e7d32') + '">' + r.passed + '/' + r.total + ' passed</b>'
      : '<b>running\\u2026</b> ' + r.passed + '/' + r.total + ' so far';
    box.innerHTML = '<a href="/" style="float:right">index</a>' + head + rows;
    if (!(r && r.done)) setTimeout(draw, 300);
  }
  draw();
})();
</script>`;

http.createServer((req, res) => {
  const url = decodeURIComponent(req.url.split('?')[0]);
  if (url === '/') {
    res.writeHead(200, { 'Content-Type': types['.html'], 'Cache-Control': 'no-store' });
    res.end(index());
    return;
  }
  const file = [path.join(testDir, url), path.join(rootDir, url)]
    .find((f) => f.startsWith(rootDir) && fs.existsSync(f) && fs.statSync(f).isFile());
  if (!file) { res.writeHead(404); res.end('not found: ' + url); return; }
  const type = types[path.extname(file)] || 'application/octet-stream';
  res.writeHead(200, { 'Content-Type': type, 'Cache-Control': 'no-store' });
  if (TESTS.some(([p]) => p === url)) {
    res.end(fs.readFileSync(file, 'utf8').replace('</body>', panel + '\n</body>'));
  } else {
    fs.createReadStream(file).pipe(res);
  }
}).listen(PORT, '127.0.0.1', () => console.log(`x-ocaml pages at http://localhost:${PORT}/`));
