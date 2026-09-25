#!/usr/bin/env node
// Serves the test pages and examples next to the built x-ocaml scripts, to
// go through them by hand, with an index at /.
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

// The pages name Recursive and fall back to system fonts, so the automated
// runs need no network; here, for reading them, the font is fetched.
const fonts = `
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Recursive:slnt,wght,CASL,CRSV,MONO@-15..0,300..1000,0..1,0..1,0..1&display=swap">`;

function index() {
  const rows = (pages) => pages.map(([p, what]) =>
    `<li><a href="${p}"><span class="what">${escape(what)}</span><span class="path">${escape(p)}</span></a></li>`).join('\n');
  return `<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>x-ocaml tests</title>
${fonts}
<link rel="stylesheet" href="/test.css">
<style>
  .pages { list-style: none; margin: 0; padding: 0; }
  .pages li + li { border-top: 1px dashed var(--rule); }
  .pages a { display: flex; justify-content: space-between; gap: 1rem; padding: 0.5rem 0; text-decoration: none; color: var(--ink); }
  .pages a:hover .what { color: var(--id); }
  .path { font-family: var(--mono); font-variation-settings: "MONO" 1; font-size: 12px; color: var(--muted); white-space: nowrap; }
  .case h2 { margin-bottom: 0.3rem; }
  .case::before { display: none; } /* no checks here, so no status */
  .actions { margin-top: 0.4rem; font-size: 13px; color: var(--muted); }
  .actions button { font: inherit; font-size: 12px; padding: 0.15rem 0.6rem; border: 1px solid var(--rule); border-radius: 4px; background: var(--card); color: var(--ink); cursor: pointer; }
  @media (max-width: 40rem) { .pages a { flex-direction: column; gap: 0; } }
</style></head><body>
<header class="bench"><div class="bench-inner">
  <span class="crumb">${escape(rootDir)}</span>
  <h1>x-ocaml tests</h1>
  <div class="tally">Rebuild with dune build and reload a page: files are read afresh each time.</div>
</div></header>
<main>
  <section class="case">
    <h2>Test pages</h2>
    <p>Each runs its checks by itself when it opens, clicking and typing
      where it needs to. Several use this site's local storage.</p>
    <ul class="pages">${rows(TESTS)}</ul>
    <div class="actions"><button onclick="localStorage.clear(); this.textContent = 'Storage cleared'">Clear local storage</button></div>
  </section>
  <section class="case">
    <h2>Examples</h2>
    <p>Pages to read and play with.</p>
    <ul class="pages">${rows(EXAMPLES)}</ul>
  </section>
</main>
</body></html>`;
}

http.createServer((req, res) => {
  const url = decodeURIComponent(req.url.split('?')[0]);
  if (url === '/') {
    res.writeHead(200, { 'Content-Type': types['.html'], 'Cache-Control': 'no-store' });
    res.end(index());
    return;
  }
  const file = [path.join(testDir, url), path.join(rootDir, url), path.join(rootDir, '_build/default/test', url)]
    .find((f) => f.startsWith(rootDir) && fs.existsSync(f) && fs.statSync(f).isFile());
  // (The universe for the js_top_worker pages is built under _build: dune build @runbrowser, or ./test/universe/universe.)
  if (!file) { res.writeHead(404); res.end('not found: ' + url); return; }
  const type = types[path.extname(file)] || 'application/octet-stream';
  res.writeHead(200, { 'Content-Type': type, 'Cache-Control': 'no-store' });
  if (TESTS.some(([p]) => p === url)) {
    res.end(fs.readFileSync(file, 'utf8').replace('</head>', fonts + '\n</head>'));
  } else {
    fs.createReadStream(file).pipe(res);
  }
}).listen(PORT, '127.0.0.1', () => console.log(`x-ocaml pages at http://localhost:${PORT}/`));
