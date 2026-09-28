#!/usr/bin/env node
// Serves the test pages next to the built x-ocaml scripts and runs each in
// Chromium; a page reports through window.testResults.
//   node run_tests.js [--headed]

// The full package when installed here; otherwise any playwright-core
// on NODE_PATH.
const { chromium } = (() => {
  try { return require('playwright'); } catch (e) { return require('playwright-core'); }
})();
const http = require('http');
const fs = require('fs');
const path = require('path');

const TIMEOUT = 120000;
const { TESTS, EXAMPLES: EXAMPLE_PAGES } = require('./pages.js');
const PAGES = TESTS.map(([p]) => p);

// Example pages are checked only for running without errors: every cell
// that runs on load has run, and nothing threw.
const EXAMPLES = EXAMPLE_PAGES.map(([p]) => p);

const testDir = __dirname;
const rootDir = path.resolve(testDir, '..');
const types = { '.html': 'text/html', '.js': 'application/javascript', '.css': 'text/css' };

function serve() {
  const server = http.createServer((req, res) => {
    const url = req.url.split('?')[0];
    const file = [path.join(testDir, url), path.join(rootDir, url)].find(fs.existsSync);
    if (!file) { res.writeHead(404); res.end(); return; }
    res.writeHead(200, { 'Content-Type': types[path.extname(file)] || 'application/octet-stream' });
    fs.createReadStream(file).pipe(res);
  });
  // Any free port, so that runs side by side do not collide.
  return new Promise((resolve) => server.listen(0, '127.0.0.1', () => resolve(server)));
}

// Chromium: $XOCAML_CHROMIUM if set, else Playwright's own (npm install
// fetches it), else an installed Google Chrome.
async function launch() {
  const headless = !process.argv.includes('--headed');
  const tries = process.env.XOCAML_CHROMIUM
    ? [{ executablePath: process.env.XOCAML_CHROMIUM }]
    : [{}, { channel: 'chrome' }];
  for (const t of tries) {
    try { return await chromium.launch({ headless, ...t }); } catch (e) {}
  }
  console.error('No Chromium to run the tests in: in test/, run\n' +
    '  npx playwright install chromium\n' +
    'or set XOCAML_CHROMIUM to a Chromium or Chrome executable.');
  process.exit(2);
}

(async () => {
  const server = await serve();
  const port = server.address().port;
  const browser = await launch();
  let failed = 0;
  try {
    const page = await browser.newPage();
    page.on('pageerror', (e) => console.error(`[page error] ${e.message}`));
    for (const p of PAGES) {
      await page.goto(`http://127.0.0.1:${port}${p}`);
      await page.waitForFunction(() => window.testResults && window.testResults.done, null, { timeout: TIMEOUT });
      const r = await page.evaluate(() => window.testResults);
      console.log(`${p}: ${r.passed}/${r.total} passed`);
      for (const d of r.details) console.log(`  [${d.ok ? 'PASS' : 'FAIL'}] ${d.name}`);
      failed += r.failed;
    }
    for (const p of EXAMPLES) {
      const errors = [];
      const onError = (e) => errors.push(e.message);
      const onConsole = (m) => { if (m.type() === 'error') errors.push(m.text()); };
      page.on('pageerror', onError);
      page.on('console', onConsole);
      await page.goto(`http://127.0.0.1:${port}${p}`);
      // Settled: no cell running, or waiting to, and the cells that run on
      // load have answered (a js_top_worker page loads its universe first).
      const settled = await page.waitForFunction(() => {
        const cells = [...document.querySelectorAll('*')].filter((e) => e.shadowRoot);
        const answered = (c) => c.shadowRoot.querySelector('.caml_stdout, .caml_stderr, .caml_meta');
        return cells.length > 0 && cells.every((c) => !c.shadowRoot.querySelector('.run_btn button.running'))
          && (cells.some(answered) || cells.every((c) => c.getAttribute('run-on') === 'click'));
      }, null, { timeout: TIMEOUT, polling: 500 }).then(() => true, () => false);
      await page.waitForTimeout(1000);
      // An example whose code does not compile or raises is broken too,
      // unless the cell says it shows an error on purpose, or it is an
      // exercise (and its tests), which fail until the reader writes it.
      const failing = await page.evaluate(() =>
        [...document.querySelectorAll('*')]
          .filter((e) => e.shadowRoot && e.getAttribute('data-expect') !== 'error'
            && !['exercise', 'test'].includes(e.getAttribute('mode')))
          .map((c) => [...c.shadowRoot.querySelectorAll('.caml_stderr, .caml_meta')].map((o) => o.textContent).join(' '))
          .filter((t) => /(^|\s)(Error|Exception):|Stack overflow/.test(t)));
      for (const f of failing) errors.push('a cell failed: ' + f.slice(0, 200));
      page.off('pageerror', onError);
      page.off('console', onConsole);
      const ok = settled && errors.length === 0;
      console.log(`${p}: ${ok ? 'runs without errors' : 'FAIL ' + (settled ? errors.join(' | ') : 'cells still running')}`);
      if (!ok) failed++;
    }
  } catch (e) {
    console.error(e.message);
    failed++;
  } finally {
    await browser.close();
    server.close();
  }
  process.exit(failed ? 1 : 0);
})();
