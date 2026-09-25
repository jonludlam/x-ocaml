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

const PORT = 8766;
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
  return new Promise((resolve) => server.listen(PORT, () => resolve(server)));
}

(async () => {
  const server = await serve();
  const browser = await chromium.launch({
    headless: !process.argv.includes('--headed'),
    executablePath: process.env.XOCAML_CHROMIUM || undefined,
  });
  let failed = 0;
  try {
    const page = await browser.newPage();
    page.on('pageerror', (e) => console.error(`[page error] ${e.message}`));
    for (const p of PAGES) {
      await page.goto(`http://localhost:${PORT}${p}`);
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
      await page.goto(`http://localhost:${PORT}${p}`);
      // Settled: no cell running, or waiting to.
      const settled = await page.waitForFunction(() => {
        const cells = [...document.querySelectorAll('*')].filter((e) => e.shadowRoot);
        return cells.length > 0 && cells.every((c) => !c.shadowRoot.querySelector('.run_btn button.running'));
      }, null, { timeout: TIMEOUT, polling: 500 }).then(() => true, () => false);
      await page.waitForTimeout(1000);
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
