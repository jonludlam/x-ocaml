// Helpers shared by the test pages. A page calls check(name, ok) as each
// check finishes and sets testResults.done when it has finished.
//
// The page is laid out as cases, <section class="case" data-case="L1">
// with an <h2> saying what the case tries. A check named "L1: ..." is
// listed under case L1; any other check under the page's own checks at the
// end. A strip at the top shows every check, in the order it finished.

window.testResults = { total: 0, passed: 0, failed: 0, done: false, details: [] };

// Each run starts afresh: forget the exercises this page kept last time.
// (A page that wants a kept version sets it after loading this file.)
try {
  const prefix = 'xo:' + location.pathname + ':';
  for (const k of Object.keys(localStorage)) if (k.startsWith(prefix)) localStorage.removeItem(k);
} catch (e) {}

const bench = {};

function checksOf(section) {
  let ul = section.querySelector(':scope > .checks');
  if (!ul) {
    ul = document.createElement('ul');
    ul.className = 'checks';
    section.appendChild(ul);
  }
  return ul;
}

function pageChecks() {
  let s = document.querySelector('.case.page-checks');
  if (!s) {
    s = document.createElement('section');
    s.className = 'case page-checks';
    s.id = 'page-checks';
    s.innerHTML = '<h2>The page as a whole</h2>';
    (document.querySelector('main') || document.body).appendChild(s);
  }
  return s;
}

function drawTally() {
  if (!bench.tally) return;
  const r = window.testResults;
  const n = (k) => `${k} check${k === 1 ? '' : 's'}`;
  bench.tally.innerHTML = !r.done
    ? `${n(r.total)} so far${r.failed ? `, <b class="bad">${r.failed} failed</b>` : ''}`
    : r.failed
      ? `<b class="bad">${r.failed} of ${r.total} failed</b>`
      : `${n(r.total)}, <b class="good">all passed</b>`;
  bench.running.hidden = r.done;
}

function drawCheck(name, ok) {
  if (!bench.strip) return;
  const m = /^([A-Z]+[0-9]+): (.*)$/.exec(name);
  const sections = m ? [...document.querySelectorAll(`.case[data-case="${m[1]}"]`)] : [];
  const home = sections[0] || pageChecks();
  // A segment is a link to the case its check is about.
  const seg = document.createElement('li');
  seg.className = ok ? 'pass' : 'fail';
  seg.innerHTML = `<a href="#${home.id}"></a>`;
  seg.firstChild.title = name;
  seg.firstChild.setAttribute('aria-label', (ok ? 'passed: ' : 'failed: ') + name);
  bench.strip.insertBefore(seg, bench.running);
  const li = document.createElement('li');
  li.className = ok ? 'pass' : 'fail';
  li.textContent = sections.length ? m[2] : name;
  li.setAttribute('aria-label', (ok ? 'passed: ' : 'failed: ') + li.textContent);
  checksOf(home).appendChild(li);
  for (const s of sections.length ? sections : [home]) {
    if (!ok) s.dataset.state = 'fail';
    else if (s.dataset.state !== 'fail') s.dataset.state = 'pass';
  }
  drawTally();
}

function check(name, ok) {
  const r = window.testResults;
  r.total++;
  ok ? r.passed++ : r.failed++;
  r.details.push({ name, ok: !!ok });
  drawCheck(name, !!ok);
}

// testResults.done, when set, redraws the tally.
(function () {
  let done = false;
  Object.defineProperty(window.testResults, 'done', {
    enumerable: true,
    get: () => done,
    set: (v) => { done = v; drawTally(); },
  });
})();

document.addEventListener('DOMContentLoaded', () => {
  const header = document.createElement('header');
  header.className = 'bench';
  const title = document.title.replace(/^x-ocaml:?\s*/, '');
  header.innerHTML = `<div class="bench-inner">
    <a class="crumb" href="/">x-ocaml tests</a>
    <h1></h1>
    <ol class="strip" aria-label="Checks, in the order they finished"><li class="running" title="still running"></li></ol>
    <div class="tally" aria-live="polite"></div>
  </div>`;
  header.querySelector('h1').textContent = title.charAt(0).toUpperCase() + title.slice(1);
  document.body.prepend(header);
  bench.strip = header.querySelector('.strip');
  bench.running = header.querySelector('.running');
  bench.tally = header.querySelector('.tally');
  const seen = new Set();
  for (const s of document.querySelectorAll('.case[data-case]')) {
    if (!seen.has(s.dataset.case)) s.id = 'case-' + s.dataset.case;
    const h = s.querySelector('h2');
    if (h) h.insertAdjacentHTML('afterbegin', `<span class="case-id">${s.dataset.case}</span>`);
    // A case can go on further down the page; its checks are listed once.
    if (!seen.has(s.dataset.case)) checksOf(s);
    seen.add(s.dataset.case);
  }
  drawTally();
});

const cell = (id) => document.getElementById(id);
const shadow = (id) => cell(id).shadowRoot;
const editorOf = (id) => shadow(id) && shadow(id).querySelector('.cm-editor');
const readOnly = (id) =>
  editorOf(id).querySelector('.cm-content').getAttribute('aria-readonly') === 'true';
// The editor's text, without the output drawn among its lines.
const code = (id) =>
  [...shadow(id).querySelectorAll('.cm-line')].map((l) => l.textContent).join('\n');
const output = (id) =>
  [...shadow(id).querySelectorAll('.caml_stdout, .caml_stderr, .caml_meta')]
    .map((e) => e.textContent).join('\n');
const verdict = (id) => {
  const i = shadow(id).querySelector('.test-indicator');
  return i ? i.textContent : null;
};
const button = (id) => shadow(id).querySelector('.run_btn button');
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function waitFor(pred, ms) {
  return new Promise((resolve) => {
    const start = Date.now();
    (function poll() {
      let ok = false;
      try { ok = pred(); } catch (e) {}
      if (ok || Date.now() - start > ms) resolve(ok);
      else setTimeout(poll, 100);
    })();
  });
}

// Type into a cell's editor, at the cursor (the start, if never moved).
function type(id, text) {
  shadow(id).querySelector('.cm-content').focus();
  document.execCommand('insertText', false, text);
}

// Errors thrown on the page (not in the worker) are failures too.
window.pageErrors = [];
window.addEventListener('error', (e) => window.pageErrors.push(String(e.message)));
function noPageErrors() {
  check('no errors on the page', window.pageErrors.length === 0);
  if (window.pageErrors.length) console.log('page errors: ' + window.pageErrors.join(' | '));
}

// What x-ocaml warns about, in order.
window.warnings = [];
(function () {
  const warn = console.warn.bind(console);
  console.warn = (...args) => { window.warnings.push(args.join(' ')); warn(...args); };
})();
const warned = (sub) => window.warnings.some((w) => w.includes(sub));
