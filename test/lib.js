// Helpers shared by the test pages. A page calls check(name, ok) and sets
// testResults.done when it has finished.
window.testResults = { total: 0, passed: 0, failed: 0, done: false, details: [] };

function check(name, ok) {
  const r = window.testResults;
  r.total++;
  ok ? r.passed++ : r.failed++;
  r.details.push({ name, ok: !!ok });
}

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

// For pages that end with done(): the page's own errors are checked too.
function done() {
  noPageErrors();
  window.testResults.done = true;
}
