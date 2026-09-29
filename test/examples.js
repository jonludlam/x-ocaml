// What a test page of an example needs, after lib.js: example(src, quiet,
// more) loads the example at src in a frame, with its folded cells open,
// waits for every cell to run, and checks that each did without an error.
// A cell in quiet shows nothing when it runs; any other shows something,
// its answer or a display. more(frame) is the page's own checks, if any.

const tall = 40000; // A frame tall enough for every line of every cell to be drawn.

function example(src, quiet, more) {
  const frame = document.createElement('iframe');
  frame.style.width = '1000px';
  frame.style.height = tall + 'px';
  frame.src = src;
  document.body.append(frame);
  frame.addEventListener('load', async () => {
    const doc = frame.contentDocument;
    frame.contentWindow.addEventListener('error', (e) => window.pageErrors.push(String(e.message)));
    doc.querySelectorAll('details').forEach((d) => (d.open = true));
    const cells = [...doc.querySelectorAll('x-ocaml')];
    const outputs = (c) => (c.shadowRoot ? [...c.shadowRoot.querySelectorAll('.caml_stdout, .caml_stderr, .caml_meta')] : []);
    const displays = (c) => {
      const next = c.nextElementSibling;
      return next && next.classList.contains('x-ocaml-displays') ? next.children.length : 0;
    };
    const shown = (c) => outputs(c).length > 0 || displays(c) > 0;
    // The cells run in order, so the page is done once its last cell has
    // shown something (a quiet last cell would need another way), and
    // nothing more has been shown for a while: the last cell's later
    // phrases may show more.
    const last = cells[cells.length - 1];
    const count = () => cells.reduce((n, c) => n + outputs(c).length + displays(c), 0);
    let seen = -1, since = Date.now();
    const settled = () => {
      const n = count();
      if (n !== seen) (seen = n), (since = Date.now());
      return shown(last) && Date.now() - since > 2000;
    };
    const ran = await waitFor(settled, 50000);
    check(`every cell of ${src} runs`, ran && cells.every((c) => quiet.includes(c.id) || shown(c)));
    const failed = cells.filter((c) => outputs(c).some((o) => /Error|Exception/.test(o.textContent)));
    check('and none fails' + (failed.length ? ': ' + failed.map((c) => c.id).join(', ') : ''), failed.length === 0);
    if (more) await more(frame);
    done();
  });
}
