// <x-stepper>: a computation's states, one at a time, as box-and-pointer
// diagrams, where the reader can be asked to predict the next state before
// it is shown.
//
// Its data (JSON, from a cell through X_ocaml_lib.display, or set from
// JavaScript) is
//
//   { "id": "rev-steps",            -- optional: the element's id, for scripts
//     "title": "rev [1; 2; 3]",
//     "code": "let rec go acc xs = ...",  -- optional: shown beside the diagram
//     "mode": "predict" | "lecture",  -- lecture: nothing is asked
//     "ask": 2,                       -- predict: ask before the first 2 steps,
//                                     --   then only before the last
//     "token": 7,                     -- optional: x-ocaml callback for answers
//     "steps": [ state, ... ] }
//
// and a state is
//
//   { "label": "go [2; 3] [1]",       -- the expression, or what happened
//     "line": 4,                      -- optional: the line of code that runs next
//     "note": "...",                  -- optional
//     "roots": [ { "name": "xs", "to": "n2" },      -- an arrow to a node
//                { "name": "n", "text": "3" } ],    -- a plain value
//     "nodes": { "n2": { "k": "cons", "v": "2", "next": "n3" },  -- null is []
//                "t1": { "k": "tree", "v": "4", "kids": ["t2", null] } },
//     "highlight": ["n2"],
//     "choices": [ { "state": state, "why": "..." }, ... ] }
//
// A node keeps its id from state to state, so the diagram moves rather than
// being redrawn, and two arrows to one id show sharing. "choices" are wrong
// next states, each with what the misconception behind it gets wrong: when
// the reader is asked for this state, they choose among it and them.
//
// From a script (a slip's exec block, say): step() shows the next state,
// back() the one before, which undoes it, reset() the first; mode and index
// are properties. In lecture mode step() just steps; in predict mode, when
// the next state is to be predicted, step() asks first, and a second step()
// reveals it.
(function () {
  if (customElements.get('x-stepper')) return;

  const NS = 'http://www.w3.org/2000/svg';
  const BOX = 30, GAP = 26, ROW = 64, LEFT = 96, R = 17, LEVEL = 60, TOP = 18;

  const css = `
    :host { display: block; font: 14px/1.4 system-ui, sans-serif; color: var(--xs-ink, #1d1d1f);
      --xs-accent: #d9480f; --xs-line: #495057; --xs-box: #fff; --xs-soft: #f1f3f5; --xs-good: #2b8a3e; --xs-bad: #c92a2a; }
    .frame { border: 1px solid #dee2e6; border-radius: 8px; padding: 10px 12px; background: var(--xs-bg, #fcfcfd); }
    .head { display: flex; align-items: baseline; gap: 10px; flex-wrap: wrap; }
    .title { font-weight: 600; }
    .count { color: #868e96; font-variant-numeric: tabular-nums; margin-left: auto; }
    .body { display: grid; grid-template-columns: minmax(0, 1fr); gap: 4px 16px; align-items: start; }
    .body.with-code { grid-template-columns: max-content minmax(0, 1fr); }
    @media (max-width: 640px) { .body.with-code { grid-template-columns: minmax(0, 1fr); } }
    .code { margin: 6px 0 0; padding: 6px 0; font: 13px/1.55 ui-monospace, Menlo, monospace; background: var(--xs-soft); border-radius: 6px; overflow-x: auto; }
    .code[hidden] { display: none; }
    .code .ln { display: block; padding: 0 10px 0 6px; border-left: 3px solid transparent; white-space: pre; }
    .code .ln .no { color: #adb5bd; display: inline-block; width: 1.8em; user-select: none; }
    .code .ln.now { background: #fff3bf; border-left-color: var(--xs-accent); }
    .label { font-family: ui-monospace, Menlo, monospace; margin: 6px 0 2px; min-height: 1.4em; }
    .note { color: #495057; min-height: 1.4em; }
    svg { display: block; max-width: 100%; height: auto; overflow: visible; }
    svg .node { transition: transform 0.45s ease, opacity 0.3s; }
    svg .entering { opacity: 0; }
    svg rect, svg circle { fill: var(--xs-box); stroke: var(--xs-line); stroke-width: 1.5; }
    svg .hl rect, svg .hl circle { stroke: var(--xs-accent); stroke-width: 3; }
    svg text { font: 14px ui-monospace, Menlo, monospace; fill: currentColor; dominant-baseline: central; text-anchor: middle; }
    svg .var { font-weight: 600; text-anchor: end; }
    svg .arrow { stroke: var(--xs-line); stroke-width: 1.5; fill: none; marker-end: url(#xs-head); }
    svg .nil { stroke: var(--xs-line); stroke-width: 1.5; }
    svg circle.dot { fill: var(--xs-line); stroke: none; }
    .controls { display: flex; gap: 6px; margin-top: 8px; }
    button { font: inherit; border: 1px solid #adb5bd; background: #fff; border-radius: 6px; padding: 3px 10px; cursor: pointer; }
    button:disabled { opacity: 0.4; cursor: default; }
    .ask { margin-top: 10px; border-top: 1px dashed #ced4da; padding-top: 8px; }
    .ask[hidden] { display: none; }
    .prompt { font-weight: 600; margin-bottom: 6px; }
    .options { display: flex; flex-wrap: wrap; gap: 8px; }
    .option { display: flex; flex-direction: column; align-items: stretch; text-align: left; padding: 6px; max-width: 100%; }
    .option .olabel { font-family: ui-monospace, Menlo, monospace; font-size: 12px; margin-bottom: 2px; }
    .option:disabled { opacity: 1; cursor: default; }
    .option:disabled:not(.right):not(.wrong) { opacity: 0.5; }
    .option.right { border-color: var(--xs-good); box-shadow: 0 0 0 2px var(--xs-good); }
    .option.wrong { border-color: var(--xs-bad); box-shadow: 0 0 0 2px var(--xs-bad); }
    .feedback { margin-top: 8px; min-height: 1.4em; }
    .feedback.right { color: var(--xs-good); }
    .feedback.wrong { color: var(--xs-bad); }
  `;

  // A deterministic shuffle, so that a page shows the same order each time.
  function shuffle(xs, seed) {
    const a = xs.slice();
    let s = seed * 9301 + 49297;
    for (let i = a.length - 1; i > 0; i--) {
      s = (s * 9301 + 49297) % 233280;
      const j = Math.floor((s / 233280) * (i + 1));
      [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
  }

  // Where each node goes: a row for each root's list, a band for each tree;
  // a node already placed is not placed again, and an arrow goes to it.
  function layout(state) {
    const nodes = state.nodes || {};
    const pos = {};          // id -> { x, y, k }
    const arrows = [];       // { from: [x, y], to: id }
    const vars = [];         // { name, x, y, text }
    const nils = [];         // [x, y] of a pointer box holding []
    let y = TOP + BOX / 2;
    let width = LEFT;

    function placeList(id, x, y) {
      let prev = null;
      while (id != null && nodes[id] && nodes[id].k === 'cons' && !pos[id]) {
        pos[id] = { x, y, k: 'cons' };
        if (prev) arrows.push({ from: [pos[prev].x + 1.5 * BOX, y], to: id });
        prev = id;
        const next = nodes[id].next;
        x += 2 * BOX + GAP;
        width = Math.max(width, x);
        if (next == null) { nils.push([pos[id].x + 1.5 * BOX, y]); id = null; }
        else id = next;
      }
      if (prev && id != null) arrows.push({ from: [pos[prev].x + 1.5 * BOX, pos[prev].y], to: id });
      return prev;
    }

    function placeTree(id, top) {
      // Leaves' slots from left to right; a node above the middle of its kids.
      let slot = 0, depth = 0;
      function go(id, d) {
        if (id == null || !nodes[id] || pos[id]) return null;
        const n = nodes[id];
        depth = Math.max(depth, d);
        const kids = (n.kids || []);
        const xs = [];
        let mine = null;
        // An empty child beside a real one keeps its place, so that a lone
        // child is seen to be a left or a right one.
        const some = kids.some((k) => k != null);
        kids.forEach((k) => {
          if (k == null && some) { xs.push(LEFT + R + slot * (2 * R + 14)); slot++; return; }
          const x = go(k, d + 1);
          if (x != null) xs.push(x);
        });
        if (xs.length === 0) { mine = LEFT + R + slot * (2 * R + 14); slot++; }
        else mine = (xs[0] + xs[xs.length - 1]) / 2;
        pos[id] = { x: mine, y: top + d * LEVEL, k: 'tree' };
        return mine;
      }
      go(id, 0);
      // Edges, and dashes where a child is empty.
      function edges(id) {
        const n = nodes[id];
        if (!n || n.k !== 'tree') return;
        (n.kids || []).forEach((k) => {
          if (k != null && pos[k]) {
            arrows.push({ from: [pos[id].x, pos[id].y + R], to: k, tree: true });
            if (pos[k].y > pos[id].y) edges(k);
          }
        });
      }
      edges(id);
      width = Math.max(width, LEFT + slot * (2 * R + 14) + R);
      return top + depth * LEVEL;
    }

    for (const root of state.roots || []) {
      if (root.text !== undefined) {
        vars.push({ name: root.name, x: LEFT - 40, y, text: root.text });
        y += ROW * 0.6;
        continue;
      }
      vars.push({ name: root.name, x: LEFT - 40, y });
      const id = root.to;
      if (id == null) { vars[vars.length - 1].text = '[]'; y += ROW * 0.6; continue; }
      if (pos[id]) {
        arrows.push({ from: [LEFT - 34, y], to: id, root: true });
        y += ROW * 0.6;
      } else if (nodes[id] && nodes[id].k === 'tree') {
        arrows.push({ from: [LEFT - 34, y], to: id, root: true, side: true });
        const bottom = placeTree(id, y);
        y = bottom + ROW;
      } else {
        arrows.push({ from: [LEFT - 34, y], to: id, root: true });
        placeList(id, LEFT, y);
        y += ROW;
      }
    }
    return { pos, arrows, vars, nils, width: width + 10, height: y - ROW / 2 + 10 };
  }

  function el(tag, attrs, parent) {
    const e = document.createElementNS(NS, tag);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }

  // Where an arrow to a node ends: its left edge, or, from another row, its
  // top or bottom; a tree's edges end at the top of the child.
  function target(p, a) {
    const [, y1] = a.from;
    if (p.k === 'tree') {
      if (a.tree) return [p.x, p.y - R];
      if (Math.abs(y1 - p.y) < 1 || (a.root && a.side)) return [p.x - R - 2, p.y];
      return y1 > p.y ? [p.x, p.y + R + 2] : [p.x, p.y - R - 2];
    }
    if (Math.abs(y1 - p.y) < 1) return [p.x - 2, p.y];
    return y1 > p.y ? [p.x + BOX / 2, p.y + BOX / 2 + 2] : [p.x + BOX / 2, p.y - BOX / 2 - 2];
  }

  function arrowPath(a, p) {
    const [x1, y1] = a.from;
    const [x2, y2] = target(p, a);
    if (Math.abs(y1 - y2) < 1 || a.tree) return `M${x1},${y1} L${x2},${y2}`;
    if (Math.abs(x2 - (p.x + (p.k === 'cons' ? BOX / 2 : 0))) < 1) {
      // Into the top or bottom of a node: out sideways, then in upright.
      return `M${x1},${y1} C${(x1 + x2) / 2},${y1} ${x2},${(y1 + y2) / 2} ${x2},${y2}`;
    }
    const mx = Math.max(x1 + 20, x2 - 30);
    return `M${x1},${y1} C${mx},${y1} ${x2 - 40},${y2} ${x2},${y2}`;
  }

  // Draws a state into svg; with keep, nodes already drawn move to their new
  // places rather than being drawn again.
  function draw(svg, state, keep) {
    const L = layout(state);
    const nodes = state.nodes || {};
    const hl = new Set(state.highlight || []);
    svg.setAttribute('viewBox', `0 0 ${Math.max(L.width, 200)} ${Math.max(L.height, 40)}`);
    svg.setAttribute('width', Math.max(L.width, 200));
    let defs = svg.querySelector('defs');
    if (!defs) {
      defs = el('defs', {}, svg);
      const m = el('marker', { id: 'xs-head', viewBox: '0 0 10 10', refX: 9, refY: 5,
        markerWidth: 7, markerHeight: 7, orient: 'auto-start-reverse' }, defs);
      el('path', { d: 'M0,0 L10,5 L0,10 z', fill: 'currentColor' }, m);
    }
    let layer = svg.querySelector('g.nodes');
    if (!layer || !keep) {
      svg.querySelectorAll('g.nodes, g.edges, g.vars, g.marks').forEach((g) => g.remove());
      el('g', { class: 'edges' }, svg);
      el('g', { class: 'vars' }, svg);
      layer = el('g', { class: 'nodes' }, svg);
      el('g', { class: 'marks' }, svg);   // over the boxes: dots, and [] slashes
    }
    const edges = svg.querySelector('g.edges');
    const varsG = svg.querySelector('g.vars');
    const marks = svg.querySelector('g.marks');
    edges.textContent = '';
    marks.textContent = '';
    varsG.textContent = '';
    const seen = new Set();
    for (const id in L.pos) {
      const p = L.pos[id], n = nodes[id];
      seen.add(id);
      let g = layer.querySelector(`g.node[data-id="${CSS.escape(id)}"]`);
      const fresh = !g;
      if (fresh) {
        g = el('g', { class: 'node entering', 'data-id': id }, layer);
        if (n.k === 'cons') {
          el('rect', { x: -BOX / 2, y: -BOX / 2, width: BOX, height: BOX }, g);
          el('rect', { x: BOX / 2, y: -BOX / 2, width: BOX, height: BOX }, g);
          el('text', { x: 0, y: 0 }, g).textContent = n.v;
        } else {
          el('circle', { cx: 0, cy: 0, r: R }, g);
          el('text', { x: 0, y: 0 }, g).textContent = n.v;
        }
      }
      // A cons drawn from its value box's centre.
      const x = n.k === 'cons' ? p.x + BOX / 2 : p.x;
      g.style.transform = `translate(${x}px, ${p.y}px)`;
      g.classList.toggle('hl', hl.has(id));
      if (fresh) requestAnimationFrame(() => requestAnimationFrame(() => g.classList.remove('entering')));
    }
    layer.querySelectorAll('g.node').forEach((g) => { if (!seen.has(g.dataset.id)) g.remove(); });
    // The pointer boxes' arrows start at their centres: shift by the half box.
    for (const a of L.arrows) {
      const p = L.pos[a.to];
      if (!p) continue;
      el('path', { class: 'arrow', d: arrowPath(a, p) }, edges);
      // A pointer starts at a dot in the middle of its box.
      if (!a.root && !a.tree) el('circle', { class: 'dot', cx: a.from[0], cy: a.from[1], r: 3 }, marks);
    }
    for (const [x, y] of L.nils) {
      el('line', { class: 'nil', x1: x - BOX / 2 + 4, y1: y + BOX / 2 - 4, x2: x + BOX / 2 - 4, y2: y - BOX / 2 + 4 }, marks);
    }
    for (const v of L.vars) {
      el('text', { class: 'var', x: v.x - (v.text !== undefined ? 30 : 0), y: v.y }, varsG).textContent = v.name;
      if (v.text !== undefined) el('text', { x: v.x - 4, y: v.y, 'text-anchor': 'start', style: 'text-anchor: start' }, varsG).textContent = '= ' + v.text;
    }
  }

  class Stepper extends HTMLElement {
    constructor() {
      super();
      this._root = this.attachShadow({ mode: 'open' });
      this._root.innerHTML = `<style>${css}</style>
        <div class="frame">
          <div class="head"><span class="title"></span><span class="count" aria-live="polite"></span></div>
          <div class="body">
            <pre class="code" hidden></pre>
            <div class="state">
              <div class="label"></div>
              <svg class="main" role="img"></svg>
              <div class="note"></div>
            </div>
          </div>
          <div class="ask" hidden>
            <div class="prompt">What comes next?</div>
            <div class="options"></div>
            <div class="feedback" aria-live="polite"></div>
          </div>
          <div class="controls">
            <button class="first" aria-label="First state">⏮</button>
            <button class="back" aria-label="Back">◀</button>
            <button class="next" aria-label="Next">▶</button>
          </div>
        </div>`;
      const $ = (s) => this._root.querySelector(s);
      this._ui = { title: $('.title'), count: $('.count'), label: $('.label'), svg: $('svg.main'),
        code: $('.code'), body: $('.body'),
        note: $('.note'), ask: $('.ask'), options: $('.options'), feedback: $('.feedback'),
        first: $('.first'), back: $('.back'), next: $('.next') };
      this._ui.first.onclick = () => this.reset();
      this._ui.back.onclick = () => this.back();
      this._ui.next.onclick = () => this.step();
      this._steps = [];
      this._i = 0;
      this._asking = null;      // the index being asked for, while asking
      this._answered = new Set();
      this.answers = [];        // { step, correct, why }
    }

    set data(s) {
      this._data = s;
      const d = typeof s === 'string' ? JSON.parse(s) : s;
      this._d = d;
      if (d.id) this.id = d.id;
      this._steps = d.steps || [];
      this._renderCode(d.code);
      this._mode = this.getAttribute('mode') || d.mode || 'predict';
      this._i = 0;
      this._asking = null;
      this._answered = new Set();
      this.answers = [];
      this._ui.title.textContent = d.title || '';
      this._show(false);
    }
    get data() { return this._data; }

    get mode() { return this._mode; }
    set mode(m) { this._mode = m; this._asking = null; this._show(true); }
    get index() { return this._i; }
    get length() { return this._steps.length; }
    get asking() { return this._asking !== null; }

    // Whether the reader is asked before step i is shown.
    _asks(i) {
      if (this._mode !== 'predict' || this._answered.has(i)) return false;
      const s = this._steps[i];
      if (!s || !s.choices || s.choices.length === 0) return false;
      const ask = this._d.ask === undefined ? 2 : this._d.ask;
      return i <= ask || i === this._steps.length - 1;
    }

    step() {
      if (this._asking !== null) { this._reveal(null); return true; }
      if (this._i >= this._steps.length - 1) return false;
      if (this._asks(this._i + 1)) { this._ask(this._i + 1); return true; }
      this._i++;
      this._show(true);
      this._changed();
      return true;
    }

    back() {
      if (this._asking !== null) { this._asking = null; this._show(true); return true; }
      if (this._i === 0) return false;
      this._i--;
      this._show(true);
      this._changed();
      return true;
    }

    reset() { this._i = 0; this._asking = null; this._show(true); this._changed(); }

    _changed() {
      this.dispatchEvent(new CustomEvent('x-stepper-step', { bubbles: true, detail: { index: this._i } }));
    }

    _ask(i) {
      this._asking = i;
      const s = this._steps[i];
      const opts = shuffle([{ state: s, right: true }, ...s.choices.map((c) => ({ ...c, right: false }))], i + 7);
      const ui = this._ui;
      ui.ask.hidden = false;
      ui.feedback.textContent = '';
      ui.feedback.className = 'feedback';
      ui.options.textContent = '';
      opts.forEach((o, k) => {
        const b = document.createElement('button');
        b.className = 'option';
        b.setAttribute('aria-label', 'Option ' + (k + 1) + ': ' + (o.state.label || ''));
        const lab = document.createElement('span');
        lab.className = 'olabel';
        lab.textContent = o.state.label || '';
        b.appendChild(lab);
        const svg = document.createElementNS(NS, 'svg');
        b.appendChild(svg);
        draw(svg, o.state, false);
        svg.style.width = Math.min(260, +svg.getAttribute('width') * 0.6) + 'px';
        b.onclick = () => this._choose(o, b);
        b._option = o;
        ui.options.appendChild(b);
      });
      this._updateControls();
    }

    _choose(o, button) {
      if (this._asking === null) return;
      const i = this._asking;
      this.answers.push({ step: i, correct: o.right, why: o.why || null });
      const ui = this._ui;
      ui.options.querySelectorAll('.option').forEach((b) => {
        b.disabled = true;
        if (b._option.right) b.classList.add('right');
      });
      if (!o.right) button.classList.add('wrong');
      ui.feedback.className = 'feedback ' + (o.right ? 'right' : 'wrong');
      ui.feedback.textContent = o.right ? 'Right.' : 'Not quite: ' + (o.why || 'that is not what happens.');
      if (this._d.token !== undefined)
        this.dispatchEvent(new CustomEvent('x-ocaml-callback', { bubbles: true,
          detail: { token: this._d.token, payload: { step: i, correct: o.right } } }));
      this.dispatchEvent(new CustomEvent('x-stepper-answer', { bubbles: true, detail: { step: i, correct: o.right, why: o.why || null } }));
      this._reveal(o);
    }

    // Shows the state asked for; the question and its feedback stay until
    // the next step.
    _reveal(chosen) {
      const i = this._asking;
      this._asking = null;
      this._answered.add(i);
      this._i = i;
      if (!chosen) this._ui.ask.hidden = true;
      this._show(true, true);
      this._changed();
    }

    _renderCode(code) {
      const ui = this._ui;
      ui.code.textContent = '';
      ui.code.hidden = !code;
      ui.body.classList.toggle('with-code', !!code);
      if (!code) return;
      code.replace(/\n+$/, '').split('\n').forEach((text, i) => {
        const ln = document.createElement('span');
        ln.className = 'ln';
        const no = document.createElement('span');
        no.className = 'no';
        no.textContent = i + 1;
        ln.append(no, text || ' ');
        ui.code.appendChild(ln);
      });
    }

    _markLine(line) {
      this._ui.code.querySelectorAll('.ln').forEach((ln, i) => ln.classList.toggle('now', i + 1 === line));
    }

    _show(keep, keepAsk) {
      const ui = this._ui;
      const s = this._steps[this._i];
      if (!keepAsk && this._asking === null) ui.ask.hidden = true;
      if (!s) { ui.label.textContent = ''; ui.note.textContent = ''; return; }
      ui.label.textContent = s.label || '';
      ui.note.textContent = s.note || '';
      ui.count.textContent = `${this._i + 1} / ${this._steps.length}`;
      ui.svg.setAttribute('aria-label', s.label || 'state ' + (this._i + 1));
      draw(ui.svg, s, keep);
      this._markLine(s.line);
      this._updateControls();
    }

    _updateControls() {
      const ui = this._ui;
      ui.back.disabled = this._i === 0 && this._asking === null;
      ui.first.disabled = this._i === 0 && this._asking === null;
      ui.next.disabled = this._i >= this._steps.length - 1 && this._asking === null;
      ui.next.textContent = this._asking !== null ? 'Show' : '▶';
    }
  }

  customElements.define('x-stepper', Stepper);
})();
