// <x-score>: draws the ABC notation OCaml sends it, with abcjs, which it
// loads from a CDN the first time, and plays it, lighting each note as it
// sounds. data: {"abc", "play" (the same music, as it is to be played),
// "width" (at most, in pixels), "marks": [[the place in the text where a
// note starts, its colour]...]}.
const abcjsLoaded = new Promise((resolve) => {
  const js = document.createElement('script');
  js.src = 'https://cdn.jsdelivr.net/npm/abcjs@6.4.4/dist/abcjs-basic-min.js';
  js.onload = resolve;
  document.head.append(js);
});
// Crotchets a minute: a little quicker than Schiff's 59.
const QPM = 66;
customElements.define('x-score', class extends HTMLElement {
  set data(s) {
    const d = JSON.parse(s);
    this.stop();
    if (!this.paper) {
      this.innerHTML = `<style>
          x-score { display: block; }
          x-score .frame { background: #fff; color: #000; border-radius: 6px; padding: 0.3rem 0.4rem 0; }
          x-score .paper .playing, x-score .paper .playing * { fill: #e8661d !important; }
          x-score button { font: inherit; font-size: 0.9em; padding: 0.2rem 0.8rem; margin-top: 0.3rem; cursor: pointer; }
        </style><div class="frame"><div class="paper"></div></div><div><button>▶ Play</button></div>`;
      this.frame = this.querySelector('.frame');
      this.paper = this.querySelector('.paper');
      this.button = this.querySelector('button');
      this.button.onclick = () => (this.playing ? this.stop() : this.play());
    }
    this.frame.style.maxWidth = d.width + 'px';
    abcjsLoaded.then(() => {
      this.tune = ABCJS.renderAbc(this.paper, d.abc, { add_classes: true, responsive: 'resize', staffwidth: d.width })[0];
      // What is heard is drawn off the page: it has the same timing as
      // what is seen, so the notes seen light up as those heard sound.
      this.heard = ABCJS.renderAbc(document.createElement('div'), d.play)[0];
      // Each note is found by where it starts in the text, less the
      // spaces that abcjs counts as part of it.
      const colours = new Map(d.marks);
      for (const line of this.tune.lines) for (const staff of line.staff || []) for (const voice of staff.voices) for (const el of voice) {
        let c = el.startChar;
        while (d.abc[c] === ' ') c++;
        const colour = el.el_type === 'note' && colours.get(c);
        if (colour) for (const node of el.abselem.elemset) node.style.fill = colour;
      }
    });
  }
  async play() {
    this.playing = true;
    this.button.textContent = '■ Stop';
    this.audio = this.audio || new AudioContext();
    await this.audio.resume();
    const synth = (this.synth = new ABCJS.synth.CreateSynth());
    await synth.init({ audioContext: this.audio, visualObj: this.heard, options: { qpm: QPM } });
    await synth.prime();
    if (!this.playing) return;
    const lit = [];
    this.timer = new ABCJS.TimingCallbacks(this.tune, {
      qpm: QPM,
      eventCallback: (ev) => {
        for (const e of lit.splice(0)) e.classList.remove('playing');
        if (!ev) return this.stop();
        for (const group of ev.elements || []) for (const e of group) (e.classList.add('playing'), lit.push(e));
      },
    });
    synth.start();
    this.timer.start();
  }
  stop() {
    this.playing = false;
    if (this.button) this.button.textContent = '▶ Play';
    if (this.synth) this.synth.stop();
    if (this.timer) this.timer.stop();
    this.paper && this.paper.querySelectorAll('.playing').forEach((e) => e.classList.remove('playing'));
  }
  disconnectedCallback() { this.stop(); }
});
