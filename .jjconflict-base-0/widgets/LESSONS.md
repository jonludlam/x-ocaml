# Lessons from building widgets, Ui and their demos

Notes from writing `widgets/`, its `Ui` toolkit and the examples that use
them, first on js_top_worker and then, rebuilt, on x-ocaml's own worker:
what surprised us, what broke, and what we would do the same way again.

## Where it belongs

**Let things belong to what already has a lifetime.** The first widget
renderer lived beside the cells and had to work out for itself which cell
a widget belonged to (a queue of cells sent to the worker), when to clear
it, and how to stop the worker drawing ghosts. Cell output already had all
of that. The rebuild gave x-ocaml one idea, a *display* owned by the cell
whose code made it, and the placement, clearing and ghost code went away.

**The extension point was already on the page.** Asked how to add
channels to x-ocaml's protocol, we looked at string channels, extensible
variants (Marshal loses their identity between the page and the worker),
MessagePorts and more. The answer was custom elements: x-ocaml is a web
component, and its extensions are too. The core gained four messages and
knows nothing of widgets.

**A library brings its own page half.** `X_ocaml_lib.require` has the page
load a script from beside the worker, so a page author adds nothing.
Loading page halves into x-ocaml.js's own runtime was tried and works in a
dev build, but a release build then needs `--toplevel` and `-linkall`,
which tripled x-ocaml.js for every page (273 KB to 875 KB gzipped) to save
a plugin 30 KB. A plugin with its own runtime is 104 KB (34 KB gzipped) and
is tied to no particular x-ocaml build.

**Custom elements have a trap.** A property set on an element before its
definition arrives becomes the element's own and hides the definition's
setter: the element showed nothing. The core, not each plugin, waits for
`customElements.whenDefined` before handing data over.

**Other people's bugs surface at the seams.** Defining a second custom
element found that x-ocaml put its `connectedCallback` on
`HTMLElement.prototype` itself, making every other custom element on the
page a cell. Running Ui's bigger cells on x-ocaml's worker found that OCaml
5 calls `caml_ensure_stack_capacity` in any function with a frame over
about 32 words, which the toplevel refused. Both were fixed where they
were, in x-ocaml, as commits of their own.

## The design

**Track reads rather than subscriptions.** A root records the variables it
reads while it draws, and draws again when one changes. Nobody writes
"redraw when x changes", so nothing gets forgotten. The price is a rule that
people will trip over: build widgets *outside* `dyn`. A widget made inside
`dyn` is made again at every redraw and loses its own state (a table's
sorting, the chosen tab).

**Batch what a handler changes.** A handler that sets two variables would
otherwise draw twice, once in a state that never really existed. Handlers
run inside `Var.batch`, and a test counts the redraws.

**Work around the protocol, don't fight it.** js_top_worker's `Widget.update`
cannot change a widget's handlers; only `display` can. So each redraw calls
`display` again with the complete handler list. The renderer patches in
place, so this costs nothing on the page. Handler keys come from a counter
taken when each widget is built, so they stay stable across redraws.

**A local open with generic names is a trap.** `Interact.( let+ f = float
"β" 0.3 in ... )` looks neat, but inside it `float` and `text` are the
Interact controls, and the body broke as soon as it wrote `float (b - a)`.
Exporting `let+` and `and+` from `Ui` itself keeps the body in normal
scope; the controls are written `Interact.float`.

**`=` raises on functions, and choices often hold them.** A dropdown of
functions, or a variable that holds a widget, blew up in `compare`. The
toolkit's equality tries `==` first and treats an `=` that raises as "not
equal". Even then, every mention of a primitive such as `sin` builds a
fresh closure. Two controls with their own `[("sin", sin); ...]` lists
disagree about which option is chosen. They must share one list, and the
docs say so.

**Timers need backpressure.** A timer that ticks every 16 ms while each
tick takes 100 ms fills the worker's queue, and a Stop press waits behind
all the queued ticks. The timer adapter now sends a tick only once the
worker has reported the previous one done: the timer's config carries a
count of the ticks handled. A second bug hid in the same place: a timer
rebuilt with a new handler kept the old tick count and never ticked again.

**The worker must hear when a widget goes.** When a cell ran again, its
widgets went from the page, but the worker's roots stayed subscribed to
their variables, and the next change drew them again, as ghosts under
some other cell. First the page told the worker; now the worker knows by
itself, as a display's `on_remove` runs when its cell runs again, and the
widget's `x-ocaml:removed` handler stops the root.

**Events need to say more than "it happened".** A chart must know where a
click landed, a game which key was pressed. Pointer positions come in the
SVG's own units (through `getScreenCTM`), so a chart can invert its scales
exactly. The worker cannot call `preventDefault` in time, because it answers
later, so a view declares it with `data-x-ocaml-prevent`. Pointer moves are
coalesced to one per frame, or dragging floods the worker.

**Allow managed widgets inside views.** At first a managed widget (drawn by
JavaScript) could only be a whole widget. That made the timer awkward:
`play` needs a timer next to its buttons. Allowing managed widgets inside
views made timers, and anything else of the kind, composable.

**The view owns an input's value, except while the reader is typing.** A
view that always says `value = ""` must empty the input after Enter, so the
view is authoritative at every redraw. But a program that echoes the input
runs a keystroke behind, so values the input itself sent recently are
never written back. A program that reformats what is typed (the converter
showing `"20"` for `"20."`) still fights the reader. The fix belongs in the
program: show the field being edited exactly as typed.

**Put the styles in the view.** Each root's view includes a `<style>`
element. Patching makes it free after the first draw, it needs no setup,
and scoping everything under `.xui` keeps page and toolkit styles apart.
CSS custom properties set inline (`--xui-fill`) let each slider draw its
filled track without extra elements.

## The runtime

**Ints are 32 bits.** In the sieve, `i * i` overflowed for `i` above
46,340, went negative, and ended in "index out of bounds". Nothing suggests
this until it happens.

**The stack is small.** A non-tail-recursive `gaps` over 18,000 primes
overflowed. Anything recursive over data a reader can make large must be
tail-recursive.

**The worker is fast enough for real work.** 480×320 Mandelbrot pixels at 200
iterations take 50–70 ms, and a Life step with a redraw fits in 60 ms. A 24-bit
BMP built in OCaml and base64-encoded is the easiest image format to make.

## Writing examples

**Cell code is HTML first.** `"<b>not bold</b>"` inside `<x-ocaml>` became
a real `<b>` element, and the cell's code said `"not bold"`. Escape `<` in
strings, and put spaces around comparisons.

**Cells share one toplevel.** `let log = Output.create ()` in one cell hid
`Stdlib.log` from every later cell, and a later example broke with "this is
not a function".

**Toplevel output buries the widgets.** Forty lines of `val ... = <abstr>`
sit between a cell's code and its widget. Wrapping the definitions in
local `let ... in` helps; an x-ocaml attribute to quiet a cell's output
would be better.

## Testing

**Prove that a test can fail.** The first typing test passed with the fix
removed, because the worker answered faster than the simulated keystrokes.
Only a deliberately slow handler made the echoes land mid-typing. Break
the code on purpose and watch the test go red.

**Distrust the easy green.** One run "passed" against a stale x-ocaml.js
from the browser cache. The suite clears the cache; ad-hoc scripts must
too.

**"Runs without errors" meant very little.** A page looked settled before
any cell had started (on js_top_worker, before its universe had loaded), and cells that failed to
compile counted as fine. Two examples that passed were broken. The check
now waits for an answer and fails on a cell's error, with
`data-expect="error"` for cells that show errors on purpose.

**Real mice meet real layout.** CodeMirror measures a cell only once it
scrolls into view. A board moved 773 px just after `scrollIntoView`, so
presses landed on the cell above it, and "painting doesn't work" was a
test artefact. Readers see the same jump. Synthetic events have their own
quirks: `clientX` is truncated to a whole pixel.

**Watch for timing assumptions.** "The nested widget waits for its adapter"
failed because the adapter's cell had already run by the time of the check.
The honest check is the outcome: shown first, it starts later.

**Look at the thing.** Screenshots found the full-width badges, the heavy
default slider track and the oversized fitter chart. Element screenshots of
tall widgets lie, though: scroll, wait, and take the viewport instead.

**Know when a page has settled.** The demos' cells take twenty seconds to
run on x-ocaml's worker; a click sent before then waits behind them, and a
test that pressed Run early concluded the clock was broken. Wait for what
the page shows, not for a time. Formatting every cell with ocamlformat took
eight of the thirty seconds, so the big pages turn it off.
