# focs-check: notes, measurements and open questions

Work on branch `focs-check` (from `widgets-on-displays`), for the tasks in
`~/devel/focs-slipshow/AGENT-TASKS.md`, section B. The design is
`~/devel/focs-slipshow/EXERCISES.md`, sections 2 to 6.

## Building and testing

- `dune build` works from the opam switch (5.4.1); nothing needed
  installing. The submodules had to be checked out in the new worktree with
  `git -c protocol.file.allow=always submodule update --init` (the shared
  config points them at the main checkout's copies).
- `npm install` in `test/`, then `dune build @runbrowser`.
- Baseline on this branch before any change: every page up to
  `test_ui.html` passes; `test_ui.html` times out (as the commit
  "Worker: register the predefined exceptions" says), and since the runner
  stops at the first timeout, the examples after it are never run. New
  pages are listed before `test_ui.html` so that they do run.

## B1. Warning 51 in the browser

`test/test_tailcall.html`, on both workers (`?worker=effects` for the
effects one). **Warning 51 fires in the browser, on both workers**, exactly
as in the native toplevel:

| case | plain | effects |
|---|---|---|
| `n + (nsum [@tailcall]) (n - 1)` | warning 51 | warning 51 |
| `(summing [@tailcall]) (n - 1) (n + t)` | none | none |
| call under `try ... with` | warning 51 | warning 51 |
| mutual recursion `even`/`odd` in `match` branches | none | none |

One surprise, the same in the native toplevel: with
`[@@@ocaml.warnerror "+51"]` the toplevel prints
`Error (warning 51 ...)` **but still defines the function**. Warning 51
comes from `Simplif`, after typing, and the toplevel does not abort the
phrase for it. So a tail-call checker must read the diagnostics, not rely
on the phrase failing.

## B2. Stack depth in the browser

`test/test_stack.html`, both workers, headless Chromium (Playwright's).

| | plain worker | effects worker |
|---|---|---|
| deepest `nsum n` that returns | **6309** (binary search) | between **6000 and 7000** (probes) |
| `nsum 100_000` | Stack_overflow | Stack_overflow |
| `summing n 0`, n = 10⁵, 10⁶, 10⁷ | fine | fine |
| `even`/`odd` mutual tail recursion, 10⁶ | fine | fine |
| tail calls through a closure (`count_down 1 (fun () -> go (n-1))`), 10⁶ | **Stack_overflow** | fine |
| `List.map`, `@`, `List.rev` on 10⁶ | fine | fine |
| `List.fold_right` on 10⁶ | **Stack_overflow** | fine |
| catching `Stack_overflow` | works | **unreliable**: an overflow just past the limit escapes the handler and reaches the toplevel ("Stack overflow during evaluation"); `nsum 100_000` is caught |

What it means for the lecture:

- The demonstration works: `nsum 100_000` overflows and `summing 100_000 0`
  (and 10⁶, 10⁷) does not, on both workers.
- But the limit is very low: about 6300 frames of `nsum`, so even
  `nsum 10_000` overflows. Anything students write that recurses over a list
  of 10⁴ elements without tail calls fails. (Natively, OCaml 5's stack is
  much bigger; the bytecode toplevel handles `nsum 100_000`.)
- On the effects worker, calls that go through CPS (higher-order functions
  such as `fold_right`, closures) do not grow the JS stack, so "is it tail
  recursive" cannot be judged by whether it overflows. The `[@tailcall]`
  check (B1) is the reliable one.
- Don't let a test depend on catching `Stack_overflow` on the effects
  worker.

## B3. Reading an exercise's code, and loading it apart

What was added:

- **Worker** (commit "Worker: the code of named cells, and a hook after
  each phrase"): the page sends a named cell's code (and its first line)
  as it runs it (`X_protocol.Source`); `X_ocaml_lib.source "id"` reads it.
  `X_ocaml_cells.after_phrase` runs something once the current phrase has
  finished: needed because the toplevel ends a phrase by setting its
  environment to the one it computed *before* running it, which forgets
  modules that code in the phrase defined by running phrases of its own.
- **`focs-check/core`** (`Focs_check_core`, compiler-libs only, so the same
  code runs natively and in the worker): `Isolated` parses, rewrites,
  type-checks and evaluates code as `module Name = struct prelude code end`
  in the toplevel, with the warning reporter replaced so diagnostics are
  kept, not printed; `value ~typ e` evaluates an expression at a type and
  hands back the value.
- **`focs-check/cell`** (`Focs_check`, exported to cells by both workers;
  its interface has only stdlib types, since cells see no compiler-libs):
  `Focs_check.source`, `load ~as_:"Student" "rev-ex"`, `load_code`,
  `print_diagnostics`. A test's later phrases (and later cells) use
  `Student`.

Gotchas found:

- Warnings made errors are counted by the compiler (`Warnings.nerrors`),
  and `Warnings.restore` does not reset that count: the cell's *next*
  phrase then failed silently with "Some fatal warnings". Fixed with
  `Warnings.reset_fatal`.
- Line numbers in diagnostics are the page's (the cell's lines continue
  from the cells before it, test cells included), matching what x-ocaml
  itself reports for the exercise.
- **A pre-existing worker bug**, found by a check on `module L = List`: a
  cell defining a module alias stopped the worker (`Misc.Fatal_error`
  from `Topeval.getvalue` in `Environment.capture`). Recorded as a failing
  test in `test_text.html` (X9), then fixed: commits "Tests: a module alias
  stops the worker" and "Worker: a module alias no longer stops it". This
  is on `widgets-on-displays` too, and worth taking on its own.

Tests: `test/test_check.html` (12 checks, both workers, all pass);
`dune test` (native) for `Isolated`.

## B4. The checks

`focs-check/core`, tested natively by `dune test` (bytecode program with
the toplevel in it): 9 Isolated, 6 Tailcheck, 13 Count/Growth, 21 Ban
checks, 5 Examplar, all pass. In the browser, `test/test_focs_check.html`
(12 checks, both workers, all pass).

- **Tailcheck**: marks every call to a function of its enclosing `let rec`
  group (structure or expression level) `[@tailcall]`, loads with `+51`,
  maps warnings back to calls by location. nsum: 1 non-tail call; summing:
  none; power: 1 of 2 (the odd branch); call under `try`: flagged; inner
  `go` with accumulator: fine, `1 + go xs`: flagged; mutual `odd` calling
  `not (even ...)`: flagged.
- **Count**: ticks at each function-body entry (and each case of a
  `function`) and each `::` built; a prelude shadows `( @ )` and `List`
  with counting versions (a step per element gone through or built; sorts
  count comparisons). `measure ~input ~call sizes` counts only `call`;
  fuel (default 10⁷ steps) stops runaway code (returns `None`).
- **Growth**: least-squares fit of `a·g(n) + b` for 1, log n, n, n log n,
  n², n³, and `a·bⁿ` (log-linear) for exponential; score = max relative
  error; models within 1.5× of the best (or under 2%) are tied and the
  slower-growing wins. Feedback: a table of n / steps / ratio and "when the
  input doubled, it did about 4.0 times the work: O(n^2)".

  | function | input | sizes | ratios at the end | verdict |
  |---|---|---|---|---|
  | nrev | 0..n-1 | 64..2048 | 3.99 | O(n^2) |
  | nrev via List.append | | | 3.99 | O(n^2) |
  | rev (accumulator) | | | 2.00 | O(n) |
  | List.rev | | | 2.00 | O(n) |
  | nsum | n | 64..2048 | 2.00 | O(n) |
  | power 1 n | n | 1000..128000 | 1.06 | O(log n) |
  | insertion sort | sorted | 64..2048 | 2.00 | O(n) |
  | insertion sort | reversed | 64..2048 | 4.00 | O(n^2) |
  | mergesort | pseudo-random | 64..4096 | 2.17 | O(n log n) |
  | naive fib | n | 10..18 | 1.62 | O(2^n) |
  | a triple loop | n | 8..128 | 8.00 | O(n^3) |

- **Ban** (Typedtree): every `Texp_ident` normalised with
  `Env.normalize_value_path`, and `Stdlib__List` renamed `Stdlib.List`. So
  `module L = List` / `L.rev`, `open List` / `rev`, `let r = List.rev` all
  count as `Stdlib.List.rev`; the reader's own `rev` does not. Rules:
  `Forbid_value`, `Forbid_module`, `Forbid`/`Require` of `While`, `For`,
  `Let_rec`, `Recursion` (a let rec that calls itself), `Mutation`, `Try`,
  `Array`; `Require_value`; `recursive_functions` (for "only one recursive
  function", 8.3).
- **D8, `Forbid_library allowed`**: no value from any module of the
  standard library, however reached (aliases, open, `Stdlib.List...`),
  except the modules allowed; what is open by default (`+ = @ failwith
  raise`) stays allowed unless written `Stdlib.x`. Matches
  `exercises/harness.ml`'s `uses_library`: 3.1's `fold-left` and
  `stdlib-qualified` chaffs are caught, its wheat is not. One difference:
  the harness shadows only List, Array, Seq, Option, Fun, String, Hashtbl,
  ListLabels, ArrayLabels, so e.g. `Printf` or `Char` pass there and are
  caught here unless allowed.

## B6. Predict the next step

- `examples/elements/stepper.js`: `<x-stepper>`, a custom element taking
  JSON (format in the file's header). Generic renderer: lists as
  box-and-pointer chains, trees top-down, named roots as arrows into
  them, a node shared by two roots drawn once with two arrows; node ids
  persist across states so nodes slide rather than redraw; a highlight.
  Should do for traversals, BST path copying, queues (two lists), and refs
  could be added as another node kind.
- Predict mode: for the first `ask` steps (default 2) and the last, the
  next state is withheld and the reader picks among it and the wrong
  states, each with a "why" (refutation text); feedback, then the real
  state. Lecture mode never asks. **JS API for slips**: `el.step()`,
  `el.back()` (for `slip.onUndo`), `el.reset()`, `el.mode = "lecture"`,
  `el.index`, `el.asking`, `el.answers`; `step()` while asking reveals.
  Events `x-stepper-step`, `x-stepper-answer`, and an x-ocaml callback if
  the data has a `token`. The data's `id` becomes the element's id, so a
  slip-script can `document.getElementById("rev-steps").step()`.
- OCaml side: `Focs_check.Steps` (`list`, `tree`, `text`, `state`, `add
  ~choices`, `show`), finding sharing by physical equality.
- `test/test_stepper.html` (22 checks): rev with accumulator in predict
  mode (wrong choices: accumulator built at the end, head lost,
  accumulator replaced) and a preorder traversal in lecture mode.
  `examples/predict.html`: rev of 4 elements, and pre/in/postorder with the
  other orders' next node as the wrong choices.
- Not done: the cheaper "click the next node" and "drag to build" modes,
  typing the next reduction step, and wiring answers back to a grade.

## B5. Examples first (Examplar)

`examples/examplar.html`, checked by `test/test_examplar.html` in a frame
(8 checks, pass). Three questions (last, butlast, nth) with tick 2's wheats
and chaffs copied from `~/devel/focs/chaffs/tick2/*.ml` (the other agent's
work, read from its working tree). A hidden cell defines a correct version
(so that the reader's examples run in their cell and show `true`/`false`)
and the wheats and chaffs as source strings; the reader writes
`let examples = [ last [ 1; 2; 3 ] = 3 ]`; the test calls
`Focs_check.examplar ~wheats ~chaffs "last-examples"`, which prints each
example (valid, or "a correct version does not agree", or does not
compile), the chaffs caught by what they get wrong, "Caught 2 of 5 wrong
versions", and passes when all are caught and none is wrong.

Each example is run in its own module after each implementation's code
(examples × implementations loads), so a wrong example cannot hide the
others. Examples may also be a cell's phrases, one per line.

## D6. An exercise in its own worker

**Already possible, with no code**: a second `<script src="x-ocaml.js"
src-worker="..." elt-name="x-solo">` makes a second set of cells
(`<x-solo>`) with its own worker. `test/test_own_worker.html`: a `change`
with a zero coin running forever in an `<x-solo>` cell is stopped, and an
ordinary cell afterwards still sees the definitions of the cells before
it. Costs: x-ocaml.js is loaded and run twice (9 MB dev build, cached
after the first), a second worker starts (its own 34 MB dev script parse
and toplevel), and the solo cells see none of the other cells'
definitions (a hidden helper has to be repeated there).

**Per-cell workers in one page** (`<x-ocaml data-worker="change">`), not
done: `Page` would keep a group per worker name (Client, setup messages,
Displays, the chain of cells, the tests map), `register` would chain a
cell to the last cell of its group and link tests within it, and Stop
would restart only that group's worker. About 100-150 lines in
`src/page.ml` (Cell needs nothing, the worker nothing). Cost per group as
above. The open design question is what such a cell sees: nothing from
the page, or the page's hidden cells replayed into its worker first.

## Open questions and limits

- **Cheating the tail-call check** with `[@@@warning "-51"]` or
  `[@warning "-51"]` on the binding or the call does not work: warning 51
  comes from `Simplif`, after typing, when the attributes' scopes have
  closed (checked natively; the test passes with or without the defence).
  Tailcheck strips the code's warning attributes anyway, in case another
  compiler version scopes them differently.
- **Tailcheck runs the code** to get warning 51 (it comes from `Simplif`,
  after translation). Code with long top-level computations runs them
  again. Translating to Lambda without running is possible but uses more
  of compiler-libs' internals.
- **The isolated module sees the toplevel's environment**, so a student's
  code can use definitions from earlier cells (hidden helpers, or their
  own experiments). A clean initial environment would need the toplevel's
  initial env saved at startup.
- **Count's cost model** (Jon to decide): List functions cost a step per
  element; `List.length`, `List.nth`, `List.init` the obvious amount;
  sorts a step per comparison; Array, Hashtbl, String are not counted (ban
  them with `Forbid_module` where it matters).
- **Growth's tolerances** (1.5×, 2%) are tuned only on the cases above.
  Sizes: doubling from 64 to 2048 or 4096 for polynomial/n log n, n+1
  steps for exponential, a wide range for log.
- **B2's stack limit**: about 6300 frames of `nsum`. Students' non-tail
  recursion over 10⁴ elements fails in the browser where it works
  natively. A worker's JS stack size cannot be set from the page.
- **Release build**: see below.
- **Examplar**: the reader's examples cell needs the function defined to
  run in place (a hidden wheat does it). Exceptions in examples (testing
  `last []`) count as "does not agree", which is right when the
  specification leaves `[]` open, as tick 2 does.

## Release build

`dune build --profile=release` (x-ocaml.js 857 KB, plain worker 8.0 MB,
effects worker 9.3 MB, focs-check included): the new pages all pass on it
too (tailcall and stack on both workers, check, focs_check on both
workers, text, stepper, examplar, own_worker); deepest `nsum` 6311 on the
plain worker, as in the dev build. The full suite (`dune build
@runbrowser`, dev) passes every page up to `test_ui.html`, which times out
as it did before this work, so the examples after it are not run by the
runner (examples/examplar.html and predict.html were checked by hand and
by test_examplar.html).
