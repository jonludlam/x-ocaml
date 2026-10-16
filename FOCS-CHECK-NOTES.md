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
