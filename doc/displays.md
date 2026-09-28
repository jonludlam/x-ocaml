# How displays work

A cell's code runs in a web worker; everything the reader sees is on the
page, in the main thread. The two share nothing, and talk only by posting
messages to each other. Displays and callbacks add five messages to that
conversation, and a convention for custom elements on the page.

This is how the parts fit together. `X_ocaml_lib`'s interface, in
`worker-lib/x_ocaml_lib.mli`, says what each function does.

## The parts

```
  main thread: x-ocaml.js                 :  web worker: x-ocaml.worker.js
                                          :
  cells                 src/x_ocaml.ml,   :  worker loop       worker/x_worker.ml
    the <x-ocaml>       cell.ml,          :    runs cells, and callbacks, one at
    editors             client.ml         :    a time, as the page asks
                                          :
  Displays              src/displays.ml   :  toplevel          worker/eval.ml
    a display area after each cell that   :    runs a cell's phrases
    has any; shows, updates and           :
    removes elements; adds a required     :  X_ocaml_lib       worker-lib/
    script to the page; sends an          :    what a cell's code calls to show
    x-ocaml-callback event on as a        :    displays and register callbacks;
    Callback                              :    knows which cell each belongs to
                                          :
  elements                                :  the cell's code
    a library's, defined by a script      :    calls X_ocaml_lib, directly or
    it ships; or <x-ocaml-html>, the one  :    through a library, and makes each
    x-ocaml defines, in src/displays.ml   :    display's data
                                          :
                 <== Display, Update, Remove, Require ===
                 ================= Callback ============>
```

A library that puts things on the page comes in two halves: OCaml, in the
worker, that a cell's code calls and that calls `X_ocaml_lib`; and a script
that defines its element, which `X_ocaml_lib.require` has the page load.

A cell's **display area** is a `<div class="x-ocaml-displays">` that the
page puts straight after the cell's `<x-ocaml>` element when the cell first
shows a display, and takes away when its last display goes. The cell's
displays go in it, in the order they were shown, including those a callback
shows later. It has the one listener for `x-ocaml-callback`: an element's
event bubbles up to it, and it posts the `Callback`, so an element never needs
to know which cell it belongs to.

An element is any custom element with a `data` setter. It is given its data,
a string that only it and its library read, JSON by convention. It calls back
by dispatching a bubbling `x-ocaml-callback` event whose `detail` is
`{ token, payload }`.

In the charts below, `===>` is a message between the threads, `--->` a call
or an event within one, and `*` something done where it is.

## A cell shows an element

```
                         main thread  :  web worker
           element    x-ocaml page         worker loop   X_ocaml_lib    cell code
              |             |         :         |             |             |
              |             | Eval (2, code)    |             |             |
 1            |             |==================>|             |             |
              |             |         :         |             |             |
              |             |         :         | forget_cell 2             |
 2            |             |         :         |------------>|             |
              |             |         :         |             |             |
              |             |         :         | run the phrases           |
 3            |             |         :         |-------------------------->|
              |             |         :         |             |             |
              |             |         :         |             | require     |
 4            |             |         :         |             |<------------|
              |             |         :         |             |             |
              |             | Require (tag, src)|             |             |
 5            |             |<================================|             |
              |             |         :         |             |             |
 6            |             | * add <script src>|             |             |
              |             |         :         |             |             |
              |             |         :         |             | display     |
 7            |             |         :         |             |<------------|
              |             |         :         |             |             |
              |             | Display (2, 7, tag, data)       |             |
 8            |             |<================================|             |
              |             |         :         |             |             |
              | <tag> in area         :         |             |             |
 9            |<------------|         :         |             |             |
              |             |         :         |             |             |
10            |             | * data waits for definition     |             |
              |             |         :         |             |             |
11            | * defined: el.data = data       |             |             |
              |             |         :         |             |             |
              |             | Top_response (2, ..)            |             |
12            |             |<==================|             |             |
              |             |         :         |             |             |
```

1. The reader runs cell 2, or the page loads.
2. Before every `Eval`, `X_ocaml_lib` forgets what the cell's last run showed.
   The first time, there is nothing to forget.
3. The toplevel runs the cell's phrases. For each, it sets `X_ocaml_lib.id` to
   the cell and phrase, so whatever the code shows is known to be cell 2's.
4. A library asks for its element: `require ~tag ~src`. `X_ocaml_lib` asks the
   page once per tag.
5. The message is posted at once, while the cell is still running.
6. The page adds a `<script>` for `src`, resolved against the worker's URL,
   unless the element is already defined or the script already added.
7. `display ~tag data`: `X_ocaml_lib` records display 7 as cell 2's, with its
   `on_remove`, and returns it.
8. The page is told the cell, the display, the tag and the data.
9. The element goes in cell 2's display area, made now for its first display.
10. The script may not have run yet. A property set before an element is
    defined would hide its setter, so the page keeps the latest data and gives
    it on `customElements.whenDefined`.
11. The script defines the element; the page sets its `data`, and it draws.
12. The cell has finished. Its display stays until the cell runs again.

## Updates while the cell runs

```
                         main thread  :  web worker
           element    x-ocaml page         worker loop   X_ocaml_lib    cell code
              |             |         :         |             |             |
              |             | Eval (3, code)    |             |             |
 1            |             |==================>|             |             |
              |             |         :         |             |             |
              |             |         :         |             | html        |
 2            |             |         :         |             |<------------|
              |             |         :         |             |             |
              |             | Display (3, 4, "x-ocaml-html")  |             |
 3            |             |<================================|             |
              |             |         :         |             |             |
              |             |         :         |             | update 4    |
 4            |             |         :         |             |<------------|
              |             |         :         |             |             |
              |             | Update (4, "<b>10%</b>")        |             |
 5            |             |<================================|             |
              |             |         :         |             |             |
              | el.data     |         :         |             |             |
 6            |<------------|         :         |             |             |
              |             |         :         |             |             |
              |             |         :         |             | update 4    |
 7            |             |         :         |             |<------------|
              |             |         :         |             |             |
              |             | Update (4, "<b>20%</b>")        |             |
 8            |             |<================================|             |
              |             |         :         |             |             |
              | el.data     |         :         |             |             |
 9            |<------------|         :         |             |             |
              |             |         :         |             |             |
              |             | Top_response (3, ..)            |             |
10            |             |<==================|             |             |
              |             |         :         |             |             |
```

2-3. `html s` is `display ~tag:"x-ocaml-html" s`.
4-9. Each `update` is posted as it is made, while the worker is still busy
     with the cell. The page's thread is free, so each shows as it arrives.

## The reader uses an element

```
                         main thread  :  web worker
reader     element    x-ocaml page         worker loop   X_ocaml_lib    cell code
   |          |             |         :         |             |             |
   |          |             |         :         |             | callback f  |
 1 |          |             |         :         |             |<------------|
   |          |             |         :         |             |             |
   |          |             |         :         |             | display     |
 2 |          |             |         :         |             |<------------|
   |          |             |         :         |             |             |
   |          |             | Display (2, 7, tag, {..12..})   |             |
 3 |          |             |<================================|             |
   |          |             |         :         |             |             |
   |          | el.data     |         :         |             |             |
 4 |          |<------------|         :         |             |             |
   |          |             |         :         |             |             |
   | click    |             |         :         |             |             |
 5 |--------->|             |         :         |             |             |
   |          |             |         :         |             |             |
   |          | x-ocaml-callback      :         |             |             |
 6 |          |------------>|         :         |             |             |
   |          |             |         :         |             |             |
   |          |             | Callback (12, payload)          |             |
 7 |          |             |==================>|             |             |
   |          |             |         :         |             |             |
   |          |             |         :         | invoke      |             |
 8 |          |             |         :         |------------>|             |
   |          |             |         :         |             |             |
   |          |             |         :         |             | f payload   |
 9 |          |             |         :         |             |------------>|
   |          |             |         :         |             |             |
   |          |             |         :         |             | update 7    |
10 |          |             |         :         |             |<------------|
   |          |             |         :         |             |             |
   |          |             | Update (7, data)  |             |             |
11 |          |             |<================================|             |
   |          |             |         :         |             |             |
   |          | el.data     |         :         |             |             |
12 |          |<------------|         :         |             |             |
   |          |             |         :         |             |             |
```

1. The cell registers a function: `callback f` records token 12 as cell 2's,
   and returns it.
2. The token goes in the display's data, as the library likes: here,
   `{"click": 12, ...}`. x-ocaml does not know it is there.
3. It reaches the page inside the data,
4. and the element keeps it.
5. Later, the reader clicks.
6. The element dispatches `x-ocaml-callback`, with `{ token: 12, payload }`.
   The payload is whatever the element likes: where the click was, say.
7. The event bubbles up to cell 2's display area, which posts it as a
   `Callback`. If a cell is running, it waits its turn.
8. An exception from the function is caught and logged; a token nobody knows
   does nothing.
9. `f payload` runs as part of cell 2: `X_ocaml_lib.id` is set to cell 2 while
   it runs, so anything it shows goes under cell 2.
10-12. Here it updates the display, and the element redraws.

## Controls in an HTML display

```
                         main thread  :  web worker
reader     element    x-ocaml page         worker loop   X_ocaml_lib    cell code
   |          |             |         :         |             |             |
   |          |             |         :         |             | callback say|
 1 |          |             |         :         |             |<------------|
   |          |             |         :         |             |             |
   |          |             |         :         |             | html        |
 2 |          |             |         :         |             |<------------|
   |          |             |         :         |             |             |
   |          |             | Display (3, 8, "x-ocaml-html")  |             |
 3 |          |             |<================================|             |
   |          |             |         :         |             |             |
   |          | innerHTML   |         :         |             |             |
 4 |          |<------------|         :         |             |             |
   |          |             |         :         |             |             |
   | input    |             |         :         |             |             |
 5 |--------->|             |         :         |             |             |
   |          |             |         :         |             |             |
 6 |          | * closest [data-callback]       |             |             |
   |          |             |         :         |             |             |
   |          | x-ocaml-callback      :         |             |             |
 7 |          |------------>|         :         |             |             |
   |          |             |         :         |             |             |
   |          |             | Callback (5, "70")|             |             |
 8 |          |             |==================>|             |             |
   |          |             |         :         |             |             |
   |          |             |         :         | invoke      |             |
 9 |          |             |         :         |------------>|             |
   |          |             |         :         |             |             |
   |          |             |         :         |             | say "70"    |
10 |          |             |         :         |             |------------>|
   |          |             |         :         |             |             |
```

1-2. The token goes in a `data-callback` attribute:
     `html {|<input type="range" data-callback="5">|}`.
5-7. `<x-ocaml-html>` listens for clicks and input. The nearest element with
     `data-callback` gives the token; the payload is an input's value (`"true"`
     or `"false"` for a checkbox), or else the element's `data-payload`.
     From there on it is as for any other element.

An update replaces the whole HTML, and with it a control the reader is using,
so controls go in a display of their own that is not updated.

## The cell runs again

```
                         main thread  :  web worker
           element    x-ocaml page         worker loop   X_ocaml_lib    cell code
              |             |         :         |             |             |
              |             | Eval (2, code)    |             |             |
 1            |             |==================>|             |             |
              |             |         :         |             |             |
              |             |         :         | forget_cell 2             |
 2            |             |         :         |------------>|             |
              |             |         :         |             |             |
 3            |             |         :   forget its tokens * |             |
              |             |         :         |             |             |
              |             | Remove 7          |             |             |
 4            |             |<================================|             |
              |             |         :         |             |             |
              |             |         :         |             | on_remove ()|
 5            |             |         :         |             |------------>|
              |             |         :         |             |             |
              | remove      |         :         |             |             |
 6            |<------------|         :         |             |             |
              |             |         :         |             |             |
              |             |         :         | run the phrases           |
 7            |             |         :         |-------------------------->|
              |             |         :         |             |             |
```

2. Before the new code runs, the cell's last run is forgotten:
3. its tokens, so a `Callback` with one does nothing, even from an element that
   has not gone yet;
4-6. and its displays: for each, a `Remove`, and its `on_remove`, so code
     drawing it, an animation say, knows to stop. The page takes the element
     away, and the display area when it is empty.
7. Then the new code runs, and shows what it shows.

## Writing an element

A library's page half is a script that defines a custom element. Here is a
whole one, `counter.js`: a button that shows how often it has been clicked,
and calls back when it is.

```js
// <x-counter>: a button that shows how often it was clicked, and calls back.
// data: {"count": number, "click": token}
customElements.define('x-counter', class extends HTMLElement {
  connectedCallback() {
    this.button = document.createElement('button');
    this.button.onclick = () =>
      this.dispatchEvent(new CustomEvent('x-ocaml-callback', {
        bubbles: true,
        detail: { token: this.d.click, payload: '' },
      }));
    this.append(this.button);
  }
  set data(s) {
    this.d = JSON.parse(s);
    this.button.textContent = `clicked ${this.d.count} times`;
  }
});
```

And the OCaml that loads it, shows one, and counts the clicks:

```ocaml
let () = X_ocaml_lib.require ~tag:"x-counter" ~src:"counter.js"

let count = ref 0
let redraw = ref (fun () -> ())

let click =
  X_ocaml_lib.callback (fun _ ->
      incr count;
      !redraw ())

let data () = Printf.sprintf {|{"count":%d,"click":%d}|} !count click
let counter = X_ocaml_lib.display ~tag:"x-counter" (data ())
let () = redraw := fun () -> X_ocaml_lib.update counter (data ())
```

What x-ocaml does with such an element, and what it expects of it:

- **The script** is added to the page as a classic `<script>`, not a module,
  from `src` resolved against the worker's URL. It is added once for each
  tag, and not at all if the tag is already defined. It should define the
  element with that tag: x-ocaml waits for `customElements.whenDefined`.
- **The tag** must be a valid custom element name, with a hyphen in it.
  `x-ocaml-html` is x-ocaml's own.
- **The element** is made in the cell's display area, in the page's own DOM,
  so the page's CSS applies to it.
- **`data`** is set only once the element is connected and defined, so
  `connectedCallback` has run by then. It is set again at each update: the
  setter must cope with being called many times. If updates come before the
  element is defined, it is given only the latest.
- **Callbacks** are `x-ocaml-callback` events that bubble (`bubbles: true`)
  up to the display area. One dispatched from inside the element's own shadow
  root must also be `composed: true` to leave it. The `detail` is
  `{ token, payload }`: the token must be an integer, or nothing is called;
  a payload that is not a string is sent as its JSON, and a missing one as
  `""`.
- **A callback gets no reply.** The display area posts it to the worker, and
  nothing comes back: the element cannot tell whether its token was known,
  whether the function raised (the worker says so in the console), or whether
  it is still waiting behind a cell that is running. Callbacks run one at a
  time, in the order they were sent, and none is merged or dropped: a slider
  that calls back on every `input` event, into a slow function, falls behind
  the reader. An element that must keep up can treat the update that answers
  a callback as its acknowledgement, and call back again only once it has
  come.
- **The token** is whatever the library's OCaml put in the data; x-ocaml does
  not look inside the data, and does not know which element has which token.
- **Removal**: when the cell runs again, or its code calls `remove`, the
  element is taken off the page. `disconnectedCallback` is where to stop
  anything it has running.

## The messages

All in `protocol/x_protocol.ml`.

| Message                        | Direction      | Sent by                             | Handled by                           |
|--------------------------------|----------------|-------------------------------------|--------------------------------------|
| `Display (cell, id, tag, data)`| worker to page | `X_ocaml_lib.display`, `html`       | `Displays.show`                      |
| `Update (id, data)`            | worker to page | `X_ocaml_lib.update`                | `Displays.update`                    |
| `Remove id`                    | worker to page | `X_ocaml_lib.remove`, `forget_cell` | `Displays.remove`                    |
| `Require (tag, src)`           | worker to page | `X_ocaml_lib.require`, once per tag | `Displays.require`                   |
| `Callback (token, payload)`    | page to worker | the display area's listener         | `X_ocaml_lib.invoke`, in the worker  |
