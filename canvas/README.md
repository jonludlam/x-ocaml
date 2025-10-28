# Canvas Widget Prototype for x-ocaml

This is a prototype implementation of a Jupyter-style widget system for x-ocaml, with a focus on canvas widgets that allow OCaml code running in the worker to directly control HTML5 canvas elements.

## Architecture

The implementation uses **OffscreenCanvas** API to transfer canvas control from the main thread to the web worker, enabling the OCaml toplevel to draw directly without thread synchronization.

### Key Components

1. **Protocol Extension** ([protocol/x_protocol.ml](../protocol/x_protocol.ml))
   - Added `widget_id`, `widget_kind`, and `widget_event` types
   - Extended `request` type with `Widget_event`
   - Extended `response` type with `Create_widget`
   - Extended `output` type with `Widget` variant

2. **OffscreenCanvas Bindings** ([src/offscreen_canvas.ml](../src/offscreen_canvas.ml))
   - OCaml bindings for the OffscreenCanvas Web API
   - `transfer_control` function to transfer canvas to worker
   - Dimension and context access methods

3. **Worker-Side Canvas API** ([worker-lib/x_canvas.ml](../worker-lib/x_canvas.ml))
   - `create` - Request canvas creation in frontend
   - `get` - Retrieve canvas handle once initialized
   - `on_event` - Register event handlers for mouse/keyboard events
   - Drawing API - Wrappers around Canvas 2D context methods

4. **Frontend Widget Manager** ([src/widget.ml](../src/widget.ml))
   - Creates DOM canvas elements
   - Transfers control to worker via OffscreenCanvas
   - Sets up event listeners and forwards events to worker
   - Handles coordinate transformations for mouse events

5. **Cell Integration** ([src/cell.ml](../src/cell.ml))
   - Stores widget references per cell
   - Renders widgets as output decorations
   - `create_widget` method called when worker requests widget creation

6. **Worker Message Handler** ([worker/x_worker.ml](../worker/x_worker.ml))
   - Handles `Widget_event` messages
   - Handles special OffscreenCanvas transfer messages (non-marshaled)
   - Dispatches events to registered canvas handlers

## Usage Example

```ocaml
(* Create a canvas widget *)
let canvas_id = X_ocaml_lib.X_canvas.create ~width:400 ~height:300 in

(* Wait for initialization and draw *)
match X_ocaml_lib.X_canvas.get canvas_id with
| None -> print_endline "Canvas not ready yet"
| Some canvas ->
    (* Draw shapes *)
    X_ocaml_lib.X_canvas.set_fill_style canvas "blue";
    X_ocaml_lib.X_canvas.fill_rect canvas ~x:50 ~y:50 ~w:100 ~h:100;

    (* Register event handler *)
    X_ocaml_lib.X_canvas.on_event canvas_id (fun canvas event ->
      match event with
      | X_protocol.Mouse_down { x; y; _ } ->
          X_ocaml_lib.X_canvas.set_fill_style canvas "red";
          X_ocaml_lib.X_canvas.begin_path canvas;
          X_ocaml_lib.X_canvas.arc canvas ~x ~y ~r:10 ~start:0.0 ~end_:(2.0 *. 3.14159);
          X_ocaml_lib.X_canvas.fill canvas
      | _ -> ()
    )
```

## Testing

Open [test-canvas-widget.html](../test-canvas-widget.html) in a browser with a local HTTP server:

```bash
python3 -m http.server 8000
# Then visit http://localhost:8000/test-canvas-widget.html
```

## Technical Details

### Two-Phase Canvas Creation

1. **Worker Request**: OCaml calls `X_canvas.create` which sends `Create_widget` message
2. **Frontend Creation**: Frontend creates `<canvas>`, transfers control via `transferControlToOffscreen()`
3. **Worker Registration**: Frontend sends OffscreenCanvas back to worker as transferable object
4. **Ready for Use**: Worker registers the canvas and makes it available via `X_canvas.get`

### Event Flow

1. User interacts with canvas in DOM
2. Frontend captures events (mousedown, mousemove, mouseup)
3. Events are marshaled and sent to worker as `Widget_event` messages
4. Worker dispatches to registered event handler
5. OCaml code can draw in response to events

### Message Types

- **Marshaled**: Regular protocol messages (Create_widget, Widget_event)
- **Transferable**: OffscreenCanvas sent via postMessage with transfer array (zero-copy)

## Limitations & Future Work

- Canvas initialization is asynchronous (two-phase)
- Limited to mouse events (could add keyboard, touch, etc.)
- No animation frame support yet (could add requestAnimationFrame)
- Only 2D context (could add WebGL)
- Could extend to other widget types (buttons, sliders, text inputs)

## Browser Compatibility

Requires OffscreenCanvas support:
- Chrome 69+
- Firefox 105+
- Safari 16.4+
- Edge 79+
