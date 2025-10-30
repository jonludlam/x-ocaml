open Brr

type kind = Canvas of { canvas : El.t; offscreen : Offscreen_canvas.t }
type t = { id : int; cell_id : int; kind : kind; container : El.t }

(* Calculate canvas-relative coordinates from mouse event *)
let get_relative_coords canvas_el ev =
  let ev_mouse = Ev.as_type ev in
  let rect = Jv.call (El.to_jv canvas_el) "getBoundingClientRect" [||] in
  let rect_x = Jv.Float.get rect "left" in
  let rect_y = Jv.Float.get rect "top" in
  let client_x = int_of_float (Ev.Mouse.client_x ev_mouse) in
  let client_y = int_of_float (Ev.Mouse.client_y ev_mouse) in
  (client_x - int_of_float rect_x, client_y - int_of_float rect_y)

(* Set up event listeners on canvas element *)
let setup_event_listeners canvas_el widget_id cell_id worker =
  let send_event event =
    Client.post worker (Widget_event (cell_id, widget_id, event))
  in

  (* Mouse down *)
  let _ =
    Ev.listen Ev.mousedown
      (fun ev ->
        let x, y = get_relative_coords canvas_el ev in
        let ev_mouse = Ev.as_type ev in
        let button = Ev.Mouse.button ev_mouse in
        send_event (X_protocol.Mouse_down { x; y; button }))
      (El.as_target canvas_el)
  in

  (* Mouse move *)
  let _ =
    Ev.listen Ev.mousemove
      (fun ev ->
        let x, y = get_relative_coords canvas_el ev in
        send_event (X_protocol.Mouse_move { x; y }))
      (El.as_target canvas_el)
  in

  (* Mouse up *)
  let _ =
    Ev.listen Ev.mouseup
      (fun ev ->
        let x, y = get_relative_coords canvas_el ev in
        let ev_mouse = Ev.as_type ev in
        let button = Ev.Mouse.button ev_mouse in
        send_event (X_protocol.Mouse_up { x; y; button }))
      (El.as_target canvas_el)
  in

  ()

(* Create widget in DOM *)
let create ~cell_id ~widget_id worker = function
  | X_protocol.Canvas { width; height } ->
      (* Create canvas element *)
      let canvas_el =
        El.canvas
          ~at:[ At.int (Jstr.v "width") width; At.int (Jstr.v "height") height ]
          []
      in
      let canvas = Brr_canvas.Canvas.of_el canvas_el in

      (* Transfer control to worker *)
      let offscreen = Offscreen_canvas.transfer_control canvas in

      (* Send OffscreenCanvas to worker *)
      (* Note: We need to send this as a raw message, not marshaled *)
      let msg =
        Jv.obj
          [|
            ("type", Jv.of_string "offscreen_canvas");
            ("widget_id", Jv.of_int widget_id);
            ("canvas", Offscreen_canvas.to_jv offscreen);
          |]
      in
      (* Send the message with the OffscreenCanvas as a transferable *)
      Client.post_with_transfer worker msg [ Offscreen_canvas.to_jv offscreen ];

      (* Create container with event listeners *)
      let container = El.div [ canvas_el ] in
      setup_event_listeners canvas_el widget_id cell_id worker;

      {
        id = widget_id;
        cell_id;
        kind = Canvas { canvas = canvas_el; offscreen };
        container;
      }

(* Get the container element for rendering *)
let to_el t = t.container
