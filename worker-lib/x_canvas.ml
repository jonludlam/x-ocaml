open Js_of_ocaml

(* Canvas handle for OCaml code *)
(* Note: ctx is now lazy-initialized - it's Jv.null until a context is requested *)
type t = {
  id : int;
  offscreen : Jv.t;
  mutable ctx : Jv.t;
  mutable width : int;
  mutable height : int;
}
[@@warning "-69"]

(* Event types exposed to users *)
type mouse_event = { x : int; y : int; button : int }

type event =
  | Mouse_down of mouse_event
  | Mouse_move of { x : int; y : int }
  | Mouse_up of mouse_event

(* Global widget registry in worker *)
let widgets : (int, t) Hashtbl.t = Hashtbl.create 16

(* Event handler type *)
type event_handler = t -> event -> unit

let event_handlers : (int, event_handler) Hashtbl.t = Hashtbl.create 16

(* Ready callbacks - called when canvas is registered *)
type ready_callback = t -> unit

let ready_callbacks : (int, ready_callback list) Hashtbl.t = Hashtbl.create 16

(* Create new canvas widget *)
let create ~width ~height =
  let cell_id, _ = !X_context.current_id in
  let widget_id = Hashtbl.length widgets in

  (* Request canvas creation in frontend *)
  Worker.post_message
    (X_protocol.resp_to_bytes
       (X_protocol.Create_widget (cell_id, widget_id, Canvas { width; height })));

  (* Return the widget ID immediately *)
  widget_id

(* Called when OffscreenCanvas is transferred to worker *)
let register_offscreen widget_id offscreen =
  let width = Jv.Int.get offscreen "width" in
  let height = Jv.Int.get offscreen "height" in
  (* Don't get a context yet - let the user choose (2d, webgl, webgl2, etc.) *)
  let ctx = Jv.null in
  let canvas = { id = widget_id; offscreen; ctx; width; height } in
  Hashtbl.add widgets widget_id canvas;

  (* Call ready callbacks *)
  (match Hashtbl.find_opt ready_callbacks widget_id with
  | Some callbacks ->
      List.iter (fun cb -> cb canvas) callbacks;
      Hashtbl.remove ready_callbacks widget_id
  | None -> ());

  canvas

(* Get canvas by ID *)
let get widget_id = Hashtbl.find_opt widgets widget_id

(* Call callback when canvas is ready *)
let when_ready widget_id callback =
  match get widget_id with
  | Some canvas ->
      (* Already ready, call immediately *)
      callback canvas
  | None ->
      (* Not ready yet, register callback *)
      let existing =
        Hashtbl.find_opt ready_callbacks widget_id |> Option.value ~default:[]
      in
      Hashtbl.replace ready_callbacks widget_id (callback :: existing)

(* Register event handler *)
let on_event widget_id handler =
  Hashtbl.replace event_handlers widget_id handler

(* Convert X_protocol event to our event type *)
let convert_event (protocol_event : X_protocol.widget_event) : event =
  match protocol_event with
  | X_protocol.Mouse_down { x; y; button } -> Mouse_down { x; y; button }
  | X_protocol.Mouse_move { x; y } -> Mouse_move { x; y }
  | X_protocol.Mouse_up { x; y; button } -> Mouse_up { x; y; button }

(* Dispatch event to handler *)
let dispatch_event widget_id protocol_event =
  match (get widget_id, Hashtbl.find_opt event_handlers widget_id) with
  | Some canvas, Some handler ->
      let event = convert_event protocol_event in
      handler canvas event
  | _ -> ()

(* Get raw 2D context for advanced usage, creating it if needed *)
let get_context t =
  if Jv.is_null t.ctx then (
    let ctx = Jv.call t.offscreen "getContext" [| Jv.of_string "2d" |] in
    if Jv.is_null ctx then failwith "Failed to get 2D context";
    t.ctx <- ctx;
    ctx)
  else t.ctx

(* Drawing API - basic 2D context methods *)

let set_fill_style t color =
  let ctx = get_context t in
  Jv.set ctx "fillStyle" (Jv.of_string color)

let set_stroke_style t color =
  let ctx = get_context t in
  Jv.set ctx "strokeStyle" (Jv.of_string color)

let set_line_width t width =
  let ctx = get_context t in
  Jv.set ctx "lineWidth" (Jv.of_float width)

let fill_rect t ~x ~y ~w ~h =
  let ctx = get_context t in
  let _ =
    Jv.call ctx "fillRect"
      [|
        Jv.of_float (float x);
        Jv.of_float (float y);
        Jv.of_float (float w);
        Jv.of_float (float h);
      |]
  in
  ()

let stroke_rect t ~x ~y ~w ~h =
  let ctx = get_context t in
  let _ =
    Jv.call ctx "strokeRect"
      [|
        Jv.of_float (float x);
        Jv.of_float (float y);
        Jv.of_float (float w);
        Jv.of_float (float h);
      |]
  in
  ()

let clear_rect t ~x ~y ~w ~h =
  let ctx = get_context t in
  let _ =
    Jv.call ctx "clearRect"
      [|
        Jv.of_float (float x);
        Jv.of_float (float y);
        Jv.of_float (float w);
        Jv.of_float (float h);
      |]
  in
  ()

let begin_path t =
  let ctx = get_context t in
  let _ = Jv.call ctx "beginPath" [||] in
  ()

let move_to t ~x ~y =
  let ctx = get_context t in
  let _ =
    Jv.call ctx "moveTo" [| Jv.of_float (float x); Jv.of_float (float y) |]
  in
  ()

let line_to t ~x ~y =
  let ctx = get_context t in
  let _ =
    Jv.call ctx "lineTo" [| Jv.of_float (float x); Jv.of_float (float y) |]
  in
  ()

let arc t ~x ~y ~r ~start ~end_ =
  let ctx = get_context t in
  let _ =
    Jv.call ctx "arc"
      [|
        Jv.of_float (float x);
        Jv.of_float (float y);
        Jv.of_float (float r);
        Jv.of_float start;
        Jv.of_float end_;
      |]
  in
  ()

let fill t =
  let ctx = get_context t in
  let _ = Jv.call ctx "fill" [||] in
  ()

let stroke t =
  let ctx = get_context t in
  let _ = Jv.call ctx "stroke" [||] in
  ()

(* Get raw OffscreenCanvas for WebGL or advanced usage *)
let get_offscreen t = t.offscreen
