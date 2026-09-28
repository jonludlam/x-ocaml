let id = ref (0, 0)
let post r = Js_of_ocaml.Worker.post_message (X_protocol.resp_to_bytes r)

let output_html m =
  let id, loc = !id in
  post (X_protocol.Top_response_at (id, loc, [ Html m ]))

(* Displays and callbacks belong to the cell whose code made them, and go when
   it runs again. *)

type display = int

let counter = ref 0

let fresh () =
  incr counter;
  !counter

let cell () = fst !id

(* display -> its cell, and what to call when the cell runs again *)
let displays : (display, int * (unit -> unit)) Hashtbl.t = Hashtbl.create 8

(* token -> its cell, and the function, given the payload and the JS value
   the element transferred with it, or undefined *)
let callbacks :
    (int, int * (string -> Js_of_ocaml.Js.Unsafe.any -> unit)) Hashtbl.t =
  Hashtbl.create 8

let display ?(on_remove = ignore) ~tag data =
  let d = fresh () in
  Hashtbl.replace displays d (cell (), on_remove);
  post (X_protocol.Display (cell (), d, tag, data));
  d

let html ?on_remove s = display ?on_remove ~tag:"x-ocaml-html" s
let live d = Hashtbl.mem displays d
let update d data = if live d then post (X_protocol.Update (d, data))

let remove d =
  if live d then begin
    Hashtbl.remove displays d;
    post (X_protocol.Remove d)
  end

let callback_js f =
  let k = fresh () in
  Hashtbl.replace callbacks k (cell (), f);
  k

let callback f = callback_js (fun p _ -> f p)
let required = Hashtbl.create 4

let require ~tag ~src =
  if not (Hashtbl.mem required tag) then begin
    Hashtbl.replace required tag ();
    post (X_protocol.Require (tag, src))
  end

(* animation -> its cell *)
let animations : (int, int) Hashtbl.t = Hashtbl.create 4

let error what why =
  let open Js_of_ocaml.Js in
  let console = Unsafe.get Unsafe.global "console" in
  ignore
    (Unsafe.meth_call console "error"
       [| Unsafe.inject (string what); Unsafe.inject (string why) |])

(* Code run for a cell other than the one running, a callback or an
   on_remove, runs as part of its own cell: what it displays is that cell's. *)
let as_cell c f =
  let saved = !id in
  id := (c, snd saved);
  Fun.protect ~finally:(fun () -> id := saved) f

(* The cell runs again: its displays leave the page, and its callbacks are
   forgotten. Then each display's on_remove runs; one that raises is reported,
   and stops neither the others nor the cell's new run. *)
let forget_cell c =
  let gone =
    Hashtbl.fold
      (fun d (c', on_remove) acc ->
        if c' = c then (d, on_remove) :: acc else acc)
      displays []
  in
  Hashtbl.filter_map_inplace
    (fun _ ((c', _) as v) -> if c' = c then None else Some v)
    callbacks;
  Hashtbl.filter_map_inplace
    (fun _ c' -> if c' = c then None else Some c')
    animations;
  List.iter (fun (d, _) -> remove d) gone;
  List.iter
    (fun (_, on_remove) ->
      try as_cell c on_remove
      with e -> error "x-ocaml: an on_remove raised" (Printexc.to_string e))
    gone

let invoke_js k payload v =
  match Hashtbl.find_opt callbacks k with
  | None -> ()
  | Some (c, f) -> as_cell c (fun () -> f payload v)

let invoke k payload =
  invoke_js k payload (Js_of_ocaml.Js.Unsafe.inject Js_of_ocaml.Js.undefined)

(* The worker's own requestAnimationFrame, where there is one: an
   OffscreenCanvas drawn in its callback shows at the page's next frame.
   Elsewhere, a timer. *)
let next_frame f =
  let open Js_of_ocaml.Js in
  let g = Unsafe.global in
  let cb = wrap_callback (fun (t : float) -> f t) in
  if Optdef.test (Unsafe.get g "requestAnimationFrame") then
    ignore
      (Unsafe.fun_call
         (Unsafe.get g "requestAnimationFrame")
         [| Unsafe.inject cb |])
  else
    let now () : float =
      Unsafe.meth_call (Unsafe.get g "performance") "now" [||]
    in
    ignore
      (Unsafe.fun_call
         (Unsafe.get g "setTimeout")
         [|
           Unsafe.inject (wrap_callback (fun () -> f (now ())));
           Unsafe.inject 16;
         |])

let animate f =
  let k = fresh () and c = cell () in
  Hashtbl.replace animations k c;
  let rec tick t =
    if Hashtbl.mem animations k then
      match as_cell c (fun () -> f t) with
      | () -> next_frame tick
      | exception e ->
          Hashtbl.remove animations k;
          error "x-ocaml: an animation raised, and stops" (Printexc.to_string e)
  in
  next_frame tick;
  fun () -> Hashtbl.remove animations k

module Canvas = struct
  open Js_of_ocaml.Js

  type t = Unsafe.any (* the OffscreenCanvasRenderingContext2D itself *)
  type arg = Float of float | Int of int | String of string | Bool of bool

  let js = function
    | Float f -> Unsafe.inject f
    | Int i -> Unsafe.inject i
    | String s -> Unsafe.inject (string s)
    | Bool b -> Unsafe.inject (bool b)

  let call c m args =
    ignore (Unsafe.meth_call c m (Array.of_list (List.map js args)))

  let set c p v = Unsafe.set c (string p) (js v)
  let canvas c = Unsafe.get c "canvas"
  let width c : int = Unsafe.get (canvas c) "width"
  let height c : int = Unsafe.get (canvas c) "height"
  let set_fill_style c s = set c "fillStyle" (String s)
  let set_stroke_style c s = set c "strokeStyle" (String s)
  let set_line_width c w = set c "lineWidth" (Float w)
  let set_font c s = set c "font" (String s)
  let set_global_alpha c a = set c "globalAlpha" (Float a)
  let rect m c x y w h = call c m [ Float x; Float y; Float w; Float h ]
  let fill_rect = rect "fillRect"
  let stroke_rect = rect "strokeRect"
  let clear_rect = rect "clearRect"
  let begin_path c = call c "beginPath" []
  let close_path c = call c "closePath" []
  let move_to c x y = call c "moveTo" [ Float x; Float y ]
  let line_to c x y = call c "lineTo" [ Float x; Float y ]

  let arc c x y r a0 a1 =
    call c "arc" [ Float x; Float y; Float r; Float a0; Float a1 ]

  let fill c = call c "fill" []
  let stroke c = call c "stroke" []
  let fill_text c s x y = call c "fillText" [ String s; Float x; Float y ]
  let save c = call c "save" []
  let restore c = call c "restore" []
  let translate c x y = call c "translate" [ Float x; Float y ]
  let rotate c a = call c "rotate" [ Float a ]
  let scale c x y = call c "scale" [ Float x; Float y ]

  (* The page's <x-ocaml-canvas> makes a canvas, and hands its drawing
     surface to the worker with the callback [canvas] names. *)
  let display ?on_remove ?on_click ~width ~height ready =
    let got =
      callback_js (fun _ offscreen ->
          if not (Unsafe.equals offscreen undefined) then
            ready
              (Unsafe.meth_call offscreen "getContext"
                 [| Unsafe.inject (string "2d") |]))
    in
    let click =
      match on_click with
      | None -> "null"
      | Some f -> string_of_int (callback (fun p -> Scanf.sscanf p "%f %f" f))
    in
    display ?on_remove ~tag:"x-ocaml-canvas"
      (Printf.sprintf {|{"width":%d,"height":%d,"canvas":%d,"click":%s}|} width
         height got click)
end
