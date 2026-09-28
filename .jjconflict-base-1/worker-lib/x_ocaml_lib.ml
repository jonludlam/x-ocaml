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

(* token -> its cell, and the function *)
let callbacks : (int, int * (string -> unit)) Hashtbl.t = Hashtbl.create 8

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

let callback f =
  let k = fresh () in
  Hashtbl.replace callbacks k (cell (), f);
  k

let required = Hashtbl.create 4

let require ~tag ~src =
  if not (Hashtbl.mem required tag) then begin
    Hashtbl.replace required tag ();
    post (X_protocol.Require (tag, src))
  end

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
  List.iter (fun (d, _) -> remove d) gone;
  List.iter
    (fun (_, on_remove) ->
      try as_cell c on_remove
      with e -> error "x-ocaml: an on_remove raised" (Printexc.to_string e))
    gone

let invoke k payload =
  match Hashtbl.find_opt callbacks k with
  | None -> ()
  | Some (c, f) -> as_cell c (fun () -> f payload)
