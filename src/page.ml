type t = {
  worker : Backend.t;
  extra_style : Jstr.t option;
  inline_style : Jstr.t option;
  default_run_on : string;
  mutable cells : Cell.t list; (* most recent first *)
  mutable named : (string * Cell.t) list; (* by data-id *)
  mutable tests : (Cell.t * Cell.t list) list; (* exercise, its tests *)
  setup : X_protocol.request list; (* what every new worker is sent first *)
}

(* An answer for a cell the page does not know is dropped, whichever backend
   sent it and whenever. *)
let route t (r : X_protocol.response) =
  let to_cell id f =
    match List.find_opt (fun c -> Cell.id c = id) t.cells with
    | Some c -> f c
    | None -> ()
  in
  match r with
  | Formatted_source (id, code) -> to_cell id (fun c -> Cell.set_source c code)
  | Top_response_at (id, loc, msg) ->
      to_cell id (fun c -> Cell.add_message c loc msg)
  | Top_response (id, msg) -> to_cell id (fun c -> Cell.completed_run c msg)
  | Merlin_response (id, msg) -> to_cell id (fun c -> Cell.receive_merlin c msg)

let create ~worker ?extra_style ?inline_style ?(default_run_on = "load")
    ?format_config () =
  let t =
    {
      worker;
      extra_style;
      inline_style;
      default_run_on;
      cells = [];
      named = [];
      tests = [];
      setup =
        Setup
        :: Option.to_list
             (Option.map (fun c -> X_protocol.Format_config c) format_config);
    }
  in
  Backend.on_message worker (route t);
  List.iter (Backend.post worker) t.setup;
  t

(* Stopping a cell is the only way to interrupt the toplevel: the worker is
   replaced, so every cell's definitions are gone and all must run again. *)
let stop_all t =
  Backend.restart t.worker;
  List.iter (Backend.post t.worker) t.setup;
  List.iter Cell.reset_status t.cells

let run_on_of_string = function "click" -> `Click | "load" | _ -> `Load

let tests_of t exercise =
  Option.value ~default:[] (List.assq_opt exercise t.tests)

(* A test is linked to the exercise its data-for names by data-id, or else
   to the nearest exercise before it. It runs whenever that exercise
   completes, and at once if the exercise already has. *)
let link_test t test ~cell_for =
  let target =
    match cell_for with
    | Some name ->
        let found = List.assoc_opt name t.named in
        (* A test runs where it is on the page, so an exercise after it has
           not run when it does, and could not be linked if it had. *)
        if found = None then
          Brr.Console.warn
            [
              Jstr.v
                ("x-ocaml: data-for=\"" ^ name
               ^ "\" names no cell before this test; it will not rerun");
            ];
        found
    | None -> List.find_opt (fun c -> Cell.mode c = Cell.Exercise) t.cells
  in
  Option.iter
    (fun exercise ->
      t.tests <-
        (exercise, test :: tests_of t exercise)
        :: List.remove_assq exercise t.tests;
      Cell.set_on_completed exercise (fun _ ->
          List.iter Cell.run (List.rev (tests_of t exercise)));
      if Cell.has_completed exercise then Cell.run test)
    target

let register t this =
  let attr = Webcomponent.get_attribute this in
  let prev = match t.cells with [] -> None | c :: _ -> Some c in
  let mode =
    Cell.mode_of_string (Option.value ~default:"interactive" (attr "mode"))
  in
  let run_on =
    run_on_of_string (Option.value ~default:t.default_run_on (attr "run-on"))
  in
  let cell =
    Cell.init ~id:(List.length t.cells) ~mode ~run_on
      ~merlin:(attr "data-merlin" <> Some "false")
      ?cell_id:(attr "data-id") ?extra_style:t.extra_style
      ?inline_style:t.inline_style
      ~eval_fn:(fun ~id ~line_number code ->
        Backend.eval ~id ~line_number t.worker code)
      ~fmt_fn:(fun ~id code -> Backend.fmt ~id t.worker code)
      ~post_fn:(Backend.post t.worker) this
  in
  Cell.set_stop cell (fun () -> stop_all t);
  if mode = Cell.Test then link_test t cell ~cell_for:(attr "data-for");
  Option.iter
    (fun name ->
      (* Tests find the last cell by that name, and saved versions of
         exercises would be shared: surely a mistake. *)
      if List.mem_assoc name t.named then
        Brr.Console.warn
          [
            Jstr.v ("x-ocaml: more than one cell has data-id=\"" ^ name ^ "\"");
          ];
      t.named <- (name, cell) :: t.named)
    (attr "data-id");
  t.cells <- cell :: t.cells;
  Cell.set_prev ~prev cell;
  if List.for_all Cell.loadable t.cells then Cell.run cell;
  cell
