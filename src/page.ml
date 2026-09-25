type t = {
  worker : Client.t;
  extra_style : Jstr.t option;
  inline_style : Jstr.t option;
  default_run_on : string;
  mutable cells : Cell.t list; (* most recent first *)
  mutable named : (string * Cell.t) list; (* by data-id *)
  mutable tests : (Cell.t * Cell.t list) list; (* exercise, its tests *)
  displays : Displays.t;
}

let find_by_id t id = List.find (fun c -> Cell.id c = id) t.cells

let route t : X_protocol.response -> unit = function
  | Formatted_source (id, code_fmt) ->
      Cell.set_source (find_by_id t id) code_fmt
  | Top_response_at (id, loc, msg) -> Cell.add_message (find_by_id t id) loc msg
  | Top_response (id, msg) -> Cell.completed_run (find_by_id t id) msg
  | Merlin_response (id, msg) -> Cell.receive_merlin (find_by_id t id) msg
  | Display (cell, display, tag, data) ->
      Displays.show t.displays ~cell ~display ~tag data
  | Update (display, data) -> Displays.update t.displays display data
  | Remove display -> Displays.remove t.displays display
  | Require (tag, src) -> Displays.require t.displays ~tag ~src

let create ~worker ~worker_url ?extra_style ?inline_style
    ?(default_run_on = "load") ?format_config () =
  let t =
    {
      worker;
      extra_style;
      inline_style;
      default_run_on;
      cells = [];
      named = [];
      tests = [];
      displays = Displays.create ~post:(Client.post worker) ~worker_url ();
    }
  in
  Client.on_message worker (route t);
  Client.post worker Setup;
  Option.iter
    (fun conf -> Client.post worker (Format_config conf))
    format_config;
  t

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
  Displays.add_cell t.displays (List.length t.cells)
    (Webcomponent.as_target this);
  let cell =
    Cell.init ~id:(List.length t.cells) ~mode ~run_on
      ~merlin:(attr "data-merlin" <> Some "false")
      ?extra_style:t.extra_style ?inline_style:t.inline_style
      ~eval_fn:(fun ~id ~line_number code ->
        Client.eval ~id ~line_number t.worker code)
      ~fmt_fn:(fun ~id code -> Client.fmt ~id t.worker code)
      ~post_fn:(Client.post t.worker) this
  in
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
