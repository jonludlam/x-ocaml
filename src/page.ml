type t = {
  worker : Client.t;
  extra_style : Jstr.t option;
  inline_style : Jstr.t option;
  default_run_on : string;
  mutable cells : Cell.t list; (* most recent first *)
  mutable named : (string * Cell.t) list; (* by data-id *)
  mutable tests : (Cell.t * Cell.t list) list; (* exercise, its tests *)
  setup : X_protocol.request list; (* what every new worker is sent first *)
  displays : Displays.t;
  mutable groups : (string * Cell.group) list;
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
      setup =
        Setup
        :: Option.to_list
             (Option.map (fun c -> X_protocol.Format_config c) format_config);
      displays = Displays.create ~post:(Client.post worker) ~worker_url ();
      groups = [];
    }
  in
  Client.on_message worker (route t);
  List.iter (Client.post worker) t.setup;
  t

(* Stopping a cell is the only way to interrupt the toplevel: the worker is
   replaced, so every cell's definitions are gone and all must run again. *)
let stop_all t =
  Displays.clear_all t.displays;
  Client.restart t.worker;
  List.iter (Client.post t.worker) t.setup;
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
  Displays.add_cell t.displays (List.length t.cells)
    (Webcomponent.as_target this);
  (* A cell whose fold or highlight attribute names its lines runs as
     written: reformatting would move the lines. *)
  let verbatim =
    attr "fold" <> None || attr "highlight" <> None || attr "group" <> None
  in
  (* A cell of a group: the page is parsed by now, so the group's members
     are all there to count, and this cell's place among them to find. *)
  let group =
    Option.map
      (fun name ->
        let els =
          List.rev
            (Brr.El.fold_find_by_selector List.cons
               (Jstr.v ("[group=\"" ^ name ^ "\"]"))
               [])
        in
        let me = Brr.El.to_jv (Webcomponent.as_target this) in
        let rec index i = function
          | [] -> 0
          | e :: rest -> if Jv.strict_equal (Brr.El.to_jv e) me then i else index (i + 1) rest
        in
        let g =
          match List.assoc_opt name t.groups with
          | Some g -> g
          | None ->
              let g = Cell.make_group ~size:(List.length els) in
              t.groups <- (name, g) :: t.groups;
              g
        in
        (g, index 0 els))
      (attr "group")
  in
  let cell =
    Cell.init ~id:(List.length t.cells) ~mode ~run_on
      ~merlin:(attr "data-merlin" <> Some "false")
      ?cell_id:(attr "data-id") ?extra_style:t.extra_style
      ?inline_style:t.inline_style ?group
      ~eval_fn:(fun ~id ~line_number code ->
        Client.eval ~verbatim ~id ~line_number t.worker code)
      ~fmt_fn:(fun ~id code -> if not verbatim then Client.fmt ~id t.worker code)
      ~post_fn:(Client.post t.worker) this
  in
  Cell.set_stop cell (fun () -> stop_all t);
  Cell.set_on_stale cell (Displays.set_stale t.displays (Cell.id cell));
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
