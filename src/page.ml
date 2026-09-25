type t = {
  worker : Client.t;
  extra_style : Jstr.t option;
  inline_style : Jstr.t option;
  default_run_on : string;
  mutable cells : Cell.t list; (* most recent first *)
}

let find_by_id t id = List.find (fun c -> Cell.id c = id) t.cells

let route t : X_protocol.response -> unit = function
  | Formatted_source (id, code_fmt) ->
      Cell.set_source (find_by_id t id) code_fmt
  | Top_response_at (id, loc, msg) -> Cell.add_message (find_by_id t id) loc msg
  | Top_response (id, msg) -> Cell.completed_run (find_by_id t id) msg
  | Merlin_response (id, msg) -> Cell.receive_merlin (find_by_id t id) msg

let create ~worker ?extra_style ?inline_style ?(default_run_on = "load")
    ?format_config () =
  let t = { worker; extra_style; inline_style; default_run_on; cells = [] } in
  Client.on_message worker (route t);
  Client.post worker Setup;
  Option.iter
    (fun conf -> Client.post worker (Format_config conf))
    format_config;
  t

let run_on_of_string = function "click" -> `Click | "load" | _ -> `Load

let register t this =
  let prev = match t.cells with [] -> None | c :: _ -> Some c in
  let run_on =
    run_on_of_string
      (Option.value ~default:t.default_run_on
         (Webcomponent.get_attribute this "run-on"))
  in
  let cell =
    Cell.init ~id:(List.length t.cells) ~run_on ?extra_style:t.extra_style
      ?inline_style:t.inline_style
      ~eval_fn:(fun ~id ~line_number code ->
        Client.eval ~id ~line_number t.worker code)
      ~fmt_fn:(fun ~id code -> Client.fmt ~id t.worker code)
      ~post_fn:(Client.post t.worker) this
  in
  t.cells <- cell :: t.cells;
  Cell.set_prev ~prev cell;
  if List.for_all Cell.loadable t.cells then Cell.run cell;
  cell
