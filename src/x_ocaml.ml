let all : Cell.t list ref = ref []
let find_by_id id = List.find (fun t -> Cell.id t = id) !all

let current_script =
  Brr.El.of_jv (Jv.get (Brr.Document.to_jv Brr.G.document) "currentScript")

let current_attribute attr = Brr.El.at (Jstr.of_string attr) current_script

let extra_load =
  match current_attribute "src-load" with
  | None -> None
  | Some url -> Some (Jstr.to_string url)

let backend_name =
  match current_attribute "backend" with
  | None -> "builtin"
  | Some name -> Jstr.to_string name

let worker_url =
  match current_attribute "src-worker" with
  | None ->
      if backend_name = "builtin" then
        failwith "x-ocaml script missing src-worker attribute"
      else ""
  | Some url -> Jstr.to_string url

let backend = Backend.make ~backend:backend_name ?extra_load worker_url

let () =
  Backend.on_message backend @@ function
  | Formatted_source (id, code_fmt) -> Cell.set_source (find_by_id id) code_fmt
  | Top_response_at (id, loc, msg) -> Cell.add_message (find_by_id id) loc msg
  | Top_response (id, msg) -> Cell.completed_run (find_by_id id) msg
  | Merlin_response (id, msg) -> Cell.receive_merlin (find_by_id id) msg

let () = Backend.post backend Setup

let () =
  match current_attribute "x-ocamlformat" with
  | None -> ()
  | Some conf -> Backend.post backend (Format_config (Jstr.to_string conf))

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.of_string "x-ocaml"
  | Some name -> name

let extra_style = current_attribute "src-style"
let inline_style = current_attribute "inline-style"
let run_on = current_attribute "run-on" |> Option.map Jstr.to_string
let run_on_of_string = function "click" -> `Click | "load" | _ -> `Load

let _ =
  Webcomponent.define elt_name @@ fun this ->
  let prev = match !all with [] -> None | e :: _ -> Some e in
  let run_on =
    run_on_of_string
    @@
    match Webcomponent.get_attribute this "run-on" with
    | Some s -> s
    | None -> Option.value ~default:"load" run_on
  in
  let id = List.length !all in
  let eval_fn ~id ~line_number code = Backend.eval ~id ~line_number backend code in
  let fmt_fn ~id code = Backend.fmt ~id backend code in
  let post_fn msg = Backend.post backend msg in
  let editor = Cell.init ~id ~run_on ?extra_style ?inline_style ~eval_fn ~fmt_fn ~post_fn this in
  all := editor :: !all;
  Cell.set_prev ~prev editor;
  Cell.start editor this;
  if List.for_all Cell.loadable !all then Cell.run editor;
  ()
