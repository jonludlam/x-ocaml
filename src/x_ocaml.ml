let all : Cell.t list ref = ref []
let find_by_id id = List.find (fun t -> Cell.id t = id) !all

let current_script =
  Brr.El.of_jv (Jv.get (Brr.Document.to_jv Brr.G.document) "currentScript")

let current_attribute attr = Brr.El.at (Jstr.of_string attr) current_script

let extra_load =
  match current_attribute "src-load" with
  | None -> None
  | Some url -> Some (Jstr.to_string url)

let worker_url =
  match current_attribute "src-worker" with
  | None -> failwith "x-ocaml script missing src-worker attribute"
  | Some url -> Jstr.to_string url

let worker = Client.make ?extra_load worker_url

let () =
  Client.on_message worker @@ function
  | Formatted_source (id, code_fmt) -> Cell.set_source (find_by_id id) code_fmt
  | Top_response_at (id, loc, msg) -> Cell.add_message (find_by_id id) loc msg
  | Top_response (id, msg) -> Cell.completed_run (find_by_id id) msg
  | Merlin_response (id, msg) -> Cell.receive_merlin (find_by_id id) msg

let warnings_config =
  match current_attribute "warnings" with
  | None -> None
  | Some w -> Some (Jstr.to_string w)

let () = Client.post worker (Setup warnings_config)

let () =
  match current_attribute "x-ocamlformat" with
  | None -> ()
  | Some conf -> Client.post worker (Format_config (Jstr.to_string conf))

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
  let filename = Webcomponent.get_attribute this "filename" in
  let id = List.length !all in
  (* Check for merlin attribute on the individual x-ocaml element (default: true) *)
  let merlin =
    match Webcomponent.get_attribute this "merlin" with
    | Some "false" -> false
    | _ -> true
  in
  let editor = Cell.init ~id ~run_on ?filename ?extra_style ?inline_style ~merlin worker this in
  all := editor :: !all;
  Cell.set_prev ~prev editor;
  if List.for_all Cell.loadable !all then Cell.run editor;
  ()

(* JavaScript API *)
let () =
  (* Set source code for a cell by ID *)
  Jv.set Jv.global "xOcamlSetSource" (Jv.repr (fun id_jv source_jv ->
    let id = Jv.to_int id_jv in
    let source = Jv.to_string source_jv in
    try
      let cell = find_by_id id in
      Cell.set_source cell source
    with Not_found ->
      Brr.Console.error [Jstr.of_string ("x-ocaml cell with id " ^ string_of_int id ^ " not found")]
  ));

  (* Get source code for a cell by ID *)
  Jv.set Jv.global "xOcamlGetSource" (Jv.repr (fun id_jv ->
    let id = Jv.to_int id_jv in
    try
      let cell = find_by_id id in
      Jv.of_string (Cell.get_source cell)
    with Not_found ->
      Brr.Console.error [Jstr.of_string ("x-ocaml cell with id " ^ string_of_int id ^ " not found")];
      Jv.null
  ));

  (* Run a cell by ID *)
  Jv.set Jv.global "xOcamlRun" (Jv.repr (fun id_jv ->
    let id = Jv.to_int id_jv in
    try
      let cell = find_by_id id in
      Cell.run cell
    with Not_found ->
      Brr.Console.error [Jstr.of_string ("x-ocaml cell with id " ^ string_of_int id ^ " not found")]
  ))
