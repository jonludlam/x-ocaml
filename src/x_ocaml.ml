let all : Cell.t list ref = ref []
let id_map : (string, Cell.t) Hashtbl.t = Hashtbl.create 16
let find_by_id id = List.find (fun t -> Cell.id t = id) !all
let find_by_string_id str_id = Hashtbl.find_opt id_map str_id

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
let run_on_of_string = function
  | "click" -> `Click
  | "never" -> `Never
  | "load" | _ -> `Load

let parse_highlight_ranges s : Editor.highlight_spec list =
  let open Editor in
  let parse_item item =
    let item = String.trim item in
    match String.split_on_char ':' item with
    | [ line_spec ] -> (
        (* Just line numbers: "2" or "1-3" *)
        match String.split_on_char '-' line_spec with
        | [ single ] -> (
            match int_of_string_opt single with
            | Some n -> [ Line n ]
            | None -> [])
        | [ start; end_ ] -> (
            match (int_of_string_opt start, int_of_string_opt end_) with
            | Some s, Some e when s <= e ->
                List.init (e - s + 1) (fun i -> Line (s + i))
            | _ -> [])
        | _ -> [])
    | [ line; char_range ] -> (
        (* Line with character range: "6:2-5" *)
        match int_of_string_opt line with
        | None -> []
        | Some ln -> (
            match String.split_on_char '-' char_range with
            | [ start; end_ ] -> (
                match (int_of_string_opt start, int_of_string_opt end_) with
                | Some s, Some e when s <= e -> [ Range (ln, s, e) ]
                | _ -> [])
            | _ -> []))
    | _ -> []
  in
  String.split_on_char ',' s |> List.map parse_item |> List.concat

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
  (* Parse highlight attribute *)
  let highlight =
    match Webcomponent.get_attribute this "highlight" with
    | Some attr -> parse_highlight_ranges (Jstr.to_string attr)
    | None -> []
  in
  let editor = Cell.init ~id ~run_on ?filename ?extra_style ?inline_style ~merlin ~highlight worker this in
  all := editor :: !all;
  (* Register string ID if provided *)
  (match Webcomponent.get_attribute this "id" with
  | Some str_id -> Hashtbl.add id_map (Jstr.to_string str_id) editor
  | None -> ());
  Cell.set_prev ~prev editor;
  if List.for_all Cell.loadable !all then Cell.run editor;
  ()

(* JavaScript API *)
let () =
  let find_cell id_arg =
    let str_id = Jv.to_string id_arg in
    match find_by_string_id str_id with
    | Some cell -> Some cell
    | None -> (
        (* Try parsing as numeric ID *)
        match int_of_string_opt str_id with
        | Some id -> (try Some (find_by_id id) with _ -> None)
        | None -> None)
  in
  let api =
    Jv.obj
      [|
        ( "setHighlight",
          Jv.callback ~arity:2 (fun id spec_str ->
              match find_cell id with
              | Some cell ->
                  let specs = parse_highlight_ranges (Jv.to_string spec_str) in
                  Cell.set_highlight cell specs
              | None -> ()) );
        ( "clearHighlight",
          Jv.callback ~arity:1 (fun id ->
              match find_cell id with
              | Some cell -> Cell.set_highlight cell []
              | None -> ()) );
        ( "getCellId",
          Jv.callback ~arity:1 (fun index ->
              try
                let idx = Jv.to_int index in
                let cell = List.nth !all (List.length !all - 1 - idx) in
                Jv.of_int (Cell.id cell)
              with _ -> Jv.null) );
        ( "getCellCount",
          Jv.callback ~arity:0 (fun () -> Jv.of_int (List.length !all)) );
        ( "setSource",
          Jv.callback ~arity:2 (fun id source_jv ->
              match find_cell id with
              | Some cell -> Cell.set_source cell (Jv.to_string source_jv)
              | None ->
                  Brr.Console.error [Jstr.of_string "x-ocaml cell not found"]) );
        ( "getSource",
          Jv.callback ~arity:1 (fun id ->
              match find_cell id with
              | Some cell -> Jv.of_string (Cell.get_source cell)
              | None ->
                  Brr.Console.error [Jstr.of_string "x-ocaml cell not found"];
                  Jv.null) );
        ( "run",
          Jv.callback ~arity:1 (fun id ->
              match find_cell id with
              | Some cell -> Cell.run cell
              | None ->
                  Brr.Console.error [Jstr.of_string "x-ocaml cell not found"]) );
      |]
  in
  Jv.set Jv.global "xOcaml" api
