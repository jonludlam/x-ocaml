let all : Cell.t list ref = ref []
let id_map : (string, Cell.t) Hashtbl.t = Hashtbl.create 16
let dependents : (string, Cell.t list) Hashtbl.t = Hashtbl.create 16
(* Maps impl cell string ID -> spec cell *)
let spec_for : (string, Cell.t) Hashtbl.t = Hashtbl.create 16
(* Maps impl cell string ID -> list of test cells *)
let test_for : (string, Cell.t list) Hashtbl.t = Hashtbl.create 16
(* Maps test cell id -> impl cell string_id it's testing *)
let impl_id_for_test : (int, string) Hashtbl.t = Hashtbl.create 16
(* Accumulates Top_response_at messages for test cells *)
let test_output_accum : (int, X_protocol.output list) Hashtbl.t = Hashtbl.create 16
(* Maps spec cell id -> impl cell id for routing spec check responses *)
let impl_id_for_spec : (int, int) Hashtbl.t = Hashtbl.create 16
(* Accumulates Top_response_at messages for spec checks *)
let spec_output_accum : (int, X_protocol.output list) Hashtbl.t = Hashtbl.create 16
(* Maps impl cell id -> tests to run after impl completes (for re-run before tests) *)
let pending_tests_for_impl : (int, Cell.t list) Hashtbl.t = Hashtbl.create 16
let find_by_id id = List.find (fun t -> Cell.id t = id) !all
let find_by_string_id str_id = Hashtbl.find_opt id_map str_id

(* Transform a single "val name : type" line to "let _ : type = name" *)
let transform_val_line line =
  let line = String.trim line in
  if String.length line > 4 && String.sub line 0 4 = "val " then
    let rest = String.sub line 4 (String.length line - 4) in
    match String.index_opt rest ':' with
    | Some colon_pos ->
        let name = String.trim (String.sub rest 0 colon_pos) in
        let typ = String.trim (String.sub rest (colon_pos + 1) (String.length rest - colon_pos - 1)) in
        Printf.sprintf "let _ : %s = %s" typ name
    | None -> line
  else
    line

(* Transform spec with multiple "val" lines, each on its own line *)
let transform_val_spec spec =
  let lines = String.split_on_char '\n' spec in
  let transformed = List.map transform_val_line lines in
  (* Filter out empty lines and join with ";;" to separate declarations *)
  let non_empty = List.filter (fun s -> String.trim s <> "") transformed in
  String.concat ";;\n" non_empty

let get_tests_for_cell cell =
  match Cell.string_id cell with
  | Some str_id -> (
      match Hashtbl.find_opt test_for str_id with
      | Some tests -> tests
      | None -> [])
  | None -> []

let add_test_for ~target_id ~test_cell =
  let current = try Hashtbl.find test_for target_id with Not_found -> [] in
  Hashtbl.replace test_for target_id (test_cell :: current)

let add_dependent ~source_id ~dependent_cell =
  let current = try Hashtbl.find dependents source_id with Not_found -> [] in
  Hashtbl.replace dependents source_id (dependent_cell :: current)

let run_dependents source_id =
  match Hashtbl.find_opt dependents source_id with
  | None -> ()
  | Some cells -> List.iter Cell.run cells

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
  | Top_response_at (id, loc, msg) ->
      Brr.Console.log [Jstr.of_string (Printf.sprintf "Top_response_at for id %d, loc %d, msg count: %d" id loc (List.length msg))];
      (* Check if this is a test cell - accumulate output instead of showing inline *)
      (match Hashtbl.find_opt impl_id_for_test id with
      | Some impl_str_id ->
          Brr.Console.log [Jstr.of_string (Printf.sprintf "  -> test cell for impl '%s', accumulating" impl_str_id)];
          (* Accumulate for later use in Top_response *)
          let current = try Hashtbl.find test_output_accum id with Not_found -> [] in
          Hashtbl.replace test_output_accum id (current @ msg)
      | None ->
          (* Check if this is a spec check - accumulate for later *)
          (match Hashtbl.find_opt impl_id_for_spec id with
          | Some impl_id ->
              Brr.Console.log [Jstr.of_string (Printf.sprintf "  -> spec check for impl %d, accumulating" impl_id)];
              (* Accumulate spec output - don't show inline *)
              let current = try Hashtbl.find spec_output_accum id with Not_found -> [] in
              Hashtbl.replace spec_output_accum id (current @ msg)
          | None ->
              Brr.Console.log [Jstr.of_string (Printf.sprintf "  -> normal cell, showing inline")];
              (* Normal cell - show inline *)
              Cell.add_message (find_by_id id) loc msg))
  | Top_response (id, msg) ->
      Brr.Console.log [Jstr.of_string (Printf.sprintf "Top_response for id %d, msg count: %d" id (List.length msg))];
      (* Check if this is a test cell response *)
      (match Hashtbl.find_opt impl_id_for_test id with
      | Some impl_str_id ->
          (* This is a test cell - route output to impl cell *)
          let cell = find_by_id id in
          (* Combine accumulated Top_response_at outputs with final msg *)
          let accumulated = try Hashtbl.find test_output_accum id with Not_found -> [] in
          Hashtbl.remove test_output_accum id;  (* Clear accumulator *)
          let all_output = accumulated @ msg in
          Brr.Console.log [Jstr.of_string (Printf.sprintf "Test cell %d completed, routing to impl '%s' (accumulated: %d, final: %d, total: %d)"
            id impl_str_id (List.length accumulated) (List.length msg) (List.length all_output))];
          Cell.completed_run cell msg;
          (match find_by_string_id impl_str_id with
          | Some impl_cell ->
              Brr.Console.log [Jstr.of_string (Printf.sprintf "Found impl cell, adding test output")];
              Cell.add_test_output impl_cell all_output
          | None ->
              Brr.Console.log [Jstr.of_string (Printf.sprintf "ERROR: Could not find impl cell '%s'" impl_str_id)])
      | None ->
          (* Check if this is a spec check response *)
          (match Hashtbl.find_opt impl_id_for_spec id with
          | Some impl_id ->
              (* This is a spec check response - route to impl cell *)
              Hashtbl.remove impl_id_for_spec id;
              (* Combine accumulated Top_response_at outputs with final msg *)
              let accumulated = try Hashtbl.find spec_output_accum id with Not_found -> [] in
              Hashtbl.remove spec_output_accum id;  (* Clear accumulator *)
              let all_output = accumulated @ msg in
              let impl_cell = find_by_id impl_id in
              Brr.Console.log [Jstr.of_string (Printf.sprintf "Spec check completed for impl cell %d (spec_id=%d)" impl_id id)];
              let tests = get_tests_for_cell impl_cell in
              Brr.Console.log [Jstr.of_string (Printf.sprintf "Found %d tests for cell" (List.length tests))];
              let has_tests = tests <> [] in
              Cell.clear_spec_results impl_cell;
              Cell.set_spec_output impl_cell all_output ~has_tests;
              Brr.Console.log [Jstr.of_string (Printf.sprintf "After set_spec_output: spec_passed=%b" (Cell.spec_passed impl_cell))];
              Cell.completed_type_check impl_cell;
              (* If spec passed and has tests, re-run impl cell then run tests *)
              if Cell.spec_passed impl_cell && has_tests then begin
                Brr.Console.log [Jstr.of_string (Printf.sprintf "Spec passed, re-running impl cell before %d tests" (List.length tests))];
                Cell.start_tests impl_cell (List.length tests);
                (* Store tests to run after impl completes *)
                Hashtbl.replace pending_tests_for_impl impl_id tests;
                (* Re-run the impl cell to ensure toplevel has the definitions *)
                Cell.run_directly impl_cell
              end else begin
                Brr.Console.log [Jstr.of_string (Printf.sprintf "Spec passed=%b, has_tests=%b - not running tests" (Cell.spec_passed impl_cell) has_tests)]
              end
          | None ->
              (* Normal cell response *)
              let cell = find_by_id id in
              Cell.completed_run cell msg;
              (* Check if there are pending tests to run after this impl cell *)
              (match Hashtbl.find_opt pending_tests_for_impl id with
              | Some tests ->
                  Brr.Console.log [Jstr.of_string (Printf.sprintf "Impl cell %d completed, running %d pending tests" id (List.length tests))];
                  Hashtbl.remove pending_tests_for_impl id;
                  List.iter Cell.run_directly tests
              | None -> ());
              (* If cell completed successfully and has a spec, trigger type check (but only once) *)
              Brr.Console.log [Jstr.of_string (Printf.sprintf "After completed_run: is_run_ok=%b, is_type_check_pending=%b, is_type_check_done=%b"
                (Cell.is_run_ok cell) (Cell.is_type_check_pending cell) (Cell.is_type_check_done cell))];
              if Cell.is_run_ok cell && not (Cell.is_type_check_pending cell) && not (Cell.is_type_check_done cell) then
                (match Cell.string_id cell with
                | Some str_id ->
                    (match Hashtbl.find_opt spec_for str_id with
                    | Some spec_cell ->
                        let raw_source = Cell.get_source spec_cell in
                        let spec_content = transform_val_spec raw_source in
                        let spec_id = Cell.id spec_cell in
                        Brr.Console.log [Jstr.of_string (Printf.sprintf "Spec raw source: [%s]" raw_source)];
                        Brr.Console.log [Jstr.of_string (Printf.sprintf "Spec transformed (sending to toplevel): [%s]" spec_content)];
                        Cell.set_type_check_pending cell;
                        (* Track spec_id -> impl_id for routing the response *)
                        Hashtbl.replace impl_id_for_spec spec_id id;
                        (* Use spec cell's ID so Environment.reset uses impl's captured env *)
                        Client.eval ~id:spec_id ~line_number:1 ?filename:(Cell.filename cell) worker spec_content
                    | None -> Brr.Console.log [Jstr.of_string (Printf.sprintf "No spec found for cell %d" id)])
                | None -> ())))
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
    | Some attr -> parse_highlight_ranges attr
    | None -> []
  in
  (* Get string ID for this cell *)
  let cell_id = Webcomponent.get_attribute this "id" in
  (* Create on_change callback that runs dependents *)
  let on_change =
    match cell_id with
    | Some str_id -> Some (fun () -> run_dependents str_id)
    | None -> None
  in
  let editor = Cell.init ~id ~run_on ?filename ?extra_style ?inline_style ~merlin ~highlight ?on_change worker this in
  all := editor :: !all;
  (* Set and register string ID if provided *)
  Cell.set_string_id editor cell_id;
  (match cell_id with
  | Some str_id -> Hashtbl.add id_map str_id editor
  | None -> ());
  (* Register as spec if spec-for is specified *)
  (match Webcomponent.get_attribute this "spec-for" with
  | Some target_id -> Hashtbl.replace spec_for target_id editor
  | None -> ());
  (* Register as test if test-for is specified *)
  (match Webcomponent.get_attribute this "test-for" with
  | Some target_id ->
      add_test_for ~target_id ~test_cell:editor;
      Hashtbl.add impl_id_for_test id target_id
  | None -> ());
  (* Register as dependent if auto-run-on is specified *)
  (match Webcomponent.get_attribute this "auto-run-on" with
  | Some source_id -> add_dependent ~source_id ~dependent_cell:editor
  | None -> ());
  Cell.set_prev ~prev editor;
  if Cell.loadable editor then Cell.run editor;
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
        ( "clearOutput",
          Jv.callback ~arity:1 (fun id ->
              match find_cell id with
              | Some cell -> Cell.clear_output cell
              | None ->
                  Brr.Console.error [Jstr.of_string "x-ocaml cell not found"]) );
      |]
  in
  Jv.set Jv.global "xOcaml" api