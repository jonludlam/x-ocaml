open Brr

type status = Not_run | Running | Run_ok | Request_run

type t = {
  id : int;
  mutable string_id : string option;
  mutable prev : t option;
  mutable next : t option;
  mutable status : status;
  mutable type_check_pending : bool;
  mutable type_check_done : bool;
  cm : Editor.t;
  worker : Client.t;
  merlin_worker : Merlin_ext.Client.worker;
  run_on : [ `Click | `Load | `Never ];
  filename : string option;
  spec_results : El.t;
  (* Test tracking *)
  mutable tests_pending : int;
  mutable spec_passed : bool;
  mutable spec_output : X_protocol.output list;
  mutable test_outputs : X_protocol.output list list;
}

let id t = t.id
let string_id t = t.string_id
let set_string_id t s = t.string_id <- s
let is_run_ok t = t.status = Run_ok
let set_type_check_pending t = t.type_check_pending <- true
let is_type_check_pending t = t.type_check_pending
let is_type_check_done t = t.type_check_done

let get_source t = Editor.source t.cm
let filename t = t.filename

let pre_source t =
  let target_filename = t.filename in
  let rec go acc current =
    match current.prev with
    | None ->
        let result = String.concat "\n" acc in
        result
    | Some e when e.filename = target_filename ->
        (* Brr.Console.log ["pre_source: found matching cell with filename";
                         Option.value ~default:"<none>" e.filename;
                         "source:"; Editor.source e.cm]; *)
        go (Editor.source e.cm :: acc) e
    | Some e ->
        Brr.Console.log ["pre_source: skipping cell with different filename";
                         Option.value ~default:"<none>" e.filename;
                         "target:"; Option.value ~default:"<none>" target_filename];
        go acc e  (* Skip cells with different filename, keep searching *)
  in
  let s = go [] t in
  if s = "" then s else s ^ " ;;\n"

let rec invalidate_from ~editor =
  editor.status <- Not_run;
  editor.type_check_done <- false;
  Editor.clear editor.cm;
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      invalidate_from ~editor

let invalidate_after ~editor =
  editor.status <- Not_run;
  editor.type_check_done <- false;
  El.set_children editor.spec_results [];  (* Clear spec results on edit *)
  editor.tests_pending <- 0;
  editor.spec_passed <- false;
  editor.spec_output <- [];
  editor.test_outputs <- [];
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      invalidate_from ~editor

let rec refresh_lines_from ~editor =
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      refresh_lines_from ~editor

let is_actual_error (e : Protocol.error) =
  match e.kind with
  | Ocaml_parsing.Location.Report_error
  | Ocaml_parsing.Location.Report_warning_as_error _
  | Ocaml_parsing.Location.Report_alert_as_error _ -> true
  | _ -> false

(* Execute cell directly without Merlin check - used for test cells *)
let run_directly editor =
  if editor.status = Running then ()
  else begin
    editor.status <- Running;
    Editor.clear_messages editor.cm;
    let code_txt = Editor.source editor.cm in
    let line_number = 1 + Editor.get_previous_lines editor.cm in
    Client.eval ~id:editor.id ~line_number ?filename:editor.filename editor.worker code_txt
  end

let rec run editor =
  if editor.status = Running then ()
  else (
    editor.status <- Request_run;
    editor.type_check_done <- false;
    Editor.clear_messages editor.cm;
    El.set_children editor.spec_results [];  (* Clear spec results *)
    editor.tests_pending <- 0;
    editor.spec_passed <- false;
    editor.spec_output <- [];
    editor.test_outputs <- [];
    match editor.prev with
    | Some e when e.status <> Run_ok && e.run_on <> `Never -> run e
    | _ ->
        editor.status <- Running;
        let code_txt = Editor.source editor.cm in
        (* Query Merlin for errors before executing *)
        let open Fut.Syntax in
        let _ =
          let+ errors = Merlin_ext.Client.query_errors editor.merlin_worker code_txt in
          let has_errors = List.exists is_actual_error errors in
          if has_errors then
            (* Don't execute - there are type errors *)
            editor.status <- Not_run
          else begin
            let line_number = 1 + Editor.get_previous_lines editor.cm in
            Client.eval ~id:editor.id ~line_number ?filename:editor.filename editor.worker code_txt
          end
        in
        ())

let set_prev ~prev t =
  let () = match t.prev with None -> () | Some prev -> prev.next <- None in
  t.prev <- prev;
  match prev with
  | None ->
      Editor.set_previous_lines t.cm 0;
      refresh_lines_from ~editor:t
  | Some p ->
      assert (p.next = None);
      p.next <- Some t;
      refresh_lines_from ~editor:p

let set_source_from_html editor this =
  let doc = Webcomponent.text_content this in
  let doc = String.trim doc in
  Editor.set_source editor.cm doc;
  invalidate_from ~editor;
  Client.fmt ~id:editor.id editor.worker doc

let init_css shadow ~extra_style ~inline_style =
  El.append_children shadow
    [
      El.style
        (El.txt (Jstr.of_string [%blob "style.css"])
        ::
        (match inline_style with
        | None -> []
        | Some inline_style ->
            [
              El.txt
              @@ Jstr.of_string (":host{" ^ Jstr.to_string inline_style ^ "}");
            ]));
    ];
  match extra_style with
  | None -> ()
  | Some src_style ->
      El.append_children shadow
        [
          El.link
            ~at:
              [
                At.href src_style;
                At.rel (Jstr.of_string "stylesheet");
                At.type' (Jstr.of_string "text/css");
              ]
            ();
        ]

let init ~id ~run_on ?filename ?extra_style ?inline_style ?(merlin = true)
    ?(highlight = []) ?on_change worker this =
  let shadow = Webcomponent.attach_shadow this in
  init_css shadow ~extra_style ~inline_style;

  (* Add run button container BEFORE creating editor so it appears on top *)
  let run_btn_container =
    match run_on with
    | `Never -> None
    | `Click | `Load ->
        let run_btn = El.button [ El.txt (Jstr.of_string "Run") ] in
        let container = El.div ~at:[ At.class' (Jstr.of_string "run_btn") ] [ run_btn ] in
        El.append_children shadow [ container ];
        Some (run_btn, container)
  in

  let cm = Editor.make shadow in

  (* Create spec results container after the editor *)
  let spec_results = El.div ~at:[ At.class' (Jstr.of_string "spec_results") ] [] in
  El.append_children shadow [ spec_results ];

  let merlin_ext = Merlin_ext.make ~id ?filename worker in
  let merlin_worker = Merlin_ext.Client.make_worker merlin_ext in
  let editor =
    {
      id;
      string_id = None;
      status = Not_run;
      type_check_pending = false;
      type_check_done = false;
      cm;
      prev = None;
      next = None;
      worker;
      merlin_worker;
      run_on;
      filename;
      spec_results;
      tests_pending = 0;
      spec_passed = false;
      spec_output = [];
      test_outputs = [];
    }
  in

  (* Attach click event listener if run button was created *)
  (match run_btn_container with
  | None -> ()
  | Some (run_btn, _) ->
      let _ : Ev.listener =
        Ev.listen Ev.click (fun _ev -> run editor) (El.as_target run_btn)
      in
      ());

  Editor.on_change cm (fun () ->
    invalidate_after ~editor;
    Option.iter (fun f -> f ()) on_change);
  set_source_from_html editor this;

  if merlin then (
    Merlin_ext.set_context merlin_ext (fun () -> pre_source editor);
    Editor.configure_merlin cm (fun () -> Merlin_ext.extensions merlin_worker)
  );

  (* Set highlight specs if provided *)
  if highlight <> [] then Editor.set_highlight_specs cm highlight;

  let () =
    Mutation_observer.observe ~target:(Webcomponent.as_target this)
    @@ Mutation_observer.create (fun _ _ -> set_source_from_html editor this)
  in

  editor

let set_source editor doc =
  Editor.set_source editor.cm doc;
  editor.type_check_done <- false;
  editor.status <- Not_run;
  refresh_lines_from ~editor

let set_highlight editor specs = Editor.set_highlight_specs editor.cm specs

let clear_output editor =
  Editor.clear_messages editor.cm;
  editor.status <- Not_run

let clear_spec_results t =
  El.set_children t.spec_results []

let raw_html s =
  let el = El.div [] in
  let el_t = El.to_jv el in
  Jv.set el_t "innerHTML" (Jv.of_jstr @@ Jstr.of_string s);
  el

let render_output = function
  | X_protocol.Html str -> raw_html str
  | Stdout str -> El.pre ~at:[ At.class' (Jstr.of_string "caml_stdout") ] [ El.txt' str ]
  | Stderr str -> El.pre ~at:[ At.class' (Jstr.of_string "caml_stderr") ] [ El.txt' str ]
  | Meta str -> El.pre ~at:[ At.class' (Jstr.of_string "caml_meta") ] [ El.txt' str ]

(* Check if string contains a substring *)
let contains_substring haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len then false
  else
    let rec check i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

(* Check if spec output indicates success - look for errors *)
let spec_output_passed output =
  not (List.exists (function
    | X_protocol.Html s ->
        let s_lower = String.lowercase_ascii s in
        contains_substring s_lower "color: red" || contains_substring s_lower "color:red"
    | X_protocol.Meta s ->
        (* Check for error messages in Meta output *)
        contains_substring s "Error:" || contains_substring s "Unbound"
    | X_protocol.Stderr s ->
        (* Any stderr output indicates failure *)
        String.length (String.trim s) > 0
    | _ -> false
  ) output)

(* Filter test output to hide Meta val declarations like "val foo : ... = <fun>" *)
let filter_test_output outputs =
  List.filter (function
    | X_protocol.Meta s ->
        (* Filter out "val ... = ..." declarations *)
        let s_trimmed = String.trim s in
        not (String.length s_trimmed >= 4 && String.sub s_trimmed 0 4 = "val ")
    | _ -> true
  ) outputs

(* Render all results in a details element *)
let render_all_results t =
  (* Don't render spec output content - just show pass/fail in the summary *)
  let _spec_output = t.spec_output in (* silence unused field warning *)
  let spec_section = [] in
  let test_sections =
    List.map (fun outputs ->
      let filtered = filter_test_output outputs in
      El.div ~at:[ At.class' (Jstr.of_string "test_section") ]
        (List.map render_output filtered)
    ) (List.rev t.test_outputs)
  in
  (* Determine overall status - check for red color, #cb2431 (test failure color), or exceptions *)
  let output_has_failure outputs =
    List.exists (function
      | X_protocol.Html s ->
          let s_lower = String.lowercase_ascii s in
          contains_substring s_lower "color: red" ||
          contains_substring s_lower "color:red" ||
          contains_substring s_lower "#cb2431"
      | X_protocol.Meta s ->
          (* Check for exception messages in Meta output *)
          contains_substring s "Exception:" ||
          contains_substring s "Error:"
      | _ -> false
    ) outputs
  in
  let all_tests_passed = not (List.exists output_has_failure t.test_outputs) in
  let spec_status = if t.spec_passed then "✓" else "✗" in
  let test_count = List.length t.test_outputs in
  let summary_text =
    if test_count = 0 then
      Printf.sprintf "%s Spec" spec_status
    else
      let test_status = if all_tests_passed then "✓" else "✗" in
      Printf.sprintf "%s Spec, %s Tests" spec_status test_status
  in
  let summary_class =
    if t.spec_passed && all_tests_passed then "results_pass" else "results_fail"
  in
  let details = El.details ~at:[ At.class' (Jstr.of_string summary_class) ] (
    El.summary [ El.txt (Jstr.of_string summary_text) ]
    :: spec_section @ test_sections
  ) in
  El.set_children t.spec_results [ details ]

let set_spec_results t msg =
  El.set_children t.spec_results (List.map render_output msg)

(* Set spec output and determine if passed *)
let set_spec_output t msg ~has_tests =
  t.spec_output <- msg;
  t.spec_passed <- spec_output_passed msg;
  (* Render immediately if no tests, or if spec failed (tests won't run) *)
  if not has_tests || not t.spec_passed then render_all_results t

(* Start tests - set pending count *)
let start_tests t count =
  t.tests_pending <- count

(* Add test output and render when all done *)
let add_test_output t msg =
  t.test_outputs <- msg :: t.test_outputs;
  t.tests_pending <- t.tests_pending - 1;
  if t.tests_pending <= 0 then render_all_results t

let tests_pending t = t.tests_pending
let spec_passed t = t.spec_passed

let render_message msg =
  let kind, text =
    match msg with
    | X_protocol.Stdout str -> ("stdout", El.txt' str)
    | Stderr str -> ("stderr", El.txt' str)
    | Meta str -> ("meta", El.txt' str)
    | Html str -> ("html", raw_html str)
  in
  El.pre ~at:[ At.class' (Jstr.of_string ("caml_" ^ kind)) ] [ text ]

let add_message t loc msg =
  Editor.add_message t.cm loc (List.map render_message msg)

let completed_run ed msg =
  (if msg <> [] then
     let loc = String.length (Editor.source ed.cm) in
     add_message ed loc msg);
  (* If this was a type check response, mark as done and we're finished *)
  if ed.type_check_pending then begin
    ed.type_check_pending <- false;
    ed.type_check_done <- true
  end
  else begin
    ed.status <- Run_ok;
    match ed.next with Some e when e.status = Request_run -> run e | _ -> ()
  end

let completed_type_check ed =
  (* Mark type check as done without adding messages to this cell *)
  ed.type_check_pending <- false;
  ed.type_check_done <- true

let receive_merlin t msg =
  Merlin_ext.Client.on_message t.merlin_worker
    (Merlin_ext.fix_answer ~pre:(pre_source t) ~doc:(Editor.source t.cm) msg)

let loadable t =
  match t.run_on with
  | `Load -> true
  | `Click | `Never -> false
