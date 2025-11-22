open Brr

type status = Not_run | Running | Run_ok | Request_run

type t = {
  id : int;
  mutable prev : t option;
  mutable next : t option;
  mutable status : status;
  cm : Editor.t;
  worker : Client.t;
  merlin_worker : Merlin_ext.Client.worker;
  run_on : [ `Click | `Load | `Never ];
  filename : string option;
}

let id t = t.id

let get_source t = Editor.source t.cm

let pre_source t =
  let target_filename = t.filename in
  let rec go acc current =
    match current.prev with
    | None ->
        let result = String.concat "\n" acc in
        Brr.Console.log ["pre_source collected sources:"; result];
        result
    | Some e when e.filename = target_filename ->
        Brr.Console.log ["pre_source: found matching cell with filename";
                         Option.value ~default:"<none>" e.filename;
                         "source:"; Editor.source e.cm];
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
  Editor.clear editor.cm;
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      invalidate_from ~editor

let invalidate_after ~editor =
  editor.status <- Not_run;
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

let rec run editor =
  if editor.status = Running then ()
  else (
    editor.status <- Request_run;
    Editor.clear_messages editor.cm;
    match editor.prev with
    | Some e when e.status <> Run_ok -> run e
    | _ ->
        editor.status <- Running;
        let code_txt = Editor.source editor.cm in
        let line_number = 1 + Editor.get_previous_lines editor.cm in
        Client.eval ~id:editor.id ~line_number ?filename:editor.filename editor.worker code_txt)

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
    ?(highlight = []) worker this =
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

  let merlin_ext = Merlin_ext.make ~id ?filename worker in
  let merlin_worker = Merlin_ext.Client.make_worker merlin_ext in
  let editor =
    {
      id;
      status = Not_run;
      cm;
      prev = None;
      next = None;
      worker;
      merlin_worker;
      run_on;
      filename;
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

  Editor.on_change cm (fun () -> invalidate_after ~editor);
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
  refresh_lines_from ~editor

let set_highlight editor specs = Editor.set_highlight_specs editor.cm specs

let render_message msg =
  let raw_html s =
    let el = El.div [] in
    let el_t = El.to_jv el in
    Jv.set el_t "innerHTML" (Jv.of_jstr @@ Jstr.of_string s);
    el
  in
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
  ed.status <- Run_ok;
  match ed.next with Some e when e.status = Request_run -> run e | _ -> ()

let receive_merlin t msg =
  Merlin_ext.Client.on_message t.merlin_worker
    (Merlin_ext.fix_answer ~pre:(pre_source t) ~doc:(Editor.source t.cm) msg)

let loadable t =
  match t.run_on with
  | `Load -> true
  | `Click | `Never -> false
