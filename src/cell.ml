open Brr

type mode = Interactive | Exercise | Test | Hidden

let mode_of_string = function
  | "exercise" -> Exercise
  | "test" -> Test
  | "hidden" -> Hidden
  | "interactive" | _ -> Interactive

type status = Not_run | Running | Run_ok | Request_run

type t = {
  id : int;
  mode : mode;
  mutable prev : t option;
  mutable next : t option;
  mutable status : status;
  cm : Editor.t option; (* none for a hidden cell *)
  mutable hidden_source : string;
  eval_fn : id:int -> line_number:int -> string -> unit;
  fmt_fn : id:int -> string -> unit;
  merlin_worker : Merlin_ext.Client.worker option;
  run_on : [ `Click | `Load ];
  mutable on_completed : t -> unit;
  (* A test cell's verdict, from what its run printed. *)
  test_indicator : El.t option;
  mutable run_stderr : bool;
  mutable run_exception : bool;
}

let id t = t.id
let mode t = t.mode
let has_completed t = t.status = Run_ok
let set_on_completed t f = t.on_completed <- f
let with_cm t f = Option.iter f t.cm
let verdicts = [ "test-pass"; "test-fail"; "test-error" ]

(* Called with None, forgets the verdict: the test has not run since. *)
let set_verdict t v =
  Option.iter
    (fun ind ->
      List.iter
        (fun c ->
          El.set_class (Jstr.of_string c) (Some c = Option.map fst v) ind)
        verdicts;
      El.set_children ind
        (match v with None -> [] | Some (_, txt) -> [ El.txt' txt ]))
    t.test_indicator

let source t =
  match t.cm with Some cm -> Editor.source cm | None -> t.hidden_source

(* The number of the last line shown up to and including this cell. A
   hidden cell's lines are not shown, so it passes on its predecessor's. *)
let rec nb_lines t =
  match (t.cm, t.prev) with
  | Some cm, _ -> Editor.nb_lines cm
  | None, Some p -> nb_lines p
  | None, None -> 0

let previous_lines t =
  match t.cm with Some cm -> Editor.get_previous_lines cm | None -> 0

let pre_source t =
  let rec go acc t =
    match t.prev with
    | None -> String.concat "\n" acc
    | Some e -> go (source e :: acc) e
  in
  let s = go [] t in
  if s = "" then s else s ^ " ;;\n"

let rec invalidate_from ~editor =
  editor.status <- Not_run;
  with_cm editor Editor.clear;
  set_verdict editor None;
  match editor.next with
  | None -> ()
  | Some next ->
      with_cm next (fun cm -> Editor.set_previous_lines cm (nb_lines editor));
      invalidate_from ~editor:next

let invalidate_after ~editor =
  editor.status <- Not_run;
  match editor.next with
  | None -> ()
  | Some next ->
      with_cm next (fun cm -> Editor.set_previous_lines cm (nb_lines editor));
      invalidate_from ~editor:next

let rec refresh_lines_from ~editor =
  match editor.next with
  | None -> ()
  | Some next ->
      with_cm next (fun cm -> Editor.set_previous_lines cm (nb_lines editor));
      refresh_lines_from ~editor:next

let rec run editor =
  if editor.status = Running then ()
  else (
    editor.status <- Request_run;
    editor.run_stderr <- false;
    editor.run_exception <- false;
    with_cm editor Editor.clear_messages;
    match editor.prev with
    | Some e when e.status <> Run_ok -> run e
    | _ ->
        editor.status <- Running;
        let line_number = 1 + previous_lines editor in
        editor.eval_fn ~id:editor.id ~line_number (source editor))

let set_prev ~prev t =
  let () = match t.prev with None -> () | Some prev -> prev.next <- None in
  t.prev <- prev;
  match prev with
  | None ->
      with_cm t (fun cm -> Editor.set_previous_lines cm 0);
      refresh_lines_from ~editor:t
  | Some p ->
      assert (p.next = None);
      p.next <- Some t;
      refresh_lines_from ~editor:p

let set_source_from_html editor this =
  let doc = Webcomponent.text_content this in
  let doc = String.trim doc in
  match editor.cm with
  | Some cm ->
      Editor.set_source cm doc;
      invalidate_from ~editor;
      editor.fmt_fn ~id:editor.id doc
  | None ->
      editor.hidden_source <- doc;
      invalidate_from ~editor

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

(* A test cell's editor sits in a <details>, folded behind a summary that
   shows the verdict; its output stays visible. *)
let test_details shadow =
  let indicator =
    El.span ~at:[ At.class' (Jstr.of_string "test-indicator") ] []
  in
  let details =
    El.v (Jstr.of_string "details")
      ~at:[ At.class' (Jstr.of_string "test-details") ]
      [ El.v (Jstr.of_string "summary") [ El.txt' "Tests "; indicator ] ]
  in
  El.append_children shadow [ details ];
  (details, indicator)

let init ~id ~mode ~run_on ?(merlin = true) ?extra_style ?inline_style ~eval_fn
    ~fmt_fn ~post_fn this =
  let shadow = Webcomponent.attach_shadow this in
  let visible =
    if mode = Hidden then None
    else begin
      init_css shadow ~extra_style ~inline_style;
      let parent, indicator =
        if mode = Test then
          let details, indicator = test_details shadow in
          (details, Some indicator)
        else (shadow, None)
      in
      let run_btn = El.button [ El.txt (Jstr.of_string "Run") ] in
      El.append_children parent
        [ El.div ~at:[ At.class' (Jstr.of_string "run_btn") ] [ run_btn ] ];
      let cm = Editor.make ~read_only:(mode = Test) parent in
      let merlin =
        if merlin then
          let m = Merlin_ext.make ~id post_fn in
          Some (m, Merlin_ext.Client.make_worker m)
        else None
      in
      Some (cm, run_btn, indicator, merlin)
    end
  in
  let editor =
    {
      id;
      mode;
      status = Not_run;
      cm = Option.map (fun (cm, _, _, _) -> cm) visible;
      hidden_source = "";
      prev = None;
      next = None;
      eval_fn;
      fmt_fn;
      merlin_worker = Option.bind visible (fun (_, _, _, m) -> Option.map snd m);
      run_on;
      on_completed = ignore;
      test_indicator = Option.bind visible (fun (_, _, i, _) -> i);
      run_stderr = false;
      run_exception = false;
    }
  in
  Option.iter
    (fun (cm, run_btn, _, merlin) ->
      Editor.on_change cm (fun () -> invalidate_after ~editor);
      Option.iter
        (fun (m, w) ->
          Merlin_ext.set_context m (fun () -> pre_source editor);
          Editor.configure_merlin cm (fun () -> Merlin_ext.extensions w))
        merlin;
      let _ : Ev.listener =
        Ev.listen Ev.click (fun _ev -> run editor) (El.as_target run_btn)
      in
      ())
    visible;
  set_source_from_html editor this;
  let () =
    Mutation_observer.observe ~target:(Webcomponent.as_target this)
    @@ Mutation_observer.create (fun _ _ -> set_source_from_html editor this)
  in
  editor

let set_source editor doc =
  (match editor.cm with
  | Some cm -> Editor.set_source cm doc
  | None -> editor.hidden_source <- doc);
  refresh_lines_from ~editor

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

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

(* The verdict is read from the output. An uncaught exception (the toplevel
   reports those as "Exception: ...") fails a test; anything else on stderr,
   or a compiler error (which the toplevel reports among its own messages),
   is an error. *)
let track_outputs t msgs =
  List.iter
    (function
      | X_protocol.Stderr s ->
          t.run_stderr <- true;
          if contains ~sub:"Exception" s then t.run_exception <- true
      | Meta s when contains ~sub:"Exception" s ->
          t.run_stderr <- true;
          t.run_exception <- true
      | Meta s when contains ~sub:"Error:" s -> t.run_stderr <- true
      | _ -> ())
    msgs

let show_verdict t =
  set_verdict t
    (Some
       (if not t.run_stderr then ("test-pass", "\u{2713}")
        else if t.run_exception then ("test-fail", "\u{2717}")
        else ("test-error", "!")))

let add_message t loc msg =
  track_outputs t msg;
  with_cm t (fun cm -> Editor.add_message cm loc (List.map render_message msg))

let completed_run ed msg =
  (if msg <> [] then
     let loc = String.length (source ed) in
     add_message ed loc msg);
  ed.status <- Run_ok;
  show_verdict ed;
  (match ed.next with Some e when e.status = Request_run -> run e | _ -> ());
  ed.on_completed ed

let receive_merlin t msg =
  Option.iter
    (fun w ->
      Merlin_ext.Client.on_message w
        (Merlin_ext.fix_answer ~pre:(pre_source t) ~doc:(source t) msg))
    t.merlin_worker

let loadable t = t.run_on = `Load
