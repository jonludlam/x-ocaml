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
  group : (group * int) option;  (* its group, and its place in it *)
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
  (* An exercise with a data-id keeps the reader's version (see Saved). *)
  saved_as : string option;
  mutable original : string;
  mutable save_timer : G.timer_id option;
  reset_btn : El.t option;
  run_btn : El.t option;
  mutable stop : unit -> unit;
  (* Runs abandoned while the worker was evaluating them: the worker still
     answers each, in order, and those answers are for code no longer shown. *)
  mutable abandoned : int;
  (* The source last sent to the worker, to be formatted or run. *)
  mutable sent : string;
}

(* Cells with the same group attribute are one program, in page order: a
   module, say, with prose between its parts. The last member runs them all
   together; the others have nothing of their own to send. *)
and group = {
  size : int;  (* the members on the page *)
  mutable members : t list;  (* those registered so far, in page order *)
  mutable first_line : int;  (* the line number its code was last run from *)
}

let make_group ~size = { size; members = []; first_line = 1 }

let leader t =
  match t.group with
  | Some (g, _) when List.length g.members = g.size ->
      Some (List.nth g.members (g.size - 1))
  | _ -> None

let is_leader t =
  match t.group with None -> true | Some (g, i) -> i = g.size - 1

let id t = t.id
let mode t = t.mode
let has_completed t = t.status = Run_ok
let set_on_completed t f = t.on_completed <- f
let set_stop t f = t.stop <- f

(* While its cell runs, the button stops it instead. *)
let set_btn btn ~running =
  El.set_at
    (Jstr.of_string "aria-label")
    (Some (Jstr.of_string (if running then "Stop" else "Run")))
    btn;
  El.set_class (Jstr.of_string "running") running btn

let show_running t running =
  Option.iter (fun btn -> set_btn btn ~running) t.run_btn

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

(* A group's code is its members' joined by newlines: each member's start
   in it, and the whole. *)
let group_code g =
  let starts, _ =
    List.fold_left
      (fun (acc, at) m -> ((m, at) :: acc, at + String.length (source m) + 1))
      ([], 0) g.members
  in
  (List.rev starts, String.concat "\n" (List.map source g.members))

(* The number of the last line shown up to and including this cell. A
   hidden cell's lines are not shown, so it passes on its predecessor's;
   but a hidden member of a group is part of the code the group runs, and
   the line numbers after it count its lines, as the toplevel will. *)
let rec nb_lines t =
  match (t.cm, t.prev) with
  | Some cm, _ -> Editor.nb_lines cm
  | None, prev ->
      let before = match prev with Some p -> nb_lines p | None -> 0 in
      if t.group <> None then before + Editor.count_lines t.hidden_source
      else before

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

(* What Merlin sees around a cell: the cells before it, and for a member of
   a group, the members before and after it, so that it sees the whole
   program the cell is part of. *)
let merlin_context t =
  match t.group with
  | None -> (pre_source t, "")
  | Some (g, i) ->
      let before = List.filteri (fun j _ -> j < i) g.members
      and after = List.filteri (fun j _ -> j > i) g.members in
      let first = match g.members with m :: _ -> m | [] -> t in
      let pre = pre_source first ^ String.concat "" (List.map (fun m -> source m ^ "\n") before) in
      (pre, String.concat "" (List.map (fun m -> "\n" ^ source m) after))

let abandon t = if t.status = Running then t.abandoned <- t.abandoned + 1

let rec invalidate_from ~editor =
  abandon editor;
  editor.status <- Not_run;
  show_running editor false;
  with_cm editor Editor.clear;
  set_verdict editor None;
  match editor.next with
  | None -> ()
  | Some next ->
      with_cm next (fun cm -> Editor.set_previous_lines cm (nb_lines editor));
      invalidate_from ~editor:next

let invalidate_after ~editor =
  abandon editor;
  editor.status <- Not_run;
  show_running editor false;
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
    (* Waiting for the cells before it counts as running: the reader can see
       something is happening, and stop it from here. *)
    show_running editor true;
    editor.run_stderr <- false;
    editor.run_exception <- false;
    with_cm editor Editor.clear_messages;
    match editor.prev with
    | Some e when e.status <> Run_ok -> run e
    | _ when not (is_leader editor) ->
        (* The group's last member runs its code: this one is done. *)
        editor.status <- Run_ok;
        show_running editor false;
        (match editor.next with Some e when e.status = Request_run -> run e | _ -> ())
    | _ -> (
        editor.status <- Running;
        show_running editor true;
        editor.sent <- source editor;
        match editor.group with
        | None ->
            let line_number = 1 + previous_lines editor in
            editor.eval_fn ~id:editor.id ~line_number editor.sent
        | Some (g, _) ->
            List.iter (fun m -> with_cm m Editor.clear_messages) g.members;
            let first = List.hd g.members in
            let line_number = 1 + (match first.prev with Some p -> nb_lines p | None -> 0) in
            g.first_line <- line_number;
            editor.eval_fn ~id:editor.id ~line_number (snd (group_code g))))

(* A member's Run runs the group, from its last member. *)
let run_group editor = run (Option.value ~default:editor (leader editor))

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

let show_reset t =
  Option.iter
    (fun btn ->
      let kept = Option.bind t.saved_as Saved.find <> None in
      El.set_inline_style El.Style.display
        (Jstr.of_string (if kept then "block" else "none"))
        btn)
    t.reset_btn

(* Kept 300ms after the last change; a version back to the original is
   forgotten instead. *)
let save_later t =
  Option.iter
    (fun id ->
      Option.iter G.stop_timer t.save_timer;
      t.save_timer <-
        Some
          (G.set_timeout ~ms:300 (fun () ->
               t.save_timer <- None;
               let src = source t in
               if src = t.original then Saved.forget id else Saved.keep id src;
               show_reset t)))
    t.saved_as

(* Line ranges as attributes give them: "3-20,25,30-31". *)
let parse_ranges = function
  | None -> []
  | Some s ->
      String.split_on_char ',' s
      |> List.filter_map (fun r ->
             match String.split_on_char '-' (String.trim r) with
             | [ a ] -> Option.map (fun a -> (a, a)) (int_of_string_opt a)
             | [ a; b ] -> (
                 match (int_of_string_opt a, int_of_string_opt b) with
                 | Some a, Some b -> Some (a, b)
                 | _ -> None)
             | _ -> None)

(* When the page later replaces the element's text, that is the page
   speaking, so it wins over a version the reader kept, which it forgets. *)
let set_source_from_html ~initial editor this =
  let doc = Webcomponent.text_content this in
  let doc = String.trim doc in
  editor.original <- doc;
  if not initial then Option.iter Saved.forget editor.saved_as;
  match editor.cm with
  | Some cm -> (
      match Option.bind editor.saved_as Saved.find with
      | Some kept ->
          (* The reader's own version, as they left it: not reformatted. *)
          Editor.set_source cm kept;
          invalidate_from ~editor;
          show_reset editor
      | None ->
          Editor.set_source cm doc;
          let attr = Webcomponent.get_attribute this in
          let fold = parse_ranges (attr "fold") and mark = parse_ranges (attr "highlight") in
          Editor.set_layout cm ~fold ~mark;
          invalidate_from ~editor;
          show_reset editor;
          editor.sent <- doc;
          editor.fmt_fn ~id:editor.id doc)
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

(* Beside the run button: an arrow back to the start. *)
let reset_button shadow =
  let btn =
    El.button
      ~at:
        [
          At.class' (Jstr.of_string "reset_exercise_btn");
          At.v (Jstr.of_string "aria-label") (Jstr.of_string "Reset");
          At.title (Jstr.of_string "Back to the original code");
        ]
      [ El.txt' "\u{21BA}" ]
  in
  El.set_inline_style El.Style.display (Jstr.of_string "none") btn;
  El.append_children shadow
    [ El.div ~at:[ At.class' (Jstr.of_string "reset_btn_wrap") ] [ btn ] ];
  btn

(* What a visible cell shows. *)
type ui = {
  ui_cm : Editor.t;
  ui_run_btn : El.t;
  ui_indicator : El.t option;
  ui_merlin : (Merlin_ext.t * Merlin_ext.Client.worker) option;
  ui_reset_btn : El.t option;
}

let init ~id ~mode ~run_on ?(merlin = true) ?cell_id ?extra_style ?inline_style
    ?group ~eval_fn ~fmt_fn ~post_fn this =
  let saved_as = if mode = Exercise then cell_id else None in
  let shadow = Webcomponent.attach_shadow this in
  let ui =
    if mode = Hidden then None
    else begin
      init_css shadow ~extra_style ~inline_style;
      let parent, ui_indicator =
        if mode = Test then
          let details, indicator = test_details shadow in
          (details, Some indicator)
        else (shadow, None)
      in
      let ui_run_btn = El.button [] in
      set_btn ui_run_btn ~running:false;
      El.append_children parent
        [ El.div ~at:[ At.class' (Jstr.of_string "run_btn") ] [ ui_run_btn ] ];
      let ui_cm = Editor.make ~read_only:(mode = Test) parent in
      let ui_merlin =
        if merlin then
          let m = Merlin_ext.make ~id post_fn in
          Some (m, Merlin_ext.Client.make_worker m)
        else None
      in
      let ui_reset_btn = Option.map (fun _ -> reset_button shadow) saved_as in
      Some { ui_cm; ui_run_btn; ui_indicator; ui_merlin; ui_reset_btn }
    end
  in
  let editor =
    {
      id;
      mode;
      group;
      status = Not_run;
      cm = Option.map (fun ui -> ui.ui_cm) ui;
      hidden_source = "";
      prev = None;
      next = None;
      eval_fn;
      fmt_fn;
      merlin_worker = Option.bind ui (fun ui -> Option.map snd ui.ui_merlin);
      run_on;
      on_completed = ignore;
      test_indicator = Option.bind ui (fun ui -> ui.ui_indicator);
      run_stderr = false;
      run_exception = false;
      saved_as;
      original = "";
      save_timer = None;
      reset_btn = Option.bind ui (fun ui -> ui.ui_reset_btn);
      run_btn = Option.map (fun ui -> ui.ui_run_btn) ui;
      stop = ignore;
      abandoned = 0;
      sent = "";
    }
  in
  Option.iter
    (fun ui ->
      let cm = ui.ui_cm in
      Editor.on_change cm (fun () ->
          invalidate_after ~editor;
          save_later editor);
      Option.iter
        (fun btn ->
          let _ : Ev.listener =
            Ev.listen Ev.click
              (fun _ev ->
                Option.iter Saved.forget saved_as;
                Editor.set_source cm editor.original;
                invalidate_after ~editor;
                show_reset editor)
              (El.as_target btn)
          in
          ())
        ui.ui_reset_btn;
      Option.iter
        (fun (m, w) ->
          Merlin_ext.set_context m (fun () -> merlin_context editor);
          Editor.configure_merlin cm (fun () -> Merlin_ext.extensions w))
        ui.ui_merlin;
      let _ : Ev.listener =
        Ev.listen Ev.click
          (fun _ev ->
            match editor.status with
            | Running | Request_run -> editor.stop ()
            | Not_run | Run_ok -> run_group editor)
          (El.as_target ui.ui_run_btn)
      in
      ())
    ui;
  Option.iter (fun (g, _) -> g.members <- g.members @ [ editor ]) group;
  set_source_from_html ~initial:true editor this;
  let () =
    Mutation_observer.observe ~target:(Webcomponent.as_target this)
    @@ Mutation_observer.create (fun _ _ ->
        set_source_from_html ~initial:false editor this)
  in
  editor

(* The formatter's version of what was sent, which is only wanted if the
   reader has not changed the code since. *)
let set_source editor doc =
  if source editor = editor.sent then begin
    (match editor.cm with
    | Some cm -> Editor.set_source cm doc
    | None -> editor.hidden_source <- doc);
    editor.sent <- doc;
    refresh_lines_from ~editor
  end

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

(* The line a compiler message is about: "Line 9, characters 13-16: ...". *)
let message_line msgs =
  List.find_map
    (function
      | X_protocol.Meta s | Stderr s ->
          let s = String.trim s in
          let prefix p = String.length s > String.length p && String.sub s 0 (String.length p) = p in
          let digits from =
            let rec go i = if i < String.length s && s.[i] >= '0' && s.[i] <= '9' then go (i + 1) else i in
            int_of_string_opt (String.sub s from (go from - from))
          in
          if prefix "Lines " then digits 6 else if prefix "Line " then digits 5 else None
      | _ -> None)
    msgs

(* The offset of the end of line [n] (from 1) of [code]. *)
let offset_of_line code n =
  let rec go i line =
    if i >= String.length code then String.length code
    else if code.[i] = '\n' then if line >= n then i else go (i + 1) (line + 1)
    else go (i + 1) line
  in
  go 0 1

let add_message t loc msg =
  if t.abandoned = 0 then begin
    track_outputs t msg;
    (* For a group, [loc] is in the group's code: the message goes to the
       member it is in, or if that one is hidden, the last visible one
       before it, or else the group's last member. *)
    let target, loc =
      match t.group with
      | None -> (t, loc)
      | Some (g, _) -> (
          let starts, _ = group_code g in
          (* The last visible member starting at or before [at]. *)
          let member at =
            let rec find best = function
              | (m, start) :: rest when start <= at ->
                  find (if m.cm <> None then Some (m, start) else best) rest
              | _ -> best
            in
            find None starts
          in
          match (message_line msg, member loc) with
          | Some line, _ -> (
              (* An error names its line: after that line, in its member. *)
              let code = snd (group_code g) in
              let at = offset_of_line code (line - g.first_line + 1) in
              match member at with
              | Some (m, start) -> (m, min (at - start) (String.length (source m)))
              | None -> (t, String.length (source t)))
          | None, Some (m, start) -> (m, min (loc - start) (String.length (source m)))
          | None, None -> (t, String.length (source t)))
    in
    with_cm target (fun cm ->
        Editor.add_message cm loc (List.map render_message msg))
  end

let completed_run ed msg =
  if ed.abandoned > 0 then ed.abandoned <- ed.abandoned - 1
  else begin
    (if msg <> [] then
       let loc =
         match ed.group with
         | Some (g, _) -> String.length (snd (group_code g))
         | None -> String.length (source ed)
       in
       add_message ed loc msg);
    ed.status <- Run_ok;
    show_running ed false;
    show_verdict ed;
    (match ed.next with Some e when e.status = Request_run -> run e | _ -> ());
    ed.on_completed ed
  end

let receive_merlin t msg =
  Option.iter
    (fun w ->
      Merlin_ext.Client.on_message w
        (Merlin_ext.fix_answer ~pre:(fst (merlin_context t)) ~doc:(source t) msg))
    t.merlin_worker

let loadable t = t.run_on = `Load

let reset_status t =
  (* A new worker: nothing is owed any more. *)
  t.abandoned <- 0;
  t.status <- Not_run;
  t.run_stderr <- false;
  t.run_exception <- false;
  with_cm t Editor.clear_messages;
  show_running t false;
  set_verdict t None
